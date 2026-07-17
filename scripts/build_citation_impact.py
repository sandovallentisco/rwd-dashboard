from __future__ import annotations

import csv
import json
import math
import os
import random
import re
import statistics
import sys
import threading
import time
from collections import defaultdict
from concurrent.futures import ThreadPoolExecutor, as_completed
from datetime import date
from pathlib import Path
from urllib.error import HTTPError, URLError
from urllib.parse import urlencode
from urllib.request import Request, urlopen


ROOT = Path(__file__).resolve().parents[1]
SOURCE_CSV = ROOT / "retraction_watch.csv"
SAMPLE_CSV = ROOT / "citation_impact_sample.csv"
MAP_CSV = ROOT / "citation_impact_openalex_map.csv"
PROGRESS_JSONL = ROOT / "citation_impact_progress.jsonl"
FAILURES_JSONL = ROOT / "citation_impact_failures.jsonl"
OUTPUT_CSV = ROOT / "citation_impact_data.csv"
SUMMARY_CSV = ROOT / "citation_impact_summary.csv"

OPENALEX_URL = "https://api.openalex.org/works"
SAMPLE_SIZE = 8_000
DOI_BATCH_SIZE = 50
MAX_WORKERS = 12
MAX_RETRIES = 5
RANDOM_SEED = 20260712


def load_api_key() -> str:
    key = os.environ.get("OPENALEX_API_KEY", "").strip()
    if key:
        return key

    environ_path = ROOT / ".Renviron"
    if environ_path.exists():
        for line in environ_path.read_text(encoding="utf-8").splitlines():
            if line.startswith("OPENALEX_API_KEY="):
                return line.split("=", 1)[1].strip().strip('"').strip("'")

    raise RuntimeError("OPENALEX_API_KEY is not configured")


def normalize_doi(value: str) -> str | None:
    doi = (value or "").strip().lower()
    prefixes = (
        "https://doi.org/",
        "http://doi.org/",
        "https://dx.doi.org/",
        "http://dx.doi.org/",
        "doi:",
    )
    for prefix in prefixes:
        if doi.startswith(prefix):
            doi = doi[len(prefix) :]
            break
    doi = doi.strip().rstrip(".,;)")
    return doi if doi.startswith("10.") and "/" in doi else None


def extract_year(value: str) -> int | None:
    match = re.search(r"\b(?:19|20)\d{2}\b", value or "")
    return int(match.group(0)) if match else None


def load_eligible_records() -> tuple[list[dict], int]:
    # Use only complete calendar years: a 2015 retraction has ten complete
    # post-retraction years through 2025 when this script runs in 2026.
    cutoff_year = date.today().year - 11
    by_doi: dict[str, dict] = {}

    with SOURCE_CSV.open("r", encoding="utf-8-sig", newline="") as handle:
        for row in csv.DictReader(handle):
            nature = row.get("RetractionNature", "")
            if "retraction" not in nature.lower():
                continue

            retraction_year = extract_year(row.get("RetractionDate", ""))
            publication_year = extract_year(row.get("OriginalPaperDate", ""))
            doi = normalize_doi(row.get("OriginalPaperDOI", ""))
            if not doi or retraction_year is None or retraction_year > cutoff_year:
                continue

            existing = by_doi.get(doi)
            record = {
                "doi": doi,
                "retraction_year": retraction_year,
                "publication_year": publication_year,
            }
            if existing is None:
                by_doi[doi] = record
            elif retraction_year < existing["retraction_year"]:
                if record["publication_year"] is None:
                    record["publication_year"] = existing["publication_year"]
                by_doi[doi] = record
            elif existing["publication_year"] is None and publication_year is not None:
                existing["publication_year"] = publication_year

    return list(by_doi.values()), cutoff_year


def stratified_sample(records: list[dict], sample_size: int) -> list[dict]:
    if len(records) <= sample_size:
        return sorted(records, key=lambda item: (item["retraction_year"], item["doi"]))

    groups: dict[int, list[dict]] = defaultdict(list)
    for record in records:
        groups[record["retraction_year"]].append(record)

    total = len(records)
    exact = {year: len(items) * sample_size / total for year, items in groups.items()}
    quotas = {year: min(len(groups[year]), max(1, math.floor(value))) for year, value in exact.items()}

    while sum(quotas.values()) > sample_size:
        candidates = [year for year in quotas if quotas[year] > 1]
        year = max(candidates, key=lambda candidate: quotas[candidate] - exact[candidate])
        quotas[year] -= 1

    remainder_order = sorted(
        groups,
        key=lambda year: (exact[year] - math.floor(exact[year]), len(groups[year])),
        reverse=True,
    )
    while sum(quotas.values()) < sample_size:
        changed = False
        for year in remainder_order:
            if quotas[year] < len(groups[year]):
                quotas[year] += 1
                changed = True
                if sum(quotas.values()) == sample_size:
                    break
        if not changed:
            break

    rng = random.Random(RANDOM_SEED)
    sampled: list[dict] = []
    for year in sorted(groups):
        sampled.extend(rng.sample(groups[year], quotas[year]))
    return sorted(sampled, key=lambda item: (item["retraction_year"], item["doi"]))


def write_rows(path: Path, rows: list[dict], fieldnames: list[str]) -> None:
    with path.open("w", encoding="utf-8", newline="") as handle:
        writer = csv.DictWriter(handle, fieldnames=fieldnames)
        writer.writeheader()
        writer.writerows(rows)


def api_get(params: dict[str, str], api_key: str) -> dict:
    query = dict(params)
    query["api_key"] = api_key
    request = Request(
        f"{OPENALEX_URL}?{urlencode(query)}",
        headers={"User-Agent": "RWD-Dashboard-Citation-Analysis/1.0"},
    )

    for attempt in range(MAX_RETRIES):
        try:
            with urlopen(request, timeout=45) as response:
                return json.loads(response.read().decode("utf-8"))
        except HTTPError as error:
            if error.code not in (429, 500, 502, 503, 504) or attempt == MAX_RETRIES - 1:
                raise RuntimeError(f"OpenAlex HTTP {error.code}") from None
            retry_after = error.headers.get("Retry-After")
            delay = float(retry_after) if retry_after else min(30, 2**attempt)
        except (URLError, TimeoutError, json.JSONDecodeError) as error:
            if attempt == MAX_RETRIES - 1:
                raise RuntimeError(f"OpenAlex request failed: {type(error).__name__}") from None
            delay = min(30, 2**attempt)

        time.sleep(delay + random.random() * 0.25)

    raise RuntimeError("OpenAlex request failed")


def chunks(items: list, size: int):
    for index in range(0, len(items), size):
        yield items[index : index + size]


def map_doi_batch(batch: list[dict], api_key: str) -> list[dict]:
    doi_filter = "|".join(item["doi"] for item in batch)
    payload = api_get(
        {
            "filter": f"doi:{doi_filter}",
            "select": "id,doi",
            "per_page": "100",
        },
        api_key,
    )

    retraction_years = {item["doi"]: item["retraction_year"] for item in batch}
    mapped = []
    for result in payload.get("results", []):
        doi = normalize_doi(result.get("doi", ""))
        openalex_id = (result.get("id") or "").rsplit("/", 1)[-1]
        if doi in retraction_years and openalex_id.startswith("W"):
            mapped.append(
                {
                    "doi": doi,
                    "retraction_year": retraction_years[doi],
                    "openalex_id": openalex_id,
                }
            )
    return mapped


def load_or_create_mapping(sample: list[dict], api_key: str) -> list[dict]:
    sample_dois = {item["doi"] for item in sample}
    if MAP_CSV.exists():
        with MAP_CSV.open("r", encoding="utf-8", newline="") as handle:
            existing = list(csv.DictReader(handle))
        existing_dois = {row["doi"] for row in existing}
        if sample_dois.issubset(existing_dois):
            existing_by_doi = {row["doi"]: row for row in existing}
            all_rows = [
                {
                    "doi": item["doi"],
                    "retraction_year": item["retraction_year"],
                    "publication_year": item["publication_year"],
                    "openalex_id": existing_by_doi[item["doi"]]["openalex_id"],
                }
                for item in sample
            ]
            write_rows(
                MAP_CSV,
                all_rows,
                ["doi", "retraction_year", "publication_year", "openalex_id"],
            )
            return [row for row in all_rows if row["openalex_id"]]

    mapped: list[dict] = []
    batches = list(chunks(sample, DOI_BATCH_SIZE))
    with ThreadPoolExecutor(max_workers=8) as executor:
        futures = {executor.submit(map_doi_batch, batch, api_key): index for index, batch in enumerate(batches, 1)}
        for completed, future in enumerate(as_completed(futures), 1):
            mapped.extend(future.result())
            if completed % 20 == 0 or completed == len(batches):
                print(f"DOI mapping: {completed}/{len(batches)} batches", flush=True)

    mapped_by_doi = {row["doi"]: row for row in mapped}
    all_rows = []
    for item in sample:
        mapped_item = mapped_by_doi.get(item["doi"])
        all_rows.append(
            {
                "doi": item["doi"],
                "retraction_year": item["retraction_year"],
                "publication_year": item["publication_year"],
                "openalex_id": mapped_item["openalex_id"] if mapped_item else "",
            }
        )
    write_rows(
        MAP_CSV,
        all_rows,
        ["doi", "retraction_year", "publication_year", "openalex_id"],
    )
    return [row for row in all_rows if row["openalex_id"]]


def citation_history(record: dict, api_key: str) -> dict:
    retraction_year = int(record["retraction_year"])
    payload = api_get(
        {
            "filter": (
                f"cites:{record['openalex_id']},"
                f"publication_year:{retraction_year - 10}-{retraction_year + 10}"
            ),
            "group_by": "publication_year",
            "per_page": "100",
        },
        api_key,
    )
    counts = {
        str(int(group["key"]) - retraction_year): int(group["count"])
        for group in payload.get("group_by", [])
        if str(group.get("key", "")).lstrip("-").isdigit()
    }
    return {
        "doi": record["doi"],
        "openalex_id": record["openalex_id"],
        "retraction_year": retraction_year,
        "publication_year": record["publication_year"],
        "counts": counts,
    }


def load_progress() -> dict[str, dict]:
    progress: dict[str, dict] = {}
    if not PROGRESS_JSONL.exists():
        return progress
    with PROGRESS_JSONL.open("r", encoding="utf-8") as handle:
        for line in handle:
            try:
                item = json.loads(line)
                progress[item["openalex_id"]] = item
            except (json.JSONDecodeError, KeyError):
                continue
    return progress


def collect_citation_histories(mapped: list[dict], api_key: str) -> tuple[list[dict], list[dict]]:
    progress = load_progress()
    pending = [row for row in mapped if row["openalex_id"] not in progress]
    failures: list[dict] = []
    lock = threading.Lock()

    print(f"Citation histories already cached: {len(progress)}", flush=True)
    print(f"Citation histories pending: {len(pending)}", flush=True)

    with PROGRESS_JSONL.open("a", encoding="utf-8") as progress_handle:
        with ThreadPoolExecutor(max_workers=MAX_WORKERS) as executor:
            futures = {executor.submit(citation_history, row, api_key): row for row in pending}
            for completed, future in enumerate(as_completed(futures), 1):
                row = futures[future]
                try:
                    item = future.result()
                    progress[item["openalex_id"]] = item
                    with lock:
                        progress_handle.write(json.dumps(item, separators=(",", ":")) + "\n")
                        progress_handle.flush()
                except Exception as error:
                    failures.append(
                        {
                            "doi": row["doi"],
                            "openalex_id": row["openalex_id"],
                            "error": str(error),
                        }
                    )

                if completed % 250 == 0 or completed == len(pending):
                    print(f"Citation histories: {completed}/{len(pending)} new requests", flush=True)

    if failures:
        with FAILURES_JSONL.open("w", encoding="utf-8") as handle:
            for failure in failures:
                handle.write(json.dumps(failure, separators=(",", ":")) + "\n")
    elif FAILURES_JSONL.exists():
        FAILURES_JSONL.unlink()

    completed = []
    for row in mapped:
        if row["openalex_id"] not in progress:
            continue
        history = dict(progress[row["openalex_id"]])
        history["doi"] = row["doi"]
        history["retraction_year"] = row["retraction_year"]
        history["publication_year"] = row["publication_year"]
        completed.append(history)
    return completed, failures


def aggregate(records: list[dict], metadata: dict) -> None:
    per_year: dict[int, list[int]] = {relative_year: [] for relative_year in range(-10, 11)}
    for record in records:
        publication_year = record.get("publication_year")
        if publication_year is None:
            continue
        publication_year = int(publication_year)
        retraction_year = int(record["retraction_year"])
        counts = {int(year): int(value) for year, value in record.get("counts", {}).items()}
        for relative_year in range(-10, 11):
            if publication_year > retraction_year + relative_year:
                continue
            per_year[relative_year].append(counts.get(relative_year, 0))

    output_rows = []
    for relative_year in range(-10, 11):
        values = per_year[relative_year]
        total = sum(values)
        period = "Before retraction" if relative_year < 0 else "After retraction" if relative_year > 0 else "Retraction year"
        output_rows.append(
            {
                "relative_year": relative_year,
                "period": period,
                "citing_works": total,
                "mean_per_paper": round(total / len(values), 4) if values else 0,
                "median_per_paper": round(statistics.median(values), 4) if values else 0,
                "papers_in_denominator": len(values),
                "matched_papers": len(records),
            }
        )

    write_rows(
        OUTPUT_CSV,
        output_rows,
        [
            "relative_year",
            "period",
            "citing_works",
            "mean_per_paper",
            "median_per_paper",
            "papers_in_denominator",
            "matched_papers",
        ],
    )

    pre_total = sum(row["citing_works"] for row in output_rows if row["relative_year"] < 0)
    year_zero_total = next(row["citing_works"] for row in output_rows if row["relative_year"] == 0)
    post_total = sum(row["citing_works"] for row in output_rows if row["relative_year"] > 0)
    completed_count = len(records)
    pre_annual_mean = statistics.mean(
        row["mean_per_paper"] for row in output_rows if row["relative_year"] < 0
    )
    post_annual_mean = statistics.mean(
        row["mean_per_paper"] for row in output_rows if row["relative_year"] > 0
    )

    summary = {
        **metadata,
        "completed_records": completed_count,
        "coverage_pct": round(100 * completed_count / metadata["sampled_records"], 2),
        "pre_citing_works": pre_total,
        "retraction_year_citing_works": year_zero_total,
        "post_citing_works": post_total,
        "pre_mean_annual_per_paper": round(pre_annual_mean, 4),
        "post_mean_annual_per_paper": round(post_annual_mean, 4),
        "retrieved_date": date.today().isoformat(),
        "source": "OpenAlex",
    }
    write_rows(SUMMARY_CSV, [{"metric": key, "value": value} for key, value in summary.items()], ["metric", "value"])


def main() -> None:
    api_key = load_api_key()
    eligible, cutoff_year = load_eligible_records()
    sample = stratified_sample(eligible, SAMPLE_SIZE)
    write_rows(SAMPLE_CSV, sample, ["doi", "retraction_year", "publication_year"])

    print(f"Eligible unique DOIs through {cutoff_year}: {len(eligible)}", flush=True)
    print(f"Stratified sample: {len(sample)}", flush=True)

    mapped = load_or_create_mapping(sample, api_key)
    print(f"Matched to OpenAlex: {len(mapped)}", flush=True)

    histories, failures = collect_citation_histories(mapped, api_key)
    aggregate(
        histories,
        {
            "eligible_unique_doi": len(eligible),
            "sampled_records": len(sample),
            "matched_openalex": len(mapped),
            "retraction_cutoff_year": cutoff_year,
            "failed_requests": len(failures),
            "random_seed": RANDOM_SEED,
        },
    )
    print(f"Completed citation histories: {len(histories)}", flush=True)
    print(f"Failed citation histories: {len(failures)}", flush=True)


if __name__ == "__main__":
    try:
        main()
    except Exception as error:
        print(f"Citation analysis failed: {error}", file=sys.stderr)
        raise
