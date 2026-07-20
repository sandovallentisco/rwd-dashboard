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
TOTAL_OUTPUT_CSV = ROOT / "citation_impact_total_data.csv"
TOP_ARTICLES_CSV = ROOT / "citation_impact_top200.csv"
REPRODUCIBILITY_CSV = ROOT / "citation_impact_reproducibility.csv"
SUMMARY_CSV = ROOT / "citation_impact_summary.csv"

OPENALEX_URL = "https://api.openalex.org/works"
SAMPLE_SIZE = 8_000
DOI_BATCH_SIZE = 50
MAX_WORKERS = 12
MAX_RETRIES = 5
RANDOM_SEED = 20260712
WINDOW_START = -5
WINDOW_END = 5


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
    # Use only complete calendar years. With a +5 window and data collected in
    # 2026, a 2020 retraction has five complete post-retraction years (2021–2025).
    cutoff_year = date.today().year - (WINDOW_END + 1)
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


def load_article_metadata(records: list[dict]) -> dict[str, dict]:
    """Load display metadata for sampled DOIs from the local RWD extract."""
    target_years = {item["doi"]: int(item["retraction_year"]) for item in records}
    metadata: dict[str, dict] = {}

    with SOURCE_CSV.open("r", encoding="utf-8-sig", newline="") as handle:
        for row in csv.DictReader(handle):
            if "retraction" not in row.get("RetractionNature", "").lower():
                continue
            doi = normalize_doi(row.get("OriginalPaperDOI", ""))
            if doi not in target_years:
                continue
            retraction_year = extract_year(row.get("RetractionDate", ""))
            if retraction_year != target_years[doi]:
                continue

            candidate = {
                "title": (row.get("Title", "") or "").strip(),
                "authors": (row.get("Author", "") or "").strip(),
                "journal": (row.get("Journal", "") or "").strip(),
                "publisher": (row.get("Publisher", "") or "").strip(),
            }
            existing = metadata.get(doi)
            if existing is None:
                metadata[doi] = candidate
            else:
                for field, value in candidate.items():
                    if not existing.get(field) and value:
                        existing[field] = value

    return metadata


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


def percentile(values: list[int], probability: float) -> float:
    """Linear percentile equivalent to R's default quantile type 7."""
    if not values:
        return 0.0
    ordered = sorted(values)
    position = (len(ordered) - 1) * probability
    lower = math.floor(position)
    upper = math.ceil(position)
    if lower == upper:
        return float(ordered[lower])
    fraction = position - lower
    return ordered[lower] + fraction * (ordered[upper] - ordered[lower])


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
                f"publication_year:{retraction_year + WINDOW_START}-{retraction_year + WINDOW_END}"
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


def aggregate(
    records: list[dict],
    metadata: dict,
    article_metadata: dict[str, dict],
    sample_records: list[dict],
) -> None:
    relative_years = range(WINDOW_START, WINDOW_END + 1)
    per_year: dict[int, list[int]] = {relative_year: [] for relative_year in relative_years}
    for record in records:
        publication_year = record.get("publication_year")
        if publication_year is None:
            continue
        publication_year = int(publication_year)
        retraction_year = int(record["retraction_year"])
        counts = {int(year): int(value) for year, value in record.get("counts", {}).items()}
        for relative_year in relative_years:
            if publication_year > retraction_year + relative_year:
                continue
            per_year[relative_year].append(counts.get(relative_year, 0))

    output_rows = []
    for relative_year in relative_years:
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
                "q1_per_paper": round(percentile(values, 0.25), 4) if values else 0,
                "q3_per_paper": round(percentile(values, 0.75), 4) if values else 0,
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
            "q1_per_paper",
            "q3_per_paper",
            "papers_in_denominator",
            "matched_papers",
        ],
    )

    fixed_window_records = []
    for record in records:
        publication_year = record.get("publication_year")
        if publication_year is None:
            continue
        retraction_year = int(record["retraction_year"])
        if int(publication_year) > retraction_year + WINDOW_START:
            continue
        fixed_window_records.append(record)

    cumulative_totals: dict[int, list[int]] = {
        relative_year: [] for relative_year in relative_years
    }
    for record in fixed_window_records:
        counts = {int(year): int(value) for year, value in record.get("counts", {}).items()}
        running_total = 0
        for relative_year in relative_years:
            running_total += counts.get(relative_year, 0)
            cumulative_totals[relative_year].append(running_total)

    total_output_rows = []
    for relative_year in relative_years:
        values = cumulative_totals[relative_year]
        period = (
            "Before retraction"
            if relative_year < 0
            else "After retraction"
            if relative_year > 0
            else "Retraction year"
        )
        total_output_rows.append(
            {
                "relative_year": relative_year,
                "period": period,
                "mean_total_per_paper": round(statistics.mean(values), 4) if values else 0,
                "median_total_per_paper": round(statistics.median(values), 4) if values else 0,
                "q1_total_per_paper": round(percentile(values, 0.25), 4) if values else 0,
                "q3_total_per_paper": round(percentile(values, 0.75), 4) if values else 0,
                "papers_in_denominator": len(values),
            }
        )

    write_rows(
        TOTAL_OUTPUT_CSV,
        total_output_rows,
        [
            "relative_year",
            "period",
            "mean_total_per_paper",
            "median_total_per_paper",
            "q1_total_per_paper",
            "q3_total_per_paper",
            "papers_in_denominator",
        ],
    )

    top_articles = []
    for record in records:
        counts = {int(year): int(value) for year, value in record.get("counts", {}).items()}
        details = article_metadata.get(record["doi"], {})
        top_articles.append(
            {
                "doi": record["doi"],
                "openalex_id": record["openalex_id"],
                "title": details.get("title") or "Title unavailable",
                "authors": details.get("authors") or "Authors unavailable",
                "journal": details.get("journal") or "Journal unavailable",
                "publisher": details.get("publisher") or "Publisher unavailable",
                "publication_year": record.get("publication_year") or "",
                "retraction_year": record["retraction_year"],
                "post_retraction_citations": sum(
                    counts.get(relative_year, 0) for relative_year in range(1, WINDOW_END + 1)
                ),
            }
        )

    top_articles.sort(
        key=lambda item: (-item["post_retraction_citations"], item["doi"])
    )
    top_articles = top_articles[:200]
    for rank, item in enumerate(top_articles, 1):
        item["rank"] = rank
    write_rows(
        TOP_ARTICLES_CSV,
        top_articles,
        [
            "rank",
            "doi",
            "openalex_id",
            "title",
            "authors",
            "journal",
            "publisher",
            "publication_year",
            "retraction_year",
            "post_retraction_citations",
        ],
    )

    with MAP_CSV.open("r", encoding="utf-8", newline="") as handle:
        mapping_by_doi = {row["doi"]: row for row in csv.DictReader(handle)}
    histories_by_doi = {record["doi"]: record for record in records}
    top_rank_by_doi = {item["doi"]: item["rank"] for item in top_articles}

    def relative_suffix(relative_year: int) -> str:
        if relative_year < 0:
            return f"minus_{abs(relative_year)}"
        if relative_year > 0:
            return f"plus_{relative_year}"
        return "zero"

    reproducibility_rows = []
    for sample_row, sampled in enumerate(sample_records, 1):
        doi = sampled["doi"]
        publication_year = (
            int(sampled["publication_year"])
            if sampled.get("publication_year") not in (None, "")
            else None
        )
        retraction_year = int(sampled["retraction_year"])
        mapping = mapping_by_doi.get(doi, {})
        openalex_id = mapping.get("openalex_id", "")
        history = histories_by_doi.get(doi)
        counts = (
            {int(year): int(value) for year, value in history.get("counts", {}).items()}
            if history is not None
            else {}
        )
        details = article_metadata.get(doi, {})
        post_retraction_citations = (
            sum(counts.get(relative_year, 0) for relative_year in range(1, WINDOW_END + 1))
            if history is not None
            else ""
        )
        row = {
            "sample_row": sample_row,
            "doi": doi,
            "doi_url": f"https://doi.org/{doi}",
            "openalex_id": openalex_id,
            "openalex_url": f"https://openalex.org/{openalex_id}" if openalex_id else "",
            "openalex_matched": "TRUE" if openalex_id else "FALSE",
            "citation_history_retrieved": "TRUE" if history is not None else "FALSE",
            "title": details.get("title") or "Title unavailable",
            "authors": details.get("authors") or "Authors unavailable",
            "journal": details.get("journal") or "Journal unavailable",
            "publisher": details.get("publisher") or "Publisher unavailable",
            "publication_year": publication_year or "",
            "retraction_year": retraction_year,
            "retraction_year_stratum": retraction_year,
            "fixed_minus_5_to_plus_5_cohort": (
                "TRUE"
                if history is not None
                and publication_year is not None
                and publication_year <= retraction_year + WINDOW_START
                else "FALSE"
            ),
            "post_retraction_citing_works_years_plus_1_to_plus_5": post_retraction_citations,
            "top_200_post_retraction_rank": top_rank_by_doi.get(doi, ""),
            "sample_random_seed": metadata["random_seed"],
            "retraction_cutoff_year": metadata["retraction_cutoff_year"],
            "openalex_retrieved_date": metadata.get("retrieved_date", date.today().isoformat()),
        }
        for relative_year in relative_years:
            suffix = relative_suffix(relative_year)
            calendar_year = retraction_year + relative_year
            included = (
                history is not None
                and publication_year is not None
                and publication_year <= calendar_year
            )
            row[f"calendar_year_relative_{suffix}"] = calendar_year
            row[f"included_in_denominator_relative_{suffix}"] = "TRUE" if included else "FALSE"
            row[f"citing_works_relative_{suffix}"] = counts.get(relative_year, 0) if included else ""
        reproducibility_rows.append(row)

    reproducibility_fields = [
        "sample_row",
        "doi",
        "doi_url",
        "openalex_id",
        "openalex_url",
        "openalex_matched",
        "citation_history_retrieved",
        "title",
        "authors",
        "journal",
        "publisher",
        "publication_year",
        "retraction_year",
        "retraction_year_stratum",
        "fixed_minus_5_to_plus_5_cohort",
        "post_retraction_citing_works_years_plus_1_to_plus_5",
        "top_200_post_retraction_rank",
        "sample_random_seed",
        "retraction_cutoff_year",
        "openalex_retrieved_date",
    ]
    for relative_year in relative_years:
        suffix = relative_suffix(relative_year)
        reproducibility_fields.extend(
            [
                f"calendar_year_relative_{suffix}",
                f"included_in_denominator_relative_{suffix}",
                f"citing_works_relative_{suffix}",
            ]
        )
    write_rows(REPRODUCIBILITY_CSV, reproducibility_rows, reproducibility_fields)

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
        "analysis_window_start": WINDOW_START,
        "analysis_window_end": WINDOW_END,
        "post_retraction_ranking_start": 1,
        "post_retraction_ranking_end": WINDOW_END,
        "fixed_window_records": len(fixed_window_records),
        "retrieved_date": metadata.get("retrieved_date", date.today().isoformat()),
        "source": metadata.get("source", "OpenAlex"),
    }
    write_rows(SUMMARY_CSV, [{"metric": key, "value": value} for key, value in summary.items()], ["metric", "value"])


def load_existing_histories() -> list[dict]:
    if not MAP_CSV.exists() or not PROGRESS_JSONL.exists():
        raise RuntimeError("Existing OpenAlex mapping and citation histories are required")

    with MAP_CSV.open("r", encoding="utf-8", newline="") as handle:
        mapped = [row for row in csv.DictReader(handle) if row.get("openalex_id")]
    progress = load_progress()

    histories = []
    for row in mapped:
        history = progress.get(row["openalex_id"])
        if history is None:
            continue
        item = dict(history)
        item["doi"] = row["doi"]
        item["retraction_year"] = int(row["retraction_year"])
        item["publication_year"] = (
            int(row["publication_year"]) if row.get("publication_year") else None
        )
        histories.append(item)
    return histories


def load_existing_sample() -> list[dict]:
    if not SAMPLE_CSV.exists():
        raise RuntimeError("Existing citation sample is required")
    with SAMPLE_CSV.open("r", encoding="utf-8", newline="") as handle:
        return list(csv.DictReader(handle))


def load_existing_metadata() -> dict:
    if not SUMMARY_CSV.exists():
        raise RuntimeError("Existing citation summary is required")
    with SUMMARY_CSV.open("r", encoding="utf-8", newline="") as handle:
        summary = {row["metric"]: row["value"] for row in csv.DictReader(handle)}

    integer_metrics = (
        "eligible_unique_doi",
        "sampled_records",
        "matched_openalex",
        "retraction_cutoff_year",
        "failed_requests",
        "random_seed",
    )
    metadata = {metric: int(float(summary[metric])) for metric in integer_metrics}
    metadata["retrieved_date"] = summary["retrieved_date"]
    metadata["source"] = summary["source"]
    return metadata


def main() -> None:
    if "--aggregate-only" in sys.argv:
        histories = load_existing_histories()
        sample = load_existing_sample()
        aggregate(
            histories,
            load_existing_metadata(),
            load_article_metadata(sample),
            sample,
        )
        print(f"Re-aggregated existing citation histories: {len(histories)}", flush=True)
        return

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
        load_article_metadata(sample),
        sample,
    )
    print(f"Completed citation histories: {len(histories)}", flush=True)
    print(f"Failed citation histories: {len(failures)}", flush=True)


if __name__ == "__main__":
    try:
        main()
    except Exception as error:
        print(f"Citation analysis failed: {error}", file=sys.stderr)
        raise
