"""Resolve high-ranking RWD author-name strings to OpenAlex author identities.

The pipeline is intentionally conservative:
- only records classified as retractions are considered;
- papers are deduplicated by normalized DOI, with RWD Record ID as fallback;
- only authorships on the DOI-matched OpenAlex work can be selected;
- exact normalized-name matches are preferred;
- initial-compatible matches are retained but flagged for review;
- ambiguous matches are never assigned automatically.
"""

from __future__ import annotations

import csv
import json
import os
import random
import re
import statistics
import time
import unicodedata
from collections import Counter, defaultdict
from concurrent.futures import ThreadPoolExecutor, as_completed
from datetime import date
from pathlib import Path
from urllib.error import HTTPError, URLError
from urllib.parse import urlencode
from urllib.request import Request, urlopen


ROOT = Path(__file__).resolve().parents[1]
SOURCE_CSV = ROOT / "retraction_watch.csv"
WORK_CACHE_JSONL = ROOT / "author_identity_work_cache.jsonl"
AUTHOR_CACHE_JSONL = ROOT / "author_identity_author_cache.jsonl"
LEADERBOARD_CSV = ROOT / "author_identity_raw_leaderboard.csv"
PAPERS_CSV = ROOT / "author_identity_raw_papers.csv"
REASONS_CSV = ROOT / "author_identity_raw_reasons.csv"
PUBLISHERS_CSV = ROOT / "author_identity_raw_publishers.csv"
UNRESOLVED_CSV = ROOT / "author_identity_raw_unresolved.csv"
SUMMARY_CSV = ROOT / "author_identity_raw_summary.csv"

WORKS_URL = "https://api.openalex.org/works"
AUTHORS_URL = "https://api.openalex.org/authors"
RAW_NAME_CANDIDATE_LIMIT = 150
WORK_BATCH_SIZE = 25
AUTHOR_BATCH_SIZE = 50
MAX_WORKERS = 8
MAX_RETRIES = 5

EXCLUDED_AUTHOR_NAMES = {"unknown", "anonymous", "n/a", "not available"}
EXCLUDED_REASON_PATTERN = re.compile(
    r"investigation by journal/publisher|investigation by third party",
    re.IGNORECASE,
)


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
    for prefix in (
        "https://doi.org/",
        "http://doi.org/",
        "https://dx.doi.org/",
        "http://dx.doi.org/",
        "doi:",
    ):
        if doi.startswith(prefix):
            doi = doi[len(prefix) :]
            break
    doi = doi.strip().rstrip(".,;)")
    return doi if doi.startswith("10.") and "/" in doi else None


def extract_year(value: str) -> int | None:
    match = re.search(r"\b(?:19|20)\d{2}\b", value or "")
    return int(match.group(0)) if match else None


def split_values(value: str) -> set[str]:
    return {item.strip().lstrip("+").strip() for item in (value or "").split(";") if item.strip()}


def normalize_name(value: str) -> str:
    text = (value or "").strip()
    if "," in text:
        parts = [part.strip() for part in text.split(",") if part.strip()]
        if len(parts) == 2:
            text = f"{parts[1]} {parts[0]}"
    text = unicodedata.normalize("NFKD", text).encode("ascii", "ignore").decode("ascii")
    text = re.sub(r"[^a-zA-Z0-9]+", " ", text).lower()
    return " ".join(text.split())


def initial_compatible(source: str, candidate: str) -> bool:
    source_tokens = normalize_name(source).split()
    candidate_tokens = normalize_name(candidate).split()
    if len(source_tokens) < 2 or len(candidate_tokens) < 2:
        return False
    if source_tokens[-1] != candidate_tokens[-1]:
        return False

    source_given = source_tokens[:-1]
    candidate_given = candidate_tokens[:-1]
    source_initials = "".join(token[0] for token in source_given if token)
    candidate_initials = "".join(token[0] for token in candidate_given if token)
    if not source_initials or not candidate_initials:
        return False
    if not (source_initials.startswith(candidate_initials) or candidate_initials.startswith(source_initials)):
        return False

    first_source = source_given[0]
    first_candidate = candidate_given[0]
    return (
        first_source == first_candidate
        or first_source[0] == first_candidate[0]
        and (len(first_source) == 1 or len(first_candidate) == 1)
    )


def api_get(url: str, params: dict[str, str], api_key: str) -> dict:
    query = dict(params)
    query["api_key"] = api_key
    request = Request(
        f"{url}?{urlencode(query)}",
        headers={"User-Agent": "RWD-Dashboard-Author-Resolution/1.0"},
    )

    for attempt in range(MAX_RETRIES):
        try:
            with urlopen(request, timeout=60) as response:
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


def write_rows(path: Path, rows: list[dict], fieldnames: list[str]) -> None:
    with path.open("w", encoding="utf-8", newline="") as handle:
        writer = csv.DictWriter(handle, fieldnames=fieldnames)
        writer.writeheader()
        writer.writerows(rows)


def load_jsonl_cache(path: Path, key: str) -> dict[str, dict]:
    cache: dict[str, dict] = {}
    if not path.exists():
        return cache
    with path.open("r", encoding="utf-8") as handle:
        for line in handle:
            try:
                record = json.loads(line)
            except json.JSONDecodeError:
                continue
            if record.get(key):
                cache[record[key]] = record
    return cache


def append_jsonl(path: Path, records: list[dict]) -> None:
    if not records:
        return
    with path.open("a", encoding="utf-8") as handle:
        for record in records:
            handle.write(json.dumps(record, ensure_ascii=False, separators=(",", ":")) + "\n")


def load_candidate_records() -> tuple[list[dict], list[str]]:
    records_by_pair: dict[tuple[str, str], dict] = {}
    unique_papers_by_name: dict[str, set[str]] = defaultdict(set)
    retraction_rows_by_name: Counter[str] = Counter()

    with SOURCE_CSV.open("r", encoding="utf-8-sig", newline="") as handle:
        for row_index, row in enumerate(csv.DictReader(handle), 1):
            if "retraction" not in (row.get("RetractionNature") or "").lower():
                continue

            doi = normalize_doi(row.get("OriginalPaperDOI", ""))
            record_id = (row.get("Record ID") or str(row_index)).strip()
            paper_key = f"doi:{doi}" if doi else f"record:{record_id}"
            publication_year = extract_year(row.get("OriginalPaperDate", ""))
            retraction_year = extract_year(row.get("RetractionDate", ""))
            reasons = split_values(row.get("Reason", ""))
            publishers = split_values(row.get("Publisher", ""))

            authors = {name.strip() for name in (row.get("Author") or "").split(";") if name.strip()}
            for author in authors:
                if normalize_name(author) in EXCLUDED_AUTHOR_NAMES:
                    continue
                unique_papers_by_name[author].add(paper_key)
                retraction_rows_by_name[author] += 1
                pair = (author, paper_key)
                existing = records_by_pair.get(pair)
                if existing is None:
                    records_by_pair[pair] = {
                        "source_author": author,
                        "paper_key": paper_key,
                        "doi": doi or "",
                        "publication_year": publication_year,
                        "retraction_year": retraction_year,
                        "reasons": set(reasons),
                        "publishers": set(publishers),
                    }
                else:
                    existing["reasons"].update(reasons)
                    existing["publishers"].update(publishers)
                    if existing["publication_year"] is None and publication_year is not None:
                        existing["publication_year"] = publication_year
                    if retraction_year is not None and (
                        existing["retraction_year"] is None or retraction_year < existing["retraction_year"]
                    ):
                        existing["retraction_year"] = retraction_year

    ranked_names = sorted(
        unique_papers_by_name,
        key=lambda name: (-len(unique_papers_by_name[name]), -retraction_rows_by_name[name], name),
    )[:RAW_NAME_CANDIDATE_LIMIT]
    selected = set(ranked_names)
    records = [record for (name, _), record in records_by_pair.items() if name in selected]
    return records, ranked_names


def fetch_work_batch(batch: list[str], api_key: str) -> list[dict]:
    payload = api_get(
        WORKS_URL,
        {
            "filter": f"doi:{'|'.join(batch)}",
            "select": "id,doi,authorships",
            "per_page": "100",
        },
        api_key,
    )
    found: dict[str, dict] = {}
    for result in payload.get("results", []):
        doi = normalize_doi(result.get("doi", ""))
        if doi:
            found[doi] = {
                "id": (result.get("id") or "").rsplit("/", 1)[-1],
                "authorships": result.get("authorships") or [],
            }
    return [{"doi": doi, "work": found.get(doi)} for doi in batch]


def get_works(dois: list[str], api_key: str) -> dict[str, dict]:
    cache = load_jsonl_cache(WORK_CACHE_JSONL, "doi")
    pending = [doi for doi in dois if doi not in cache]
    batches = list(chunks(pending, WORK_BATCH_SIZE))
    print(f"OpenAlex work cache: {len(dois) - len(pending)}/{len(dois)} DOI records", flush=True)

    if batches:
        with ThreadPoolExecutor(max_workers=MAX_WORKERS) as executor:
            futures = {executor.submit(fetch_work_batch, batch, api_key): index for index, batch in enumerate(batches, 1)}
            for completed, future in enumerate(as_completed(futures), 1):
                rows = future.result()
                append_jsonl(WORK_CACHE_JSONL, rows)
                cache.update({row["doi"]: row for row in rows})
                if completed % 20 == 0 or completed == len(batches):
                    print(f"OpenAlex work batches: {completed}/{len(batches)}", flush=True)
    return cache


def authorship_names(authorship: dict) -> set[str]:
    author = authorship.get("author") or {}
    return {
        name
        for name in (
            authorship.get("raw_author_name"),
            author.get("display_name"),
        )
        if name
    }


def match_authorship(source_name: str, authorships: list[dict]) -> tuple[dict | None, str]:
    normalized_source = normalize_name(source_name)
    exact = []
    for authorship in authorships:
        author_id = ((authorship.get("author") or {}).get("id") or "").rsplit("/", 1)[-1]
        if not author_id.startswith("A"):
            continue
        if any(normalize_name(name) == normalized_source for name in authorship_names(authorship)):
            exact.append(authorship)
    exact_ids = {((item.get("author") or {}).get("id") or "").rsplit("/", 1)[-1] for item in exact}
    if len(exact_ids) == 1:
        return exact[0], "Exact"
    if len(exact_ids) > 1:
        return None, "Ambiguous"

    initial_matches = []
    for authorship in authorships:
        author_id = ((authorship.get("author") or {}).get("id") or "").rsplit("/", 1)[-1]
        if not author_id.startswith("A"):
            continue
        if any(initial_compatible(source_name, name) for name in authorship_names(authorship)):
            initial_matches.append(authorship)
    initial_ids = {
        ((item.get("author") or {}).get("id") or "").rsplit("/", 1)[-1]
        for item in initial_matches
    }
    if len(initial_ids) == 1:
        return initial_matches[0], "Initial"
    if len(initial_ids) > 1:
        return None, "Ambiguous"
    return None, "Unmatched"


def resolve_records(records: list[dict], work_cache: dict[str, dict]) -> tuple[list[dict], list[dict]]:
    resolved: list[dict] = []
    statuses: list[dict] = []
    for record in records:
        status = "No DOI"
        authorship = None
        match_type = ""
        work = None
        if record["doi"]:
            cached = work_cache.get(record["doi"])
            work = cached.get("work") if cached else None
            if work:
                authorship, match_type = match_authorship(record["source_author"], work.get("authorships") or [])
                status = "Resolved" if authorship else match_type
            else:
                status = "Work not found"

        statuses.append({"source_author": record["source_author"], "paper_key": record["paper_key"], "status": status})
        if not authorship:
            continue

        author = authorship.get("author") or {}
        author_id = (author.get("id") or "").rsplit("/", 1)[-1]
        if not author_id.startswith("A"):
            continue
        lag = None
        if record["publication_year"] is not None and record["retraction_year"] is not None:
            lag = record["retraction_year"] - record["publication_year"]
        resolved.append(
            {
                **record,
                "openalex_work_id": (work.get("id") or ""),
                "openalex_author_id": author_id,
                "authorship_display_name": author.get("display_name") or record["source_author"],
                "authorship_orcid": author.get("orcid") or "",
                "match_type": match_type,
                "retraction_lag_years": lag,
            }
        )
    return resolved, statuses


def fetch_author_batch(batch: list[str], api_key: str) -> list[dict]:
    payload = api_get(
        AUTHORS_URL,
        {
            "filter": f"openalex_id:{'|'.join(batch)}",
            "select": "id,display_name,display_name_alternatives,orcid,works_count",
            "per_page": "100",
        },
        api_key,
    )
    found = {}
    for result in payload.get("results", []):
        author_id = (result.get("id") or "").rsplit("/", 1)[-1]
        if author_id.startswith("A"):
            found[author_id] = result
    return [{"openalex_author_id": author_id, "profile": found.get(author_id)} for author_id in batch]


def get_author_profiles(author_ids: list[str], api_key: str) -> dict[str, dict]:
    cache = load_jsonl_cache(AUTHOR_CACHE_JSONL, "openalex_author_id")
    pending = [author_id for author_id in author_ids if author_id not in cache]
    batches = list(chunks(pending, AUTHOR_BATCH_SIZE))
    print(f"OpenAlex author cache: {len(author_ids) - len(pending)}/{len(author_ids)} profiles", flush=True)
    if batches:
        with ThreadPoolExecutor(max_workers=MAX_WORKERS) as executor:
            futures = {executor.submit(fetch_author_batch, batch, api_key): index for index, batch in enumerate(batches, 1)}
            for completed, future in enumerate(as_completed(futures), 1):
                rows = future.result()
                append_jsonl(AUTHOR_CACHE_JSONL, rows)
                cache.update({row["openalex_author_id"]: row for row in rows})
                if completed % 10 == 0 or completed == len(batches):
                    print(f"OpenAlex author batches: {completed}/{len(batches)}", flush=True)
    return cache


def collapse_resolved_papers(resolved: list[dict]) -> list[dict]:
    by_identity_paper: dict[tuple[str, str], dict] = {}
    for record in resolved:
        key = (record["openalex_author_id"], record["paper_key"])
        existing = by_identity_paper.get(key)
        if existing is None:
            by_identity_paper[key] = {
                **record,
                "source_author_names": {record["source_author"]},
                "match_types": {record["match_type"]},
                "reasons": set(record["reasons"]),
                "publishers": set(record["publishers"]),
            }
        else:
            existing["source_author_names"].add(record["source_author"])
            existing["match_types"].add(record["match_type"])
            existing["reasons"].update(record["reasons"])
            existing["publishers"].update(record["publishers"])
            if record["retraction_year"] is not None and (
                existing["retraction_year"] is None or record["retraction_year"] < existing["retraction_year"]
            ):
                existing["retraction_year"] = record["retraction_year"]
                if existing["publication_year"] is not None:
                    existing["retraction_lag_years"] = (
                        existing["retraction_year"] - existing["publication_year"]
                    )
    return list(by_identity_paper.values())


def aggregate_outputs(papers: list[dict], profiles: dict[str, dict], statuses: list[dict], candidate_names: list[str]) -> None:
    papers_by_author: dict[str, list[dict]] = defaultdict(list)
    for paper in papers:
        papers_by_author[paper["openalex_author_id"]].append(paper)

    leaderboard = []
    for author_id, author_papers in papers_by_author.items():
        cache_row = profiles.get(author_id) or {}
        profile = cache_row.get("profile") or {}
        display_name = profile.get("display_name") or author_papers[0]["authorship_display_name"]
        orcid = profile.get("orcid") or author_papers[0]["authorship_orcid"] or ""
        works_count = int(profile.get("works_count") or 0)
        unique_count = len(author_papers)
        rate = 100 * unique_count / works_count if works_count else None
        source_names = sorted({name for paper in author_papers for name in paper["source_author_names"]})
        exact_count = sum("Initial" not in paper["match_types"] for paper in author_papers)
        initial_count = unique_count - exact_count
        years = [paper["retraction_year"] for paper in author_papers if paper["retraction_year"] is not None]
        lags = [paper["retraction_lag_years"] for paper in author_papers if paper["retraction_lag_years"] is not None and paper["retraction_lag_years"] >= 0]

        if initial_count > 0 or works_count and unique_count > works_count:
            confidence = "Review"
        elif orcid or unique_count >= 2:
            confidence = "High"
        else:
            confidence = "Probable"

        leaderboard.append(
            {
                "rank": 0,
                "openalex_author_id": author_id,
                "author": display_name,
                "rwd_name_variants": "; ".join(source_names),
                "unique_retracted_papers": unique_count,
                "openalex_works": works_count,
                "retracted_papers_per_100_works": round(rate, 2) if rate is not None else "",
                "first_retraction_year": min(years) if years else "",
                "last_retraction_year": max(years) if years else "",
                "median_retraction_lag_years": round(statistics.median(lags), 1) if lags else "",
                "orcid": orcid,
                "match_confidence": confidence,
                "exact_match_papers": exact_count,
                "initial_match_papers": initial_count,
            }
        )

    leaderboard.sort(key=lambda row: (-row["unique_retracted_papers"], row["author"], row["openalex_author_id"]))
    for rank, row in enumerate(leaderboard, 1):
        row["rank"] = rank

    leaderboard_fields = [
        "rank",
        "openalex_author_id",
        "author",
        "rwd_name_variants",
        "unique_retracted_papers",
        "openalex_works",
        "retracted_papers_per_100_works",
        "first_retraction_year",
        "last_retraction_year",
        "median_retraction_lag_years",
        "orcid",
        "match_confidence",
        "exact_match_papers",
        "initial_match_papers",
    ]
    write_rows(LEADERBOARD_CSV, leaderboard, leaderboard_fields)

    display_by_id = {row["openalex_author_id"]: row["author"] for row in leaderboard}
    paper_rows = []
    reason_counter: Counter[tuple[str, str]] = Counter()
    publisher_counter: Counter[tuple[str, str]] = Counter()
    for paper in papers:
        author_id = paper["openalex_author_id"]
        paper_rows.append(
            {
                "openalex_author_id": author_id,
                "author": display_by_id[author_id],
                "paper_key": paper["paper_key"],
                "doi": paper["doi"],
                "source_author_names": "; ".join(sorted(paper["source_author_names"])),
                "publication_year": paper["publication_year"] if paper["publication_year"] is not None else "",
                "retraction_year": paper["retraction_year"] if paper["retraction_year"] is not None else "",
                "retraction_lag_years": paper["retraction_lag_years"] if paper["retraction_lag_years"] is not None else "",
                "match_type": "Initial" if "Initial" in paper["match_types"] else "Exact",
            }
        )
        for reason in paper["reasons"]:
            if reason and not EXCLUDED_REASON_PATTERN.search(reason):
                reason_counter[(author_id, reason)] += 1
        for publisher in paper["publishers"]:
            if publisher:
                publisher_counter[(author_id, publisher)] += 1

    paper_rows.sort(key=lambda row: (row["openalex_author_id"], row["retraction_year"] or 0, row["paper_key"]))
    write_rows(
        PAPERS_CSV,
        paper_rows,
        [
            "openalex_author_id",
            "author",
            "paper_key",
            "doi",
            "source_author_names",
            "publication_year",
            "retraction_year",
            "retraction_lag_years",
            "match_type",
        ],
    )

    reason_rows = [
        {
            "openalex_author_id": author_id,
            "author": display_by_id[author_id],
            "reason": reason,
            "unique_retracted_papers": count,
        }
        for (author_id, reason), count in reason_counter.items()
    ]
    reason_rows.sort(key=lambda row: (row["openalex_author_id"], -row["unique_retracted_papers"], row["reason"]))
    write_rows(REASONS_CSV, reason_rows, ["openalex_author_id", "author", "reason", "unique_retracted_papers"])

    publisher_rows = [
        {
            "openalex_author_id": author_id,
            "author": display_by_id[author_id],
            "publisher": publisher,
            "unique_retracted_papers": count,
        }
        for (author_id, publisher), count in publisher_counter.items()
    ]
    publisher_rows.sort(key=lambda row: (row["openalex_author_id"], -row["unique_retracted_papers"], row["publisher"]))
    write_rows(PUBLISHERS_CSV, publisher_rows, ["openalex_author_id", "author", "publisher", "unique_retracted_papers"])

    status_by_name: dict[str, Counter[str]] = defaultdict(Counter)
    for status in statuses:
        status_by_name[status["source_author"]][status["status"]] += 1
    unresolved_rows = []
    for name in candidate_names:
        counts = status_by_name[name]
        total = sum(counts.values())
        resolved_count = counts["Resolved"]
        unresolved_rows.append(
            {
                "source_author": name,
                "candidate_papers": total,
                "resolved_papers": resolved_count,
                "no_doi": counts["No DOI"],
                "work_not_found": counts["Work not found"],
                "unmatched_name": counts["Unmatched"],
                "ambiguous_name": counts["Ambiguous"],
                "resolution_pct": round(100 * resolved_count / total, 2) if total else 0,
            }
        )
    write_rows(
        UNRESOLVED_CSV,
        unresolved_rows,
        [
            "source_author",
            "candidate_papers",
            "resolved_papers",
            "no_doi",
            "work_not_found",
            "unmatched_name",
            "ambiguous_name",
            "resolution_pct",
        ],
    )

    total_pairs = len(statuses)
    doi_pairs = sum(status["status"] != "No DOI" for status in statuses)
    resolved_pairs = sum(status["status"] == "Resolved" for status in statuses)
    unique_dois = len({paper["doi"] for paper in papers if paper["doi"]})
    summary = {
        "candidate_raw_names": len(candidate_names),
        "candidate_author_paper_pairs": total_pairs,
        "candidate_pairs_with_doi": doi_pairs,
        "resolved_author_paper_pairs": resolved_pairs,
        "resolution_pct_all_pairs": round(100 * resolved_pairs / total_pairs, 2) if total_pairs else 0,
        "resolution_pct_doi_pairs": round(100 * resolved_pairs / doi_pairs, 2) if doi_pairs else 0,
        "resolved_openalex_authors": len(leaderboard),
        "resolved_unique_dois": unique_dois,
        "retrieved_date": date.today().isoformat(),
        "source": "OpenAlex",
        "candidate_name_limit": RAW_NAME_CANDIDATE_LIMIT,
    }
    write_rows(SUMMARY_CSV, [{"metric": key, "value": value} for key, value in summary.items()], ["metric", "value"])


def main() -> None:
    api_key = load_api_key()
    records, candidate_names = load_candidate_records()
    dois = sorted({record["doi"] for record in records if record["doi"]})
    print(f"Candidate raw names: {len(candidate_names)}", flush=True)
    print(f"Candidate author-paper pairs: {len(records)}", flush=True)
    print(f"Unique valid DOI records: {len(dois)}", flush=True)

    work_cache = get_works(dois, api_key)
    resolved, statuses = resolve_records(records, work_cache)
    papers = collapse_resolved_papers(resolved)
    author_ids = sorted({paper["openalex_author_id"] for paper in papers})
    print(f"Resolved author-paper pairs: {len(papers)}", flush=True)
    print(f"Resolved OpenAlex author IDs: {len(author_ids)}", flush=True)

    profiles = get_author_profiles(author_ids, api_key)
    aggregate_outputs(papers, profiles, statuses, candidate_names)
    print(f"Raw leaderboard rows: {len(author_ids)}", flush=True)

    # Keep the app-facing files synchronized whenever the DOI matching stage is refreshed.
    from review_author_identities import main as review_identities

    review_identities()


if __name__ == "__main__":
    main()
