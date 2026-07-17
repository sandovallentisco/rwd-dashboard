"""Conservatively review and merge fragmented OpenAlex author identities.

This second-stage pipeline starts from the DOI-level matches produced by
``build_author_identities.py``. It never merges profiles on name similarity
alone. Automatic merges require a shared ORCID or strong continuity in both
coauthor and affiliation evidence. All abbreviated-name assignments and pair
decisions are written to an audit table.
"""

from __future__ import annotations

import csv
import itertools
import statistics
from collections import Counter, defaultdict
from datetime import date
from pathlib import Path

from build_author_identities import (
    WORKS_URL,
    WORK_CACHE_JSONL,
    api_get,
    append_jsonl,
    initial_compatible,
    load_api_key,
    load_jsonl_cache,
    normalize_name,
)


ROOT = Path(__file__).resolve().parents[1]

RAW_LEADERBOARD = ROOT / "author_identity_raw_leaderboard.csv"
RAW_PAPERS = ROOT / "author_identity_raw_papers.csv"
RAW_REASONS = ROOT / "author_identity_raw_reasons.csv"
RAW_PUBLISHERS = ROOT / "author_identity_raw_publishers.csv"
RAW_UNRESOLVED = ROOT / "author_identity_raw_unresolved.csv"
RAW_SUMMARY = ROOT / "author_identity_raw_summary.csv"

FINAL_LEADERBOARD = ROOT / "author_identity_leaderboard.csv"
FINAL_PAPERS = ROOT / "author_identity_papers.csv"
FINAL_REASONS = ROOT / "author_identity_reasons.csv"
FINAL_PUBLISHERS = ROOT / "author_identity_publishers.csv"
FINAL_UNRESOLVED = ROOT / "author_identity_unresolved.csv"
FINAL_SUMMARY = ROOT / "author_identity_summary.csv"

REVIEW_CSV = ROOT / "author_identity_review.csv"
MERGE_MAP_CSV = ROOT / "author_identity_merge_map.csv"
AUTHOR_WORKS_CACHE = ROOT / "author_identity_author_works_cache.jsonl"


def read_rows(path: Path) -> list[dict[str, str]]:
    with path.open("r", encoding="utf-8", newline="") as handle:
        return list(csv.DictReader(handle))


def write_rows(path: Path, rows: list[dict], fieldnames: list[str]) -> None:
    with path.open("w", encoding="utf-8", newline="") as handle:
        writer = csv.DictWriter(handle, fieldnames=fieldnames)
        writer.writeheader()
        writer.writerows(rows)


def as_int(value) -> int | None:
    try:
        return int(float(value))
    except (TypeError, ValueError):
        return None


def as_float(value) -> float | None:
    try:
        return float(value)
    except (TypeError, ValueError):
        return None


def normalize_orcid(value: str) -> str:
    return (value or "").strip().lower().removeprefix("https://orcid.org/")


def split_values(value: str) -> set[str]:
    return {item.strip() for item in (value or "").split(";") if item.strip()}


def openalex_id(value: str) -> str:
    return (value or "").rsplit("/", 1)[-1]


def new_evidence() -> dict:
    return {
        "papers": set(),
        "exact_papers": set(),
        "initial_papers": set(),
        "coauthor_ids": set(),
        "coauthor_names": set(),
        "institution_ids": set(),
        "affiliations": set(),
        "countries": set(),
        "years": set(),
        "exact_coauthor_names": set(),
        "initial_coauthor_names": set(),
        "exact_institution_ids": set(),
        "initial_institution_ids": set(),
        "exact_affiliations": set(),
        "initial_affiliations": set(),
    }


def extract_work_evidence(work: dict, author_id: str) -> dict:
    target = None
    coauthor_ids: set[str] = set()
    coauthor_names: set[str] = set()

    for authorship in work.get("authorships") or []:
        candidate_id = openalex_id((authorship.get("author") or {}).get("id"))
        if candidate_id == author_id:
            target = authorship
            continue
        if candidate_id.startswith("A"):
            coauthor_ids.add(candidate_id)
        author = authorship.get("author") or {}
        for name in (authorship.get("raw_author_name"), author.get("display_name")):
            normalized = normalize_name(name or "")
            if normalized:
                coauthor_names.add(normalized)

    if target is None:
        return {
            "coauthor_ids": coauthor_ids,
            "coauthor_names": coauthor_names,
            "institution_ids": set(),
            "affiliations": set(),
            "countries": set(),
        }

    institution_ids = {
        openalex_id(institution.get("id"))
        for institution in target.get("institutions") or []
        if openalex_id(institution.get("id")).startswith("I")
    }
    affiliations = {
        normalize_name(value)
        for value in target.get("raw_affiliation_strings") or []
        if normalize_name(value)
    }
    countries = {value for value in target.get("countries") or [] if value}
    return {
        "coauthor_ids": coauthor_ids,
        "coauthor_names": coauthor_names,
        "institution_ids": institution_ids,
        "affiliations": affiliations,
        "countries": countries,
    }


def build_evidence(papers: list[dict], work_cache: dict[str, dict]) -> dict[tuple[str, str], dict]:
    evidence: dict[tuple[str, str], dict] = defaultdict(new_evidence)

    for paper in papers:
        author_id = paper["openalex_author_id"]
        doi = paper.get("doi", "")
        cached = work_cache.get(doi) or {}
        work = cached.get("work") or {}
        features = extract_work_evidence(work, author_id)
        match_type = paper.get("match_type") or "Exact"
        match_prefix = "initial" if match_type == "Initial" else "exact"
        publication_year = as_int(paper.get("publication_year"))

        for source_name in split_values(paper.get("source_author_names", "")):
            row = evidence[(source_name, author_id)]
            row["papers"].add(paper["paper_key"])
            row[f"{match_prefix}_papers"].add(paper["paper_key"])
            row["coauthor_ids"].update(features["coauthor_ids"])
            row["coauthor_names"].update(features["coauthor_names"])
            row["institution_ids"].update(features["institution_ids"])
            row["affiliations"].update(features["affiliations"])
            row["countries"].update(features["countries"])
            row[f"{match_prefix}_coauthor_names"].update(features["coauthor_names"])
            row[f"{match_prefix}_institution_ids"].update(features["institution_ids"])
            row[f"{match_prefix}_affiliations"].update(features["affiliations"])
            if publication_year is not None:
                row["years"].add(publication_year)

    return evidence


def year_gap(left: set[int], right: set[int]) -> int | None:
    if not left or not right:
        return None
    left_min, left_max = min(left), max(left)
    right_min, right_max = min(right), max(right)
    if left_max < right_min:
        return right_min - left_max
    if right_max < left_min:
        return left_min - right_max
    return 0


def display_names_compatible(source_name: str, left_name: str, right_name: str) -> bool:
    return all(
        normalize_name(value) == normalize_name(source_name) or initial_compatible(source_name, value)
        for value in (left_name, right_name)
        if value
    )


class UnionFind:
    def __init__(self, items: list[str], orcids: dict[str, str]):
        self.parent = {item: item for item in items}
        self.orcids = {item: ({orcids[item]} if orcids.get(item) else set()) for item in items}

    def find(self, item: str) -> str:
        root = item
        while self.parent[root] != root:
            root = self.parent[root]
        while self.parent[item] != item:
            parent = self.parent[item]
            self.parent[item] = root
            item = parent
        return root

    def union(self, left: str, right: str) -> bool:
        left_root, right_root = self.find(left), self.find(right)
        if left_root == right_root:
            return True
        combined_orcids = self.orcids[left_root] | self.orcids[right_root]
        if len(combined_orcids) > 1:
            return False
        if right_root < left_root:
            left_root, right_root = right_root, left_root
        self.parent[right_root] = left_root
        self.orcids[left_root] = combined_orcids
        return True


def pair_decision(
    source_name: str,
    group_size: int,
    left_id: str,
    right_id: str,
    left: dict,
    right: dict,
    profiles: dict[str, dict],
) -> dict:
    shared_coauthor_ids = left["coauthor_ids"] & right["coauthor_ids"]
    shared_coauthor_names = left["coauthor_names"] & right["coauthor_names"]
    shared_institutions = left["institution_ids"] & right["institution_ids"]
    shared_affiliations = left["affiliations"] & right["affiliations"]
    shared_countries = left["countries"] & right["countries"]
    gap = year_gap(left["years"], right["years"])

    left_profile = profiles[left_id]
    right_profile = profiles[right_id]
    left_orcid = normalize_orcid(left_profile.get("orcid", ""))
    right_orcid = normalize_orcid(right_profile.get("orcid", ""))
    same_orcid = bool(left_orcid and left_orcid == right_orcid)
    conflicting_orcid = bool(left_orcid and right_orcid and left_orcid != right_orcid)
    names_compatible = display_names_compatible(
        source_name,
        left_profile.get("author", ""),
        right_profile.get("author", ""),
    )
    source_specific = len(normalize_name(source_name).split()) >= 3 or group_size <= 4

    if same_orcid:
        decision = "Automatic merge"
        confidence = "High"
        rationale = "The OpenAlex profiles share the same ORCID."
    elif conflicting_orcid:
        decision = "Keep separate"
        confidence = "High"
        rationale = "The OpenAlex profiles have different ORCID identifiers."
    elif not names_compatible:
        decision = "Keep separate"
        confidence = "High"
        rationale = "The OpenAlex display names are incompatible with the same RWD source name."
    else:
        strong_network = len(shared_coauthor_ids) >= 2 or len(shared_coauthor_names) >= 3
        shared_affiliation = bool(shared_institutions or shared_affiliations)
        very_strong_network = len(shared_coauthor_ids) >= 4 or len(shared_coauthor_names) >= 5
        singleton_fragment_bridge = (
            min(len(left["papers"]), len(right["papers"])) == 1
            and max(len(left["papers"]), len(right["papers"])) >= 3
            and bool(shared_institutions)
            and bool(shared_affiliations)
            and len(shared_coauthor_ids) >= 1
            and gap == 0
        )

        if singleton_fragment_bridge:
            decision = "Automatic merge"
            confidence = "High"
            rationale = (
                "The one-paper profile matches the larger profile on the RWD name, year, institution, "
                "affiliation and at least one coauthor."
            )
        elif source_specific and (very_strong_network or (strong_network and shared_affiliation)):
            decision = "Automatic merge"
            confidence = "High"
            rationale = (
                f"Same RWD name with {len(shared_coauthor_names)} shared coauthors and "
                f"{len(shared_institutions) + len(shared_affiliations)} shared affiliation signals."
            )
        elif source_specific and shared_affiliation and len(shared_coauthor_names) >= 1:
            decision = "Probable merge"
            confidence = "Review"
            rationale = "The profiles share an affiliation and a coauthor, but that is not enough to merge them automatically."
        else:
            decision = "Unresolved"
            confidence = "Review"
            rationale = "The names are compatible, but there are not enough shared coauthors or institutions to link the profiles."

    return {
        "review_type": "Profile pair",
        "source_author": source_name,
        "openalex_author_id": left_id,
        "compared_openalex_author_id": right_id,
        "author": left_profile.get("author", ""),
        "compared_author": right_profile.get("author", ""),
        "exact_match_papers": len(left["exact_papers"]),
        "initial_match_papers": len(left["initial_papers"]),
        "compared_exact_match_papers": len(right["exact_papers"]),
        "compared_initial_match_papers": len(right["initial_papers"]),
        "shared_coauthors": len(shared_coauthor_names),
        "shared_coauthor_ids": len(shared_coauthor_ids),
        "shared_institutions": len(shared_institutions),
        "shared_affiliations": len(shared_affiliations),
        "shared_countries": "; ".join(sorted(shared_countries)),
        "publication_year_gap": "" if gap is None else gap,
        "orcid_relation": "Same" if same_orcid else "Different" if conflicting_orcid else "Unavailable",
        "decision": decision,
        "confidence": confidence,
        "rationale": rationale,
    }


def identity_validation_row(source_name: str, author_id: str, row: dict, profile: dict) -> dict:
    shared_coauthors = row["exact_coauthor_names"] & row["initial_coauthor_names"]
    shared_institutions = row["exact_institution_ids"] & row["initial_institution_ids"]
    shared_affiliations = row["exact_affiliations"] & row["initial_affiliations"]
    exact_count = len(row["exact_papers"])
    initial_count = len(row["initial_papers"])

    if initial_count == 0:
        decision = "Exact match"
        confidence = "High" if exact_count >= 2 or profile.get("orcid") else "Probable"
        rationale = "Every matched paper uses the full RWD name."
    elif exact_count >= 3:
        decision = "Confirmed abbreviation"
        confidence = "High"
        rationale = f"The same OpenAlex profile contains {exact_count} papers that match the full RWD name exactly."
    elif exact_count >= 1 and (len(shared_coauthors) >= 2 or shared_institutions or shared_affiliations):
        decision = "Confirmed abbreviation"
        confidence = "High"
        rationale = (
            f"Full-name and abbreviated papers share {len(shared_coauthors)} coauthors and "
            f"{len(shared_institutions) + len(shared_affiliations)} institution or affiliation matches."
        )
    else:
        decision = "Unresolved abbreviation"
        confidence = "Review"
        rationale = "No exact full-name paper or strong link to one was found."

    return {
        "review_type": "Abbreviated-name validation",
        "source_author": source_name,
        "openalex_author_id": author_id,
        "compared_openalex_author_id": "",
        "author": profile.get("author", ""),
        "compared_author": "",
        "exact_match_papers": exact_count,
        "initial_match_papers": initial_count,
        "compared_exact_match_papers": "",
        "compared_initial_match_papers": "",
        "shared_coauthors": len(shared_coauthors),
        "shared_coauthor_ids": "",
        "shared_institutions": len(shared_institutions),
        "shared_affiliations": len(shared_affiliations),
        "shared_countries": "; ".join(sorted(row["countries"])),
        "publication_year_gap": "",
        "orcid_relation": "Present" if normalize_orcid(profile.get("orcid", "")) else "Unavailable",
        "decision": decision,
        "confidence": confidence,
        "rationale": rationale,
    }


def choose_canonical(members: list[str], profiles: dict[str, dict]) -> str:
    def score(author_id: str):
        profile = profiles[author_id]
        name = profile.get("author", "")
        return (
            1 if normalize_orcid(profile.get("orcid", "")) else 0,
            as_int(profile.get("exact_match_papers")) or 0,
            len(normalize_name(name).split()),
            len(name),
            as_int(profile.get("unique_retracted_papers")) or 0,
            author_id,
        )

    return max(members, key=score)


def fetch_union_work_count(author_ids: list[str], api_key: str, cache: dict[str, dict]) -> tuple[int, str]:
    if len(author_ids) == 1:
        return 0, "Profile count"

    key = "|".join(sorted(author_ids))
    cached = cache.get(key)
    if cached:
        return len(set(cached.get("work_ids") or [])), "OpenAlex union"

    cursor = "*"
    work_ids: set[str] = set()
    while cursor:
        payload = api_get(
            WORKS_URL,
            {
                "filter": f"author.id:{'|'.join(sorted(author_ids))}",
                "select": "id",
                "per_page": "100",
                "cursor": cursor,
            },
            api_key,
        )
        for work in payload.get("results") or []:
            work_id = openalex_id(work.get("id"))
            if work_id.startswith("W"):
                work_ids.add(work_id)
        next_cursor = (payload.get("meta") or {}).get("next_cursor")
        if not next_cursor or next_cursor == cursor or not payload.get("results"):
            break
        cursor = next_cursor

    record = {
        "author_ids_key": key,
        "author_ids": sorted(author_ids),
        "work_ids": sorted(work_ids),
        "retrieved_date": date.today().isoformat(),
    }
    append_jsonl(AUTHOR_WORKS_CACHE, [record])
    cache[key] = record
    return len(work_ids), "OpenAlex union"


def collapse_final_papers(raw_papers: list[dict], canonical_by_id: dict[str, str]) -> list[dict]:
    collapsed: dict[tuple[str, str], dict] = {}

    for paper in raw_papers:
        source_id = paper["openalex_author_id"]
        canonical_id = canonical_by_id[source_id]
        key = (canonical_id, paper["paper_key"])
        existing = collapsed.get(key)
        if existing is None:
            collapsed[key] = {
                **paper,
                "openalex_author_id": canonical_id,
                "source_openalex_author_ids": {source_id},
                "source_author_names_set": split_values(paper.get("source_author_names", "")),
                "match_types": {paper.get("match_type") or "Exact"},
            }
        else:
            existing["source_openalex_author_ids"].add(source_id)
            existing["source_author_names_set"].update(split_values(paper.get("source_author_names", "")))
            existing["match_types"].add(paper.get("match_type") or "Exact")

    final = []
    for row in collapsed.values():
        row["source_openalex_author_ids"] = "; ".join(sorted(row["source_openalex_author_ids"]))
        row["source_author_names"] = "; ".join(sorted(row.pop("source_author_names_set")))
        row["match_type"] = "Exact" if "Exact" in row.pop("match_types") else "Initial"
        final.append(row)
    return final


def aggregate_category_rows(
    raw_rows: list[dict],
    canonical_by_id: dict[str, str],
    display_by_id: dict[str, str],
    category: str,
) -> list[dict]:
    counts: Counter[tuple[str, str]] = Counter()
    for row in raw_rows:
        canonical_id = canonical_by_id[row["openalex_author_id"]]
        counts[(canonical_id, row[category])] += as_int(row.get("unique_retracted_papers")) or 0

    result = [
        {
            "openalex_author_id": author_id,
            "author": display_by_id[author_id],
            category: value,
            "unique_retracted_papers": count,
        }
        for (author_id, value), count in counts.items()
    ]
    result.sort(key=lambda row: (row["openalex_author_id"], -row["unique_retracted_papers"], row[category]))
    return result


def main() -> None:
    raw_leaderboard = read_rows(RAW_LEADERBOARD)
    raw_papers = read_rows(RAW_PAPERS)
    raw_reasons = read_rows(RAW_REASONS)
    raw_publishers = read_rows(RAW_PUBLISHERS)
    raw_summary = {row["metric"]: row["value"] for row in read_rows(RAW_SUMMARY)}
    profiles = {row["openalex_author_id"]: row for row in raw_leaderboard}
    author_ids = sorted(profiles)

    work_cache = load_jsonl_cache(WORK_CACHE_JSONL, "doi")
    evidence = build_evidence(raw_papers, work_cache)
    by_source: dict[str, list[str]] = defaultdict(list)
    for source_name, author_id in evidence:
        by_source[source_name].append(author_id)
    by_source = {name: sorted(set(ids)) for name, ids in by_source.items()}

    orcids = {author_id: normalize_orcid(profile.get("orcid", "")) for author_id, profile in profiles.items()}
    union_find = UnionFind(author_ids, orcids)
    raw_top_review_ids = {
        row["openalex_author_id"]
        for row in raw_leaderboard[:35]
        if row.get("match_confidence") == "Review"
    }

    pair_rows: list[dict] = []
    automatic_pairs: list[dict] = []
    for source_name, ids in by_source.items():
        if len(ids) < 2:
            continue
        for left_id, right_id in itertools.combinations(ids, 2):
            row = pair_decision(
                source_name,
                len(ids),
                left_id,
                right_id,
                evidence[(source_name, left_id)],
                evidence[(source_name, right_id)],
                profiles,
            )
            if row["decision"] == "Automatic merge":
                if union_find.union(left_id, right_id):
                    automatic_pairs.append(row)
                else:
                    row["decision"] = "Keep separate"
                    row["confidence"] = "High"
                    row["rationale"] = "The proposed merge would combine conflicting ORCID identifiers through a transitive group."
            if row["decision"] != "Unresolved" or left_id in raw_top_review_ids or right_id in raw_top_review_ids:
                pair_rows.append(row)

    groups_by_root: dict[str, list[str]] = defaultdict(list)
    for author_id in author_ids:
        groups_by_root[union_find.find(author_id)].append(author_id)

    canonical_by_id: dict[str, str] = {}
    groups_by_canonical: dict[str, list[str]] = {}
    for members in groups_by_root.values():
        canonical = choose_canonical(members, profiles)
        groups_by_canonical[canonical] = sorted(members)
        for member in members:
            canonical_by_id[member] = canonical

    validation_rows: list[dict] = []
    validation_by_node: dict[tuple[str, str], dict] = {}
    for (source_name, author_id), row in evidence.items():
        if row["initial_papers"]:
            review = identity_validation_row(source_name, author_id, row, profiles[author_id])
            validation_rows.append(review)
            validation_by_node[(source_name, author_id)] = review

    for (source_name, author_id), review in validation_by_node.items():
        if review["decision"] != "Unresolved abbreviation":
            continue
        canonical = canonical_by_id[author_id]
        peers = groups_by_canonical[canonical]
        exact_peer_count = sum(
            len(evidence.get((source_name, peer), new_evidence())["exact_papers"])
            for peer in peers
            if peer != author_id
        )
        if exact_peer_count:
            review["decision"] = "Confirmed by profile merge"
            review["confidence"] = "High"
            review["rationale"] = f"A merged OpenAlex profile provides {exact_peer_count} exact full-name matches."

    final_papers = collapse_final_papers(raw_papers, canonical_by_id)
    papers_by_author: dict[str, list[dict]] = defaultdict(list)
    for paper in final_papers:
        papers_by_author[paper["openalex_author_id"]].append(paper)

    api_key = load_api_key()
    author_works_cache = load_jsonl_cache(AUTHOR_WORKS_CACHE, "author_ids_key")
    group_evidence: dict[str, tuple[str, str]] = {}
    final_leaderboard: list[dict] = []

    for canonical, members in groups_by_canonical.items():
        author_papers = papers_by_author.get(canonical, [])
        if not author_papers:
            continue
        canonical_profile = profiles[canonical]
        member_orcids = sorted({orcids[member] for member in members if orcids[member]})

        if len(members) == 1:
            works_count = as_int(canonical_profile.get("openalex_works")) or 0
            works_method = "Profile count"
        else:
            try:
                works_count, works_method = fetch_union_work_count(members, api_key, author_works_cache)
            except RuntimeError:
                works_count = sum(as_int(profiles[member].get("openalex_works")) or 0 for member in members)
                works_method = "Summed profile counts"

        exact_count = sum(paper.get("match_type") == "Exact" for paper in author_papers)
        initial_count = len(author_papers) - exact_count
        source_names = sorted(
            {name for paper in author_papers for name in split_values(paper.get("source_author_names", ""))}
        )
        publication_years = [as_int(paper.get("publication_year")) for paper in author_papers]
        retraction_years = [as_int(paper.get("retraction_year")) for paper in author_papers]
        lags = [as_float(paper.get("retraction_lag_years")) for paper in author_papers]
        publication_years = [value for value in publication_years if value is not None]
        retraction_years = [value for value in retraction_years if value is not None]
        lags = [value for value in lags if value is not None and value >= 0]

        unresolved_nodes = []
        confirmed_nodes = []
        for source_name in source_names:
            for member in members:
                review = validation_by_node.get((source_name, member))
                if review is None:
                    continue
                if review["confidence"] == "High":
                    confirmed_nodes.append(review)
                else:
                    unresolved_nodes.append(review)

        relevant_pairs = [
            row for row in automatic_pairs
            if row["openalex_author_id"] in members and row["compared_openalex_author_id"] in members
        ]
        merge_evidence = "; ".join(sorted({row["rationale"] for row in relevant_pairs}))

        if unresolved_nodes:
            match_confidence = "Review"
            unresolved_evidence = "; ".join(sorted({row["rationale"] for row in unresolved_nodes}))
            if len(members) > 1:
                identity_decision = f"Merged {len(members)} profile fragments; full name unresolved"
                identity_decision_explanation = (
                    f"We combined {len(members)} OpenAlex fragments because their papers have the same RWD name, year, "
                    "institution, affiliation and coauthors. We did not connect them to a profile that spells out the full name."
                )
                review_evidence = f"{merge_evidence} Remaining limitation: {unresolved_evidence}"
            else:
                identity_decision = "Unresolved abbreviation"
                identity_decision_explanation = (
                    "We kept this OpenAlex profile separate because we could not identify one full-name profile with enough certainty."
                )
                review_evidence = unresolved_evidence
            match_confidence_explanation = (
                f"Needs review: all {initial_count} papers use an abbreviated OpenAlex name. There is no ORCID and no "
                f"paper that spells out ‘{'; '.join(source_names)}’, so the full name cannot be confirmed."
            )
        elif len(members) > 1:
            match_confidence = "High"
            identity_decision = f"Merged {len(members)} OpenAlex profiles"
            review_evidence = merge_evidence
            match_confidence_explanation = (
                "High confidence: the profiles share the same ORCID or repeatedly match on coauthors and institutions. "
                "No conflicting ORCID was found, and the name alone was not enough to merge them."
            )
            identity_decision_explanation = (
                f"We combined {len(members)} OpenAlex profiles under {canonical} because the evidence indicates the same person. "
                "A work appearing in more than one profile is counted only once."
            )
        elif confirmed_nodes:
            match_confidence = "High"
            identity_decision = "Confirmed abbreviation"
            review_evidence = "; ".join(sorted({row["rationale"] for row in confirmed_nodes}))
            match_confidence_explanation = (
                f"High confidence: {initial_count} papers use a shortened name and {exact_count} papers use the full name "
                "on the same OpenAlex profile. The evidence below links the two forms."
            )
            identity_decision_explanation = (
                "We kept the shortened-name papers in this identity. No additional OpenAlex profile had to be merged."
            )
        else:
            match_confidence = "High" if len(author_papers) >= 2 or member_orcids else "Probable"
            identity_decision = "Exact DOI-level match"
            review_evidence = "All matched papers use an exact normalized author name."
            if match_confidence == "High":
                match_confidence_explanation = (
                    f"High confidence: all {exact_count} papers match the full RWD name exactly"
                    + (" and the profile also has an ORCID." if member_orcids else ".")
                )
            else:
                match_confidence_explanation = (
                    "Probable confidence: one paper matches the full RWD name, but there is no ORCID or second paper to confirm it."
                )
            identity_decision_explanation = (
                "We kept this as one OpenAlex identity and did not merge any other profile into it."
            )

        unique_count = len(author_papers)
        rate = 100 * unique_count / works_count if works_count else None
        group_evidence[canonical] = (identity_decision, review_evidence)
        final_leaderboard.append(
            {
                "rank": 0,
                "openalex_author_id": canonical,
                "author": canonical_profile.get("author", ""),
                "rwd_name_variants": "; ".join(source_names),
                "unique_retracted_papers": unique_count,
                "openalex_works": works_count,
                "retracted_papers_per_100_works": round(rate, 2) if rate is not None else "",
                "first_retraction_year": min(retraction_years) if retraction_years else "",
                "last_retraction_year": max(retraction_years) if retraction_years else "",
                "median_retraction_lag_years": round(statistics.median(lags), 1) if lags else "",
                "orcid": f"https://orcid.org/{member_orcids[0]}" if len(member_orcids) == 1 else "",
                "match_confidence": match_confidence,
                "match_confidence_explanation": match_confidence_explanation,
                "identity_decision": identity_decision,
                "identity_decision_explanation": identity_decision_explanation,
                "review_evidence": review_evidence,
                "merged_openalex_ids": "; ".join(members),
                "raw_openalex_profile_count": len(members),
                "works_count_method": works_method,
                "exact_match_papers": exact_count,
                "initial_match_papers": initial_count,
            }
        )

    final_leaderboard.sort(
        key=lambda row: (-row["unique_retracted_papers"], row["author"], row["openalex_author_id"])
    )
    for rank, row in enumerate(final_leaderboard, 1):
        row["rank"] = rank

    display_by_id = {row["openalex_author_id"]: row["author"] for row in final_leaderboard}
    for paper in final_papers:
        paper["author"] = display_by_id[paper["openalex_author_id"]]
    final_papers.sort(
        key=lambda row: (row["openalex_author_id"], as_int(row.get("retraction_year")) or 0, row["paper_key"])
    )

    final_reasons = aggregate_category_rows(raw_reasons, canonical_by_id, display_by_id, "reason")
    final_publishers = aggregate_category_rows(raw_publishers, canonical_by_id, display_by_id, "publisher")

    leaderboard_fields = [
        "rank", "openalex_author_id", "author", "rwd_name_variants", "unique_retracted_papers",
        "openalex_works", "retracted_papers_per_100_works", "first_retraction_year",
        "last_retraction_year", "median_retraction_lag_years", "orcid", "match_confidence",
        "match_confidence_explanation", "identity_decision", "identity_decision_explanation",
        "review_evidence", "merged_openalex_ids", "raw_openalex_profile_count",
        "works_count_method", "exact_match_papers", "initial_match_papers",
    ]
    write_rows(FINAL_LEADERBOARD, final_leaderboard, leaderboard_fields)
    write_rows(
        FINAL_PAPERS,
        final_papers,
        [
            "openalex_author_id", "author", "paper_key", "doi", "source_author_names",
            "source_openalex_author_ids", "publication_year", "retraction_year",
            "retraction_lag_years", "match_type",
        ],
    )
    write_rows(FINAL_REASONS, final_reasons, ["openalex_author_id", "author", "reason", "unique_retracted_papers"])
    write_rows(FINAL_PUBLISHERS, final_publishers, ["openalex_author_id", "author", "publisher", "unique_retracted_papers"])

    review_rows = validation_rows + pair_rows
    review_rows.sort(
        key=lambda row: (
            0 if row["decision"] == "Automatic merge" else 1,
            0 if row["confidence"] == "Review" else 1,
            row["source_author"],
            row["openalex_author_id"],
        )
    )
    review_fields = [
        "review_type", "source_author", "openalex_author_id", "compared_openalex_author_id",
        "author", "compared_author", "exact_match_papers", "initial_match_papers",
        "compared_exact_match_papers", "compared_initial_match_papers", "shared_coauthors",
        "shared_coauthor_ids", "shared_institutions", "shared_affiliations", "shared_countries",
        "publication_year_gap", "orcid_relation", "decision", "confidence", "rationale",
    ]
    write_rows(REVIEW_CSV, review_rows, review_fields)

    merge_map = []
    for source_id in author_ids:
        canonical = canonical_by_id[source_id]
        members = groups_by_canonical[canonical]
        merge_map.append(
            {
                "source_openalex_author_id": source_id,
                "canonical_openalex_author_id": canonical,
                "canonical_author": display_by_id[canonical],
                "profile_group_size": len(members),
                "decision": group_evidence[canonical][0],
            }
        )
    write_rows(
        MERGE_MAP_CSV,
        merge_map,
        ["source_openalex_author_id", "canonical_openalex_author_id", "canonical_author", "profile_group_size", "decision"],
    )

    if RAW_UNRESOLVED.exists():
        unresolved_rows = read_rows(RAW_UNRESOLVED)
        write_rows(FINAL_UNRESOLVED, unresolved_rows, list(unresolved_rows[0]) if unresolved_rows else [])

    merged_groups = [members for members in groups_by_canonical.values() if len(members) > 1]
    top35 = final_leaderboard[:35]
    confirmed_initial_ids = {
        row["openalex_author_id"] for row in validation_rows if row["confidence"] == "High"
    }
    unresolved_initial_ids = {
        row["openalex_author_id"] for row in validation_rows if row["confidence"] == "Review"
    }
    final_summary = dict(raw_summary)
    final_summary.update(
        {
            "raw_openalex_authors": len(raw_leaderboard),
            "resolved_openalex_authors": len(final_leaderboard),
            "automatic_merge_groups": len(merged_groups),
            "openalex_profiles_merged": sum(len(members) - 1 for members in merged_groups),
            "confirmed_initial_profiles": len(confirmed_initial_ids),
            "unresolved_initial_profiles": len(unresolved_initial_ids),
            "top35_high_confidence": sum(row["match_confidence"] == "High" for row in top35),
            "top35_review_required": sum(row["match_confidence"] == "Review" for row in top35),
            "identity_review_date": date.today().isoformat(),
            "identity_review_method": "ORCID plus coauthor and affiliation continuity; no name-only automatic merges",
        }
    )
    write_rows(
        FINAL_SUMMARY,
        [{"metric": key, "value": value} for key, value in final_summary.items()],
        ["metric", "value"],
    )

    print(f"Raw OpenAlex profiles: {len(raw_leaderboard)}", flush=True)
    print(f"Automatic merge groups: {len(merged_groups)}", flush=True)
    print(f"Profiles merged: {sum(len(members) - 1 for members in merged_groups)}", flush=True)
    print(f"Reviewed leaderboard rows: {len(final_leaderboard)}", flush=True)
    print(f"Top 35 requiring review: {sum(row['match_confidence'] == 'Review' for row in top35)}", flush=True)


if __name__ == "__main__":
    main()
