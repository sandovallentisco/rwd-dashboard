"""Build OpenAlex publication denominators for the RWD country and publisher tables.

The snapshot uses a common window beginning in 1990. Country counts include a work
when at least one authorship has an institution in that country. Publisher counts
use the primary source's OpenAlex publisher lineage. Parent and historical imprint
IDs are combined with an OR filter, so each OpenAlex work is counted only once.
"""

from __future__ import annotations

import csv
import json
import time
from datetime import date
from pathlib import Path
from urllib.error import HTTPError, URLError
from urllib.parse import urlencode
from urllib.request import Request, urlopen


ROOT = Path(__file__).resolve().parents[1]
COUNTRY_OUTPUT = ROOT / "openalex_country_denominators.csv"
PUBLISHER_OUTPUT = ROOT / "openalex_publisher_denominators.csv"
START_DATE = "1990-01-01"
END_DATE = date.today().isoformat()
MAX_RETRIES = 5


COUNTRY_CODES = {
    "Australia": "AU",
    "Bangladesh": "BD",
    "Belgium": "BE",
    "Brazil": "BR",
    "Canada": "CA",
    "China": "CN",
    "Egypt": "EG",
    "France": "FR",
    "Germany": "DE",
    "Greece": "GR",
    "India": "IN",
    "Indonesia": "ID",
    "Iran": "IR",
    "Iraq": "IQ",
    "Israel": "IL",
    "Italy": "IT",
    "Japan": "JP",
    "Malaysia": "MY",
    "Mexico": "MX",
    "Netherlands": "NL",
    "Nigeria": "NG",
    "Pakistan": "PK",
    "Poland": "PL",
    "Russia": "RU",
    "Saudi Arabia": "SA",
    "Singapore": "SG",
    "South Africa": "ZA",
    "South Korea": "KR",
    "Spain": "ES",
    "Sweden": "SE",
    "Switzerland": "CH",
    "Taiwan": "TW",
    "Thailand": "TH",
    "Turkey": "TR",
    "United Arab Emirates": "AE",
    "United Kingdom": "GB",
    "United States": "US",
}


PUBLISHER_GROUPS = {
    "Hindawi (Wiley)": {
        "ids": ["P4310319869"],
        "note": "Hindawi Publishing Corporation, kept separate from Wiley",
    },
    "IEEE": {
        "ids": [
            "P4310319808",  # Institute of Electrical and Electronics Engineers parent
            "P4310320754",  # IEEE Microwave Theory and Techniques Society
            "P4310321340",  # IEEE Council on Superconductivity
            "P4322697011",  # IEEE historical fragment
            "P4322632812",  # IEEE Xplore fragment
        ],
        "note": "IEEE parent lineage plus unlinked OpenAlex society fragments",
    },
    "Springer Nature": {
        "ids": [
            "P4310319965",  # Springer Nature parent; includes linked Springer, Nature, BMC, Palgrave
            "P4404664013",  # Springer, Singapore fragment
            "P4404662779",  # Springer Vieweg fragment
            "P4310319702",  # historical Palgrave Macmillan fragment under Macmillan
        ],
        "note": "Springer Nature parent lineage plus unlinked Springer and Palgrave fragments",
    },
    "Elsevier": {
        "ids": [
            "P4310320990",  # Elsevier parent; includes linked Cell Press sources
            "P4310320146",  # Pergamon Press historical fragment
            "P4310320175",  # Elsevier Masson historical fragment
        ],
        "note": "Elsevier parent lineage plus historical Pergamon and Masson fragments",
    },
    "Wiley (excl. Hindawi)": {
        "ids": ["P4310320595"],
        "note": "Wiley lineage; Hindawi has a separate OpenAlex publisher identity",
    },
    "SAGE Publications": {
        "ids": ["P4310320017"],
        "note": "SAGE Publishing lineage",
    },
    "Taylor & Francis": {
        "ids": ["P4310320547", "P4310319847", "P4310320348", "P4310315328"],
        "note": "Taylor & Francis, Routledge, Dove Medical Press and NISC fragments",
    },
    "PLOS": {
        "ids": ["P4310315706"],
        "note": "Public Library of Science lineage",
    },
    "IOP Publishing": {
        "ids": ["P4310320083"],
        "note": "IOP Publishing lineage",
    },
    "Oxford University Press": {
        "ids": ["P4310311648"],
        "note": "Oxford University Press lineage",
    },
    "Spandidos": {
        "ids": ["P4310320412"],
        "note": "Spandidos Publishing lineage",
    },
    "EDP Sciences": {
        "ids": ["P4310319748"],
        "note": "EDP Sciences lineage",
    },
    "Frontiers": {
        "ids": ["P4310320527"],
        "note": "Frontiers Media lineage",
    },
    "Royal Society of Chemistry (RSC)": {
        "ids": ["P4310320556"],
        "note": "Royal Society of Chemistry lineage",
    },
    "MDPI": {
        "ids": ["P4310310987"],
        "note": "Multidisciplinary Digital Publishing Institute lineage",
    },
}


def load_api_key() -> str:
    for path in (ROOT / ".Renviron",):
        if not path.exists():
            continue
        for line in path.read_text(encoding="utf-8").splitlines():
            if line.startswith("OPENALEX_API_KEY="):
                return line.split("=", 1)[1].strip().strip('"').strip("'")
    raise RuntimeError("OPENALEX_API_KEY is not configured in .Renviron")


def api_get(params: dict[str, str], api_key: str) -> dict:
    query = dict(params)
    query["api_key"] = api_key
    request = Request(
        f"https://api.openalex.org/works?{urlencode(query)}",
        headers={"User-Agent": "RWD-Dashboard-Normalization/1.0"},
    )

    for attempt in range(MAX_RETRIES):
        try:
            with urlopen(request, timeout=60) as response:
                return json.loads(response.read().decode("utf-8"))
        except HTTPError as error:
            if error.code not in {429, 500, 502, 503, 504} or attempt == MAX_RETRIES - 1:
                raise
        except (TimeoutError, URLError):
            if attempt == MAX_RETRIES - 1:
                raise
        time.sleep(2 ** attempt)

    raise RuntimeError("OpenAlex request failed after retries")


def works_count(filter_value: str, api_key: str) -> int:
    payload = api_get(
        {
            "filter": filter_value,
            "per_page": "1",
            "select": "id",
        },
        api_key,
    )
    return int((payload.get("meta") or {}).get("count") or 0)


def write_rows(path: Path, rows: list[dict], fields: list[str]) -> None:
    with path.open("w", encoding="utf-8", newline="") as handle:
        writer = csv.DictWriter(handle, fieldnames=fields)
        writer.writeheader()
        writer.writerows(rows)


def main() -> None:
    api_key = load_api_key()
    retrieved_date = date.today().isoformat()
    common_dates = f"from_publication_date:{START_DATE},to_publication_date:{END_DATE}"

    country_rows = []
    for country, code in COUNTRY_CODES.items():
        count = works_count(
            f"authorships.institutions.country_code:{code},{common_dates}",
            api_key,
        )
        country_rows.append(
            {
                "Country": country,
                "CountryCode": code,
                "OpenAlexWorks": count,
                "PeriodStart": START_DATE,
                "PeriodEnd": END_DATE,
                "RetrievedDate": retrieved_date,
                "Method": "At least one authorship institution in country",
            }
        )

    publisher_rows = []
    for publisher_group, mapping in PUBLISHER_GROUPS.items():
        publisher_ids = mapping["ids"]
        count = works_count(
            f"primary_location.source.publisher_lineage:{'|'.join(publisher_ids)},{common_dates}",
            api_key,
        )
        publisher_rows.append(
            {
                "PublisherGroup": publisher_group,
                "OpenAlexPublisherIDs": ";".join(publisher_ids),
                "OpenAlexWorks": count,
                "PeriodStart": START_DATE,
                "PeriodEnd": END_DATE,
                "RetrievedDate": retrieved_date,
                "Method": "Primary source publisher lineage",
                "MappingNote": mapping["note"],
            }
        )

    write_rows(
        COUNTRY_OUTPUT,
        country_rows,
        ["Country", "CountryCode", "OpenAlexWorks", "PeriodStart", "PeriodEnd", "RetrievedDate", "Method"],
    )
    write_rows(
        PUBLISHER_OUTPUT,
        publisher_rows,
        [
            "PublisherGroup", "OpenAlexPublisherIDs", "OpenAlexWorks", "PeriodStart",
            "PeriodEnd", "RetrievedDate", "Method", "MappingNote",
        ],
    )

    print(f"Wrote {len(country_rows)} country denominators and {len(publisher_rows)} publisher denominators.")


if __name__ == "__main__":
    main()
