"""Read-only, limited standards screening for the synthetic ABC101 CSV package.

Run from any directory: python qc/validate_conformance_v1_0.py
Optional XSD: python qc/validate_conformance_v1_0.py --schema PATH_TO_define2-1-0.xsd

This is not an implementation of CDISC or FDA rule workbooks. Its SC-* IDs are
local screening IDs, not published conformance-rule identifiers. It never edits
datasets or metadata. Only the versioned Markdown findings report is written.
"""

import argparse
import contextlib
import csv
from datetime import date
from io import StringIO
from pathlib import Path
import re
import sys
import xml.etree.ElementTree as ET

sys.dont_write_bytecode = True  # Importing the existing validator must not create qc/__pycache__.

ROOT = Path(__file__).resolve().parents[1]
REPORT = ROOT / "output/qc/ABC101_synthetic_conformance_findings_v1.0.md"
ODM = "http://www.cdisc.org/ns/odm/v1.3"
DEFINE = "http://www.cdisc.org/ns/def/v2.1"
LAYERS = {"sdtm": ("DM", "AE", "LB", "VS", "EX"),
          "adam": ("ADSL", "ADAE", "ADLB", "ADVS")}
SOURCES = [
    ("FDA Data Standards Catalog v11.0, March 2025 (version selection for assumed 2025-01-01 start)",
     "https://www.fda.gov/regulatory-information/search-fda-guidance-documents/data-standards-catalog"),
    ("CDISC SDTM v2.0 / SDTMIG v3.4; SDTM/SDTMIG Conformance Rules v2.0, 2021-11-29",
     "https://www.cdisc.org/standards/foundational/sdtmig/sdtm-and-sdtmig-conformance-rules-v2-0"),
    ("CDISC ADaM v2.1 / ADaMIG v1.3; ADaM Conformance Rules v5.0, 2023-10-06",
     "https://www.cdisc.org/standards/foundational/adam/adam-conformance-rules-v5-0"),
    ("CDISC Define-XML v2.1; Conformance Rules for Define-XML v2.1, 2021-03-30",
     "https://www.cdisc.org/standards/foundational/define-xml/conformance-rules-define-xml-v2-1"),
    ("CDISC Define-XML v2.1.8 schema package, 2024-12-20 (package-58 update; not locally available for this run)",
     "https://www.cdisc.org/standards/data-exchange/define-xml/define-xml-v2-1-8"),
    ("CDISC terminology package 58, 2024-09-27 (selected target; not loaded)",
     "https://www.cdisc.org/standards/terminology/controlled-terminology"),
    ("FDA Business Rules v1.5, May 2019; FDA Validator Rules v1.6, December 2022",
     "https://www.fda.gov/industry/fda-data-standards-advisory-board/study-data-standards-resources"),
    ("FDA Study Data Technical Conformance Guide, October 2026 (current guidance at this report date)",
     "https://www.fda.gov/media/153632/download"),
]


def read_csv(layer, domain):
    path = ROOT / "data" / layer / f"{domain.lower()}.csv"
    with path.open(newline="", encoding="utf-8-sig") as handle:
        reader = csv.DictReader(handle)
        rows = list(reader)
    if not reader.fieldnames or not rows:
        raise ValueError(f"Missing header or records: {path}")
    return reader.fieldnames, rows


def check(check_id, area, description, fn, results):
    try:
        detail = fn()
        results.append((check_id, area, "PASS", description, detail))
    except Exception as exc:
        results.append((check_id, area, "FINDING", description,
                        f"{type(exc).__name__}: {exc}"))


def require(condition, message):
    if not condition:
        raise ValueError(message)


def main(schema):
    results = []
    data = {(layer, domain): read_csv(layer, domain)
            for layer, domains in LAYERS.items() for domain in domains}

    def naming():
        total = 0
        for (layer, domain), (names, _) in data.items():
            require(len(domain) <= 8 and re.fullmatch(r"[A-Z][A-Z0-9]*", domain),
                    f"{layer}/{domain}: dataset name")
            require(len(names) == len(set(names)), f"{layer}/{domain}: duplicate variable name")
            for name in names:
                require(len(name) <= 8 and re.fullmatch(r"[A-Z][A-Z0-9]*", name),
                        f"{layer}/{domain}: variable {name}")
                total += 1
        return f"9 CSV names and {total} column names fit the basic 8-character uppercase pattern."

    check("SC-01", "CSV structure", "Basic dataset/variable name screening", naming, results)

    def identifiers():
        subjects = {r["USUBJID"] for r in data["sdtm", "DM"][1]}
        require(len(subjects) == 500, "DM subject IDs are not 500 distinct values")
        for (layer, domain), (_, rows) in data.items():
            require(all(r.get("STUDYID") == "ABC101" and r.get("USUBJID")
                        for r in rows), f"{layer}/{domain}: missing/mismatched identifiers")
            require({r["USUBJID"] for r in rows} <= subjects,
                    f"{layer}/{domain}: subject not in DM")
            if layer == "sdtm":
                require(all(r.get("DOMAIN") == domain for r in rows),
                        f"{domain}: DOMAIN mismatch")
        return "STUDYID, USUBJID, DOMAIN (SDTM), and DM subject links agree."

    check("SC-02", "SDTM/ADaM", "Identifiers and subject links", identifiers, results)

    def keys():
        key_names = {"DM": ("USUBJID",), "AE": ("USUBJID", "AESEQ"),
                     "LB": ("USUBJID", "LBSEQ"), "VS": ("USUBJID", "VSSEQ"),
                     "EX": ("USUBJID", "EXSEQ"), "ADSL": ("USUBJID",),
                     "ADAE": ("USUBJID", "ASEQ"), "ADLB": ("USUBJID", "ASEQ"),
                     "ADVS": ("USUBJID", "ASEQ")}
        for (layer, domain), (_, rows) in data.items():
            keys = [tuple(r.get(k, "") for k in key_names[domain]) for r in rows]
            require(all(all(v for v in key) for key in keys),
                    f"{layer}/{domain}: blank key")
            require(len(keys) == len(set(keys)), f"{layer}/{domain}: duplicate key")
        return "All 9 declared dataset keys are populated and unique."

    check("SC-03", "SDTM/ADaM", "Declared record-key uniqueness", keys, results)

    def dates():
        tested = 0
        for domain in LAYERS["sdtm"]:
            names, rows = data["sdtm", domain]
            for name in names:
                if name.endswith("DTC"):
                    for row in rows:
                        value = row[name]
                        if value:
                            require(bool(re.fullmatch(r"\d{4}-\d{2}-\d{2}", value)),
                                    f"{domain}.{name}: {value!r} is not a full ISO date")
                            date.fromisoformat(value)
                            tested += 1
        return f"{tested:,} populated SDTM --DTC values parse as complete ISO calendar dates."

    check("SC-04", "SDTM", "Current full-date syntax and calendar validity", dates, results)

    def test_codes():
        for domain in ("LB", "VS"):
            rows = data["sdtm", domain][1]
            code, label = ("LBTESTCD", "LBTEST") if domain == "LB" else ("VSTESTCD", "VSTEST")
            require(all(r[code] and len(r[code]) <= 8 and r[label] and len(r[label]) <= 40
                        for r in rows), f"{domain}: blank or overlength test code/name")
        return "LB/VS test codes are nonblank and <=8 characters; test names are nonblank and <=40."

    check("SC-05", "SDTM", "Findings test-code/name length screening", test_codes, results)

    def trace():
        for domain, source, sequence in (("ADAE", "AE", "AESEQ"),
                                         ("ADLB", "LB", "LBSEQ"),
                                         ("ADVS", "VS", "VSSEQ")):
            source_keys = {(r["USUBJID"], r[sequence]) for r in data["sdtm", source][1]}
            rows = data["adam", domain][1]
            require(len(rows) == len(source_keys), f"{domain}: row count differs from {source}")
            for r in rows:
                require(r["SRCDOM"] == source and r["SRCVAR"] == sequence and
                        r["SRCSEQ"] == r["ASEQ"] == r[sequence] and
                        (r["USUBJID"], r["SRCSEQ"]) in source_keys,
                        f"{domain}: broken source sequence at {r['USUBJID']}/{r['ASEQ']}")
        return "ADAE/ADLB/ADVS retain every source row with matching SRCDOM/SRCVAR/SRCSEQ."

    check("SC-06", "ADaM", "Record-level SDTM source traceability", trace, results)

    def metadata():
        sys.path.insert(0, str(ROOT / "qc"))
        from validate_define_xml import check as xml_check, SETS
        output = StringIO()
        with contextlib.redirect_stdout(output):
            for layer, details in SETS.items():
                xml_check(layer, *details, schema)
        return "Existing Define validator passed: 9 CSV links, 182 ordered items, origins/methods; " + (
            "supplied XSD passed." if schema else "XSD not rerun (no schema supplied).")

    check("SC-07", "Define-XML", "XML-to-specification/CSV references and optional XSD", metadata, results)

    def context():
        for layer, standard, version in (("sdtm", "SDTMIG", "3.4"),
                                         ("adam", "ADaMIG", "1.3")):
            path = ROOT / "metadata/v1.0" / layer / "define.xml"
            root = ET.parse(path).getroot()
            require(root.get(f"{{{DEFINE}}}Context") == "Other", f"{layer}: context")
            mdv = root.find(f"{{{ODM}}}Study/{{{ODM}}}MetaDataVersion")
            require(mdv is not None and mdv.get(f"{{{DEFINE}}}DefineVersion") == "2.1.0",
                    f"{layer}: Define version")
            std = mdv.find(f"{{{DEFINE}}}Standards/{{{DEFINE}}}Standard")
            require(std is not None and std.get("Name") == standard and
                    std.get("Version") == version, f"{layer}: IG version")
        return "Both XMLs declare Define 2.1.0, selected IG versions, and Context=Other."

    check("SC-08", "Define-XML", "Declared XML and IG versions/context", context, results)

    # These are observed package gaps or limitations, not an official rule-run count.
    gaps = [
        ("G-01", "Submission transport", "The nine datasets exist only as CSVs; no XPT files or physical transport attributes were validated.", "OPEN"),
        ("G-02", "Trial design/start date", "No TS dataset or actual protocol/start-date record exists; the 2025-01-01 date is an assumption.", "OPEN"),
        ("G-03", "Controlled terminology", "Package 58 codelist IDs and allowed values are not represented in the XML; terminology cannot be checked against the selected package.", "NOT TESTED"),
        ("G-04", "Medical coding", "AEDECOD is the uppercased source term; no MedDRA dictionary/version or coded-term validation exists.", "OPEN"),
        ("G-05", "Origins and source documents", "There is no real aCRF, protocol, source system, or collection-time evidence to verify origin and predose status.", "NOT TESTED"),
        ("G-06", "Analysis metadata", "ADaM XML has no analysis-results metadata or SAP-backed endpoint definitions.", "OPEN"),
        ("G-07", "Complete published rule execution", "The CDISC/FDA rule workbooks and a configured standards validator were not run; no official rule IDs or pass rate are asserted.", "NOT TESTED"),
        ("G-08", "Core/expected/permissible and value-level rules", "The local specifications and XML do not classify all variables by published core status or define value-level metadata; the relevant formal rules were not evaluated.", "NOT TESTED"),
        ("G-09", "FDA technical rejection criteria", "There is no eCTD submission, study tagging file, or ts.xpt; receipt-level checks cannot run against this checkout.", "NOT TESTED"),
    ]
    xml_sdtm = ET.parse(ROOT / "metadata/v1.0/sdtm/define.xml").getroot()
    xml_adam = ET.parse(ROOT / "metadata/v1.0/adam/define.xml").getroot()
    require(not list((ROOT / "data").rglob("*.xpt")), "Unexpected XPT: update G-01")
    require(not (ROOT / "data/sdtm/ts.csv").exists(), "TS now exists: update G-02")
    require(not xml_sdtm.findall(f".//{{{ODM}}}CodeList") and
            not xml_adam.findall(f".//{{{ODM}}}CodeList"), "Codelists now exist: update G-03")
    require(all(r["AEDECOD"] == r["AETERM"].upper()
                for r in data["sdtm", "AE"][1]), "AEDECOD changed: update G-04")
    require(not xml_adam.findall(f".//{{{DEFINE}}}AnalysisResultDisplays"),
            "ARM now exists: update G-06")

    passed = sum(r[2] == "PASS" for r in results)
    failed = len(results) - passed
    lines = [
        "# ABC101 synthetic conformance screening findings — v1.0", "",
        "> **SYNTHETIC REVIEW DEMONSTRATION.** This is a limited, read-only standards screening of current CSVs and Define-XML. It is not a CDISC CORE, FDA business/validator-rule, XPT, or submission conformance certificate.", "",
        "| Control | Value |", "| --- | --- |",
        "| Version / date | 1.0 / 2026-10-13 |",
        "| Program | [validate_conformance_v1_0.py](../../qc/validate_conformance_v1_0.py) |",
        "| Inputs | 5 SDTM CSVs, 4 ADaM CSVs, 2 Define-XML files, committed specifications |",
        f"| Limited executable screening | {passed}/{len(results)} PASS; {failed} failed check(s) |",
        f"| Open/untested areas | {len(gaps)} separately listed below |",
        "| Full standards conformance | **NOT ASSESSED** |", "",
        "## Rule sources and version boundaries", "",
        "The [study-design assumptions](../../docs/ABC101_synthetic_study_design_and_standards_v1.0.md) select standards using an assumed 2025-01-01 start. FDA's March 2025 catalog is a retrospective support-date reference, not a snapshot from that start date. The October 2026 technical guide is the current review-process guidance at this report date, not a replacement for that version-selection basis. Relevant sources:", "",
    ]
    lines += [f"- [{label}]({url})." for label, url in SOURCES]
    lines += [
        "", "The CDISC conformance-rule files require sign-in on their publication pages and are not in this checkout. FDA's public resources identify the Business/Validator rule versions, but their spreadsheet rules were not executed here. No published rule ID is assigned to a local check below. Rule applicability can also depend on study design, dataset class, metadata, and submission context.",
        "", "## Executable screening (local IDs only)", "",
        "| ID | Area | Result | Check | Evidence |", "| --- | --- | --- | --- | --- |",
    ]
    for cid, area, status, description, detail in results:
        lines.append(f"| {cid} | {area} | **{status}** | {description} | {detail} |")
    lines += [
        "", "These checks inspect standard-facing properties that the current files expose. SC-02, SC-03, and SC-06 overlap internal QC; passing them is not an additional standards-conformance determination. SC-07 reuses the existing [Define validator](../../qc/validate_define_xml.py). When no `--schema` is supplied, XML-to-data integrity is checked, but XSD validation is **not rerun**. A prior structural XSD result and its older terminology-enumeration limit are documented in the [Define preparation report](../../docs/ABC101_synthetic_define_xml_preparation_v1.0.md).",
        "", "## Observed gaps and untestable areas", "",
        "| ID | Area | Disposition | Finding or reason |", "| --- | --- | --- | --- |",
    ]
    for gid, area, detail, status in gaps:
        lines.append(f"| {gid} | {area} | **{status}** | {detail} |")
    lines += [
        "", "`OPEN` means a visible package gap or known limitation. `NOT TESTED` means source inputs, terminology data, business context, or the official rules/validator are unavailable. These dispositions are not official FDA/CDISC rule severities. The generated metadata declares `def:Context=Other` and points to CSVs, so an XML reference PASS does not make it submission-ready. The current [SDTM specifications](../../docs/ABC101_synthetic_SDTM_dataset_specifications_v1.0.md) and [ADaM specifications](../../docs/ABC101_synthetic_ADaM_dataset_specifications_v1.0.md) describe project mappings, not full core/expected/permissible-variable conformance.",
        "", "## Separate internal QC evidence", "",
        "The existing [SDTM QC](sdtm_validation_report.md) (45/45), [ADaM QC](adam_validation_report.md) (52/52), [final TLF QC](tlf_validation_report.md) (36/36 for the currently reviewed PNG), [specification mapping check](../../docs/ABC101_dataset_spec_validation_v1.0.md), and [Define-XML reference check](../../docs/ABC101_synthetic_define_xml_preparation_v1.0.md) establish consistency of this synthetic pipeline. They do not certify the published conformance rule sets, controlled terminology, or submission transport.",
        "", "## Reproduction and next boundary", "",
        "Run `python qc/validate_conformance_v1_0.py` from any directory. Supply `--schema PATH` only for a locally available Define-XML 2.1 entry XSD with its dependencies; note that a structural XSD pass does not check package-58 terminology. The program writes this findings report and does not change datasets, metadata, TLFs, or reviewer guides. Full CDISC/FDA rule execution, transport conversion, and a Quarto report remain outside this step.", "",
    ]
    REPORT.write_text("\n".join(lines), encoding="utf-8")
    print(f"Limited screening: {passed}/{len(results)} PASS; {failed} failed check(s); "
          f"{len(gaps)} open/untested areas. Report: {REPORT}")
    return 1 if failed else 0


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--schema", type=Path, help="Local Define-XML 2.1 entry XSD")
    args = parser.parse_args()
    if args.schema and not args.schema.is_file():
        parser.error(f"Schema not found: {args.schema}")
    raise SystemExit(main(args.schema))
