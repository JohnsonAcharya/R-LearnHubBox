"""Read-only, cell-level check of ABC101 dataset specifications against sources.

Run from the repository root: python qc/validate_dataset_specs.py
Checks every documented output variable for every current SDTM/ADaM row.
Only reads files; never writes datasets or reports.
"""

import csv
from collections import defaultdict
from datetime import date, timedelta
from pathlib import Path
import re

ROOT = Path(__file__).resolve().parents[1]
SDTM_SPEC = ROOT / "docs/ABC101_synthetic_SDTM_dataset_specifications_v1.0.md"
ADAM_SPEC = ROOT / "docs/ABC101_synthetic_ADaM_dataset_specifications_v1.0.md"
ARM_CODE = {"Placebo": "PBO", "Drug 10 mg": "DRUG10", "Drug 20 mg": "DRUG20"}
TRT_NUMBER = {"Placebo": 0, "Drug 10 mg": 1, "Drug 20 mg": 2}
VISIT_NUMBER = {"Baseline": 1, "Week 2": 2, "Week 4": 3, "Week 8": 4, "Week 12": 5}
LB_CATEGORY = {"ALT": "CHEMISTRY", "AST": "CHEMISTRY", "CREAT": "CHEMISTRY", "HGB": "HEMATOLOGY"}
LB_UNIT = {"ALT": "U/L", "AST": "U/L", "CREAT": "mg/dL", "HGB": "g/dL"}
VS_UNIT = {"SYSBP": "mmHg", "DIABP": "mmHg", "PULSE": "bpm", "TEMP": "C", "WEIGHT": "kg"}


def read_csv(path):
    with (ROOT / path).open(newline="", encoding="utf-8-sig") as handle:
        reader = csv.DictReader(handle)
        return reader.fieldnames, list(reader)


def index(rows, fields):
    result = {}
    for row in rows:
        key = tuple(row[field] for field in fields)
        assert key not in result, f"duplicate source key {key}"
        result[key] = row
    return result


def day(value, reference):
    diff = (date.fromisoformat(value) - date.fromisoformat(reference)).days
    return diff + 1 if diff >= 0 else diff


def epoch(value, start, end):
    if value < start:
        return "PRE-TREATMENT"
    return "TREATMENT" if value <= end else "FOLLOW-UP"


def spec_columns(path, domain):
    content = path.read_text(encoding="utf-8")
    match = re.search(rf"^## {domain} —[^\n]*\n(.*?)(?=^## |\Z)", content, re.M | re.S)
    assert match, f"missing {domain} section in {path.name}"
    columns = []
    for line in match.group(1).splitlines():
        parts = [part.strip() for part in line.split("|")[1:-1]]
        if len(parts) == 4 and parts[1] in {"C", "N", "ISO", "Date"}:
            columns.append((parts[0], parts[1]))
    assert columns, f"no variable table for {domain}"
    assert len(columns) == len({name for name, _ in columns}), f"duplicate spec variable in {domain}"
    return columns


def equal(actual, expected, data_type):
    if expected == "":
        return actual == ""
    if data_type == "N":
        try:
            return abs(float(actual) - float(expected)) <= 1e-8
        except ValueError:
            return False
    return actual == str(expected)


def verify(layer, domain, source_rows, source_key, expected, spec_path):
    columns, output = read_csv(f"data/{layer}/{domain.lower()}.csv")
    specified = spec_columns(spec_path, domain)
    assert columns == [name for name, _ in specified], f"{domain}: CSV/spec columns or order differ"
    actual = index(output, source_key)
    source = index(source_rows, source_key)
    assert actual.keys() == source.keys(), f"{domain}: source/output key sets differ"
    errors = []
    for key, record in source.items():
        projected = expected(record)
        assert set(projected) == set(columns), f"{domain}: expected map does not cover every variable"
        for name, data_type in specified:
            if not equal(actual[key][name], projected[name], data_type):
                errors.append(f"{key} {name}: {actual[key][name]!r} != {projected[name]!r}")
                if len(errors) >= 10:
                    break
        if len(errors) >= 10:
            break
    assert not errors, f"{domain}: mapping failures\n" + "\n".join(errors)
    print(f"PASS {domain}: {len(output):,} rows x {len(columns)} mapped variables; keys and column order")
    return len(output), len(columns)


raw = {}
for name, path in {
    "DM": "data/raw/demographics.csv",
    "AE": "data/raw/adverse_events.csv",
    "LB": "data/raw/labs.csv",
    "VS": "data/raw/vital_signs.csv",
    "EX": "data/raw/exposure.csv",
}.items():
    _, raw[name] = read_csv(path)

raw_by_subject = {name: index(raw[name], ("USUBJID",)) for name in ("DM", "EX")}
activity_start, activity_end = defaultdict(list), defaultdict(list)
for record in raw["EX"]:
    activity_start[record["USUBJID"]].append(record["EXSTDTC"])
    activity_end[record["USUBJID"]].append(record["EXENDTC"])
for name, start_field, end_field in (
    ("AE", "AESTDTC", "AEENDTC"),
    ("LB", "LBDTC", "LBDTC"),
    ("VS", "VSDTC", "VSDTC"),
):
    for record in raw[name]:
        activity_start[record["USUBJID"]].append(record[start_field])
        activity_end[record["USUBJID"]].append(record[end_field])


def expected_dm(r):
    sid = r["USUBJID"]
    ex = raw_by_subject["EX"][(sid,)]
    return dict(
        STUDYID="ABC101", DOMAIN="DM", USUBJID=sid,
        SUBJID=re.sub(r"^ABC-", "", sid),
        RFSTDTC=min(activity_start[sid]), RFENDTC=max(activity_end[sid]),
        RFXSTDTC=ex["EXSTDTC"], RFXENDTC=ex["EXENDTC"],
        AGE=r["AGE"], AGEU="YEARS", SEX=r["SEX"],
        ARMCD=ARM_CODE[r["TRT01P"]], ARM=r["TRT01P"],
        ACTARMCD=ARM_CODE[ex["EXTRT"]], ACTARM=ex["EXTRT"],
    )


counts = []
counts.append(verify("sdtm", "DM", raw["DM"], ("USUBJID",), expected_dm, SDTM_SPEC))
_, dm_rows = read_csv("data/sdtm/dm.csv")
dm = index(dm_rows, ("USUBJID",))


def expected_ae(r):
    d = dm[(r["USUBJID"],)]
    return dict(
        STUDYID="ABC101", DOMAIN="AE", USUBJID=r["USUBJID"],
        AESEQ=r["AESEQ"], AETERM=r["AETERM"], AEDECOD=r["AETERM"].upper(),
        AESEV=r["AESEV"].upper(), AESER=r["AESER"], AEOUT=r["AEOUT"],
        AESTDTC=r["AESTDTC"], AEENDTC=r["AEENDTC"],
        AESTDY=day(r["AESTDTC"], d["RFSTDTC"]),
        AEENDY=day(r["AEENDTC"], d["RFSTDTC"]),
        EPOCH=("TREATMENT" if d["RFXSTDTC"] <= r["AESTDTC"] <= d["RFXENDTC"] else "FOLLOW-UP"),
    )


counts.append(verify("sdtm", "AE", raw["AE"], ("USUBJID", "AESEQ"), expected_ae, SDTM_SPEC))


def expected_finding(r, prefix):
    d = dm[(r["USUBJID"],)]
    test = r[f"{prefix}TESTCD"]
    result = r[f"{prefix}ORRES"]
    value = dict(
        STUDYID="ABC101", DOMAIN=prefix, USUBJID=r["USUBJID"],
        VISITNUM=VISIT_NUMBER[r["VISIT"]], VISIT=r["VISIT"],
        EPOCH=epoch(r[f"{prefix}DTC"], d["RFXSTDTC"], d["RFXENDTC"]),
    )
    value[f"{prefix}SEQ"] = r[f"{prefix}SEQ"]
    value[f"{prefix}TESTCD"] = test
    value[f"{prefix}TEST"] = r[f"{prefix}TEST"]
    value[f"{prefix}CAT"] = LB_CATEGORY[test] if prefix == "LB" else "VITAL SIGNS"
    value[f"{prefix}ORRES"] = result
    value[f"{prefix}ORRESU"] = r[f"{prefix}ORRESU"]
    value[f"{prefix}STRESC"] = result
    value[f"{prefix}STRESN"] = float(result)
    value[f"{prefix}STRESU"] = (LB_UNIT if prefix == "LB" else VS_UNIT)[test]
    value[f"{prefix}BLFL"] = "Y" if r["VISIT"] == "Baseline" else ""
    value[f"{prefix}DTC"] = r[f"{prefix}DTC"]
    value[f"{prefix}DY"] = day(r[f"{prefix}DTC"], d["RFSTDTC"])
    return value


for name in ("LB", "VS"):
    counts.append(verify(
        "sdtm", name, raw[name], ("USUBJID", f"{name}SEQ"),
        lambda r, name=name: expected_finding(r, name), SDTM_SPEC,
    ))


def expected_ex(r):
    d = dm[(r["USUBJID"],)]
    return dict(
        STUDYID="ABC101", DOMAIN="EX", USUBJID=r["USUBJID"],
        EXSEQ=r["EXSEQ"], EXTRT=r["EXTRT"], EXCAT="STUDY TREATMENT",
        EXDOSE=r["EXDOSE"], EXDOSU=r["EXDOSU"].upper(),
        EXDOSFRQ=r["EXDOSFRQ"], EXSTDTC=r["EXSTDTC"], EXENDTC=r["EXENDTC"],
        EXSTDY=day(r["EXSTDTC"], d["RFSTDTC"]),
        EXENDY=day(r["EXENDTC"], d["RFSTDTC"]), EPOCH="TREATMENT",
    )


counts.append(verify("sdtm", "EX", raw["EX"], ("USUBJID", "EXSEQ"), expected_ex, SDTM_SPEC))
_, ex_rows = read_csv("data/sdtm/ex.csv")
ex = index(ex_rows, ("USUBJID",))


def expected_adsl(d):
    e = ex[(d["USUBJID"],)]
    return dict(
        STUDYID=d["STUDYID"], USUBJID=d["USUBJID"], SUBJID=d["SUBJID"],
        AGE=d["AGE"], AGEU=d["AGEU"], SEX=d["SEX"],
        TRT01P=d["ARM"], TRT01PN=TRT_NUMBER[d["ARM"]],
        TRT01A=d["ACTARM"], TRT01AN=TRT_NUMBER[d["ACTARM"]],
        TRTSDT=e["EXSTDTC"], TRTEDT=e["EXENDTC"],
        TRTDURD=(date.fromisoformat(e["EXENDTC"]) - date.fromisoformat(e["EXSTDTC"])).days + 1,
        ITTFL="Y", SAFFL="Y",
    )


counts.append(verify("adam", "ADSL", dm_rows, ("USUBJID",), expected_adsl, ADAM_SPEC))
_, adsl_rows = read_csv("data/adam/adsl.csv")
adsl = index(adsl_rows, ("USUBJID",))
_, ae_rows = read_csv("data/sdtm/ae.csv")


def subject_fields(sid, include_itt=False):
    a = adsl[(sid,)]
    fields = {name: a[name] for name in (
        "SUBJID", "TRT01P", "TRT01PN", "TRT01A", "TRT01AN",
        "TRTSDT", "TRTEDT", "SAFFL",
    )}
    if include_itt:
        fields["ITTFL"] = a["ITTFL"]
    return fields


def expected_adae(r):
    a = adsl[(r["USUBJID"],)]
    eligible_end = date.fromisoformat(a["TRTEDT"]) + timedelta(days=30)
    return dict(
        STUDYID=r["STUDYID"], USUBJID=r["USUBJID"],
        **subject_fields(r["USUBJID"]),
        ASEQ=r["AESEQ"], AESEQ=r["AESEQ"],
        AETERM=r["AETERM"], AEDECOD=r["AEDECOD"], AESEV=r["AESEV"],
        AESER=r["AESER"], AEOUT=r["AEOUT"],
        AESTDTC=r["AESTDTC"], AEENDTC=r["AEENDTC"],
        ASTDT=r["AESTDTC"], AENDT=r["AEENDTC"],
        ASTDY=day(r["AESTDTC"], a["TRTSDT"]),
        AENDY=day(r["AEENDTC"], a["TRTSDT"]), EPOCH=r["EPOCH"],
        TRTEMFL=("Y" if a["TRTSDT"] <= r["AESTDTC"] <= eligible_end.isoformat() else ""),
        SRCDOM="AE", SRCVAR="AESEQ", SRCSEQ=r["AESEQ"],
    )


counts.append(verify("adam", "ADAE", ae_rows, ("USUBJID", "AESEQ"), expected_adae, ADAM_SPEC))


def verify_analysis_finding(name):
    prefix = name[2:]
    _, source_rows = read_csv(f"data/sdtm/{prefix.lower()}.csv")
    test_field = f"{prefix}TESTCD"
    baseline = index(
        [r for r in source_rows if r[f"{prefix}BLFL"] == "Y"],
        ("USUBJID", test_field),
    )
    assert len(baseline) == len({(r["USUBJID"], r[test_field]) for r in source_rows})

    def expected(r):
        sid = r["USUBJID"]
        b = baseline[(sid, r[test_field])]
        value = float(r[f"{prefix}STRESN"])
        base = float(b[f"{prefix}STRESN"])
        is_baseline = r[f"{prefix}BLFL"] == "Y"
        change = "" if is_baseline else round(value - base, 4)
        percent = "" if is_baseline or base == 0 else round(100 * change / base, 4)
        return dict(
            STUDYID=r["STUDYID"], USUBJID=sid,
            **subject_fields(sid, include_itt=True),
            ASEQ=r[f"{prefix}SEQ"], **{f"{prefix}SEQ": r[f"{prefix}SEQ"]},
            PARAMCD=r[test_field], PARAM=r[f"{prefix}TEST"], PARCAT1=r[f"{prefix}CAT"],
            AVAL=value, AVALU=r[f"{prefix}STRESU"],
            ADT=r[f"{prefix}DTC"], ADY=r[f"{prefix}DY"],
            AVISIT=r["VISIT"], AVISITN=r["VISITNUM"],
            ABLFL=r[f"{prefix}BLFL"], BLSEQ=b[f"{prefix}SEQ"],
            BASE=base, CHG=change, PCHG=percent,
            SRCDOM=prefix, SRCVAR=f"{prefix}SEQ", SRCSEQ=r[f"{prefix}SEQ"],
        )

    return verify(
        "adam", name, source_rows, ("USUBJID", f"{prefix}SEQ"),
        expected, ADAM_SPEC,
    )


for name in ("ADLB", "ADVS"):
    counts.append(verify_analysis_finding(name))

assert sum(columns for _, columns in counts) == 182
print(f"PASS all 9 datasets: 182/182 variable mappings over {sum(rows for rows, _ in counts):,} output rows.")
