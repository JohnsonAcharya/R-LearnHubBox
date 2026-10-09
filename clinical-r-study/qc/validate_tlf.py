"""Independent final TLF QC. Run: python qc/validate_tlf.py.

Uses Python's standard library; does not call or source production R programs.
Recalculates from ADaM CSVs and exits nonzero if any check fails.
--root permits validation of a disposable project copy.
"""

import argparse
import csv
import hashlib
import math
import re
import statistics
import struct
import zlib
from datetime import date, timedelta
from pathlib import Path

ARMS = ["Placebo", "Drug 10 mg", "Drug 20 mg"]
VISITS = [("1", "Baseline"), ("2", "Week 2"), ("3", "Week 4"),
          ("4", "Week 8"), ("5", "Week 12")]


def require(condition, message):
    if not condition:
        raise ValueError(message)


def read_csv(path):
    with path.open(newline="", encoding="utf-8-sig") as stream:
        reader = csv.DictReader(stream)
        return reader.fieldnames, list(reader)


def png_content_evidence(image):
    """Count treatment pixels in the plot and dark pixels in the count grid.

    The R Cairo figure is an 8-bit, non-interlaced RGB PNG. Decode it with
    standard-library modules so a valid but blank PNG cannot pass artifact QC.
    """
    require(image[:8] == b"\x89PNG\r\n\x1a\n", "Invalid PNG signature")
    offset = 8
    compressed = bytearray()
    dimensions = None
    while offset < len(image):
        require(offset + 12 <= len(image), "Truncated PNG chunk")
        length = struct.unpack_from(">I", image, offset)[0]
        kind = image[offset + 4:offset + 8]
        payload = image[offset + 8:offset + 8 + length]
        require(len(payload) == length, "Truncated PNG payload")
        stored_crc = struct.unpack_from(">I", image, offset + 8 + length)[0]
        require(zlib.crc32(kind + payload) == stored_crc, "PNG chunk CRC mismatch")
        if kind == b"IHDR":
            dimensions = struct.unpack(">IIBBBBB", payload)
        elif kind == b"IDAT":
            compressed.extend(payload)
        elif kind == b"IEND":
            break
        offset += 12 + length

    require(dimensions == (1800, 1100, 8, 2, 0, 0, 0),
            "Expected an 1800 x 1100 non-interlaced RGB PNG")
    width, height = dimensions[:2]
    stride = width * 3
    pixels = zlib.decompress(compressed)
    require(len(pixels) == height * (stride + 1), "Wrong decompressed PNG size")
    target_colors = {"Placebo": (68, 68, 68),
                     "Drug 10 mg": (0, 114, 178),
                     "Drug 20 mg": (213, 94, 0)}
    treatment_pixels = {arm: 0 for arm in target_colors}
    count_grid_pixels = 0
    previous = bytearray(stride)
    position = 0
    for y in range(height):
        filter_type = pixels[position]
        row = bytearray(pixels[position + 1:position + 1 + stride])
        position += stride + 1
        require(filter_type in range(5), "Unsupported PNG row filter")
        for i in range(stride):
            left = row[i - 3] if i >= 3 else 0
            above = previous[i]
            upper_left = previous[i - 3] if i >= 3 else 0
            if filter_type == 1:
                predictor = left
            elif filter_type == 2:
                predictor = above
            elif filter_type == 3:
                predictor = (left + above) // 2
            elif filter_type == 4:
                estimate = left + above - upper_left
                distances = (abs(estimate - left), abs(estimate - above),
                             abs(estimate - upper_left))
                predictor = (left, above, upper_left)[distances.index(min(distances))]
            else:
                predictor = 0
            row[i] = (row[i] + predictor) & 255

        if 230 <= y < 650:
            for x in range(250, 1450):
                rgb = tuple(row[3 * x:3 * x + 3])
                for arm, color in target_colors.items():
                    treatment_pixels[arm] += rgb == color
        if 850 <= y < 1050:
            for x in range(150, 1650):
                red, green, blue = row[3 * x:3 * x + 3]
                count_grid_pixels += max(red, green, blue) < 120
        previous = row

    for arm, count in treatment_pixels.items():
        require(count >= 100, f"Missing {arm} treatment curve in PNG ({count} pixels)")
    require(count_grid_pixels >= 300,
            f"Missing contributing-subject count grid in PNG ({count_grid_pixels} dark pixels)")
    return treatment_pixels, count_grid_pixels


def incidence(events, denominator):
    n = len({r["USUBJID"] for r in events})
    return f"{n} ({100 * n / denominator:.1f}%)"


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--root", type=Path,
                        default=Path(__file__).resolve().parents[1])
    parser.add_argument("--reviewed-figure-sha256",
                        help="SHA256 of the PNG independently inspected visually; binds review to the exact image")
    args = parser.parse_args()
    root = args.root.resolve()
    results = []

    def check(identifier, description, calculation):
        try:
            detail = calculation()
            results.append((identifier, description, "PASS", str(detail)))
        except Exception as error:
            results.append((identifier, description, "FAIL", str(error)))

    datasets = {}
    for name in ("adsl", "adae", "adlb", "advs"):
        def load(name=name):
            _, records = read_csv(root / f"data/adam/{name}.csv")
            require(bool(records), "Empty dataset")
            datasets[name] = records
            keys = [(r["USUBJID"], r.get("ASEQ", "")) for r in records]
            require(len(keys) == len(set(keys)), "Duplicate subject/sequence keys")
            return f"{len(records)} records; unique keys"
        check(name.upper(), f"{name.upper()} input and keys", load)

    if len(datasets) == 4:
        adsl, adae, adlb, advs = [datasets[n] for n in
                                 ("adsl", "adae", "adlb", "advs")]
        subjects = {r["USUBJID"]: r for r in adsl}
        itt = [r for r in adsl if r["ITTFL"] == "Y"]
        safety = [r for r in adsl if r["SAFFL"] == "Y"]

        for name in ("adae", "adlb", "advs"):
            def mapping(name=name):
                for row in datasets[name]:
                    member = subjects[row["USUBJID"]]
                    require(row["TRT01A"] == member["TRT01A"],
                            f"Actual-treatment mismatch: {row['USUBJID']}")
                    require(row["SAFFL"] == member["SAFFL"],
                            f"Safety-flag mismatch: {row['USUBJID']}")
                return "Actual treatment and safety flags agree with ADSL"
            check(f"MAP-{name.upper()}", "ADaM treatment/population agreement", mapping)

        def teae_rule():
            for row in adae:
                member = subjects[row["USUBJID"]]
                onset = date.fromisoformat(row["ASTDT"])
                eligible = (date.fromisoformat(member["TRTSDT"]) <= onset <=
                            date.fromisoformat(member["TRTEDT"]) + timedelta(days=30))
                require(row["TRTEMFL"] == ("Y" if eligible else ""),
                        f"Wrong TRTEMFL: {row['USUBJID']}/{row['ASEQ']}")
            return "Inclusive onset window recalculated using ADSL treatment dates"
        check("TEAE-RULE", "ADAE treatment-emergent window", teae_rule)
        # Calculate qualifying events independently of the stored TRTEMFL flag.
        teae = [r for r in adae if subjects[r["USUBJID"]]["SAFFL"] == "Y" and
                date.fromisoformat(subjects[r["USUBJID"]]["TRTSDT"]) <=
                date.fromisoformat(r["ASTDT"]) <=
                date.fromisoformat(subjects[r["USUBJID"]]["TRTEDT"]) + timedelta(days=30)]

        def markdown_check(stem, folder, title, notes):
            fields, rows = read_csv(root / f"output/{folder}/{stem}.csv")
            text = (root / f"output/{folder}/{stem}.md").read_text(encoding="utf-8")
            require(text.splitlines()[0] == title, "Wrong output title/number")
            expected = ["| " + " | ".join(fields) + " |",
                        "| " + " | ".join(["---"] * len(fields)) + " |"]
            expected += ["| " + " | ".join(r[f].replace("|", r"\|")
                                           for f in fields) + " |" for r in rows]
            displayed = [line for line in text.splitlines() if line.startswith("| ")]
            require(displayed == expected, "Markdown does not exactly reproduce CSV rows")
            for note in notes:
                require(note in text, f"Missing population/definition note: {note}")
            return f"All {len(rows)} displayed rows match CSV; title and definitions checked"

        table1 = root / "output/tables/table_01_demographics.csv"
        for arm in ARMS + ["Overall"]:
            def demographics(arm=arm):
                fields, rows = read_csv(table1)
                require(fields == ["Characteristic", "Statistic"] + ARMS + ["Overall"],
                        "Wrong Table 1 columns")
                require([(r["Characteristic"], r["Statistic"]) for r in rows] ==
                        [("Subjects", "N"), ("Age (years)", "n"), ("", "Mean (SD)"),
                         ("", "Median"), ("", "Min"), ("", "Max"),
                         ("Sex", "Female, n (%)"), ("", "Male, n (%)")],
                        "Wrong Table 1 row labels/order")
                group = itt if arm == "Overall" else [r for r in itt if r["TRT01P"] == arm]
                ages = [float(r["AGE"]) for r in group]
                require(all(r["AGEU"] == "YEARS" for r in group), "Wrong age unit")
                n = len(group)
                expected = [str(n), str(len(ages)),
                            f"{statistics.mean(ages):.1f} ({statistics.stdev(ages):.1f})",
                            f"{statistics.median(ages):.1f}",
                            f"{min(ages):.1f}", f"{max(ages):.1f}"]
                expected += [incidence([r for r in group if r["SEX"] == sex], n)
                             for sex in ("F", "M")]
                require([r[arm] for r in rows] == expected,
                        f"Count, age statistic, or sex percentage mismatch: {arm}")
                return f"8 cells; planned treatment; ITT denominator {n}; 1-decimal display"
            check(f"T1-{arm}", "Table 1 counts, age, sex, denominator and rounding", demographics)

        terms = sorted({r["AETERM"] for r in teae}, key=lambda term:
                       (-len({r["USUBJID"] for r in teae if r["AETERM"] == term}), term))
        for arm in ARMS + ["Overall"]:
            def adverse_events(arm=arm):
                fields, rows = read_csv(root / "output/tables/table_02_teae.csv")
                require(fields == ["Characteristic", "Statistic"] + ARMS + ["Overall"],
                        "Wrong Table 2 columns")
                labels = ["Safety population", "Subjects with any TEAE", "TEAE records",
                          "Subjects with serious TEAE", "Subjects with severe TEAE", "Event term"] + terms
                require([r["Characteristic"] for r in rows] == labels, "Wrong TEAE rows/order")
                require([r["Statistic"] for r in rows] ==
                        ["N", "n (%)", "n", "n (%)", "n (%)", ""] + ["n (%)"] * len(terms),
                        "Wrong TEAE statistic labels")
                members = safety if arm == "Overall" else [r for r in safety if r["TRT01A"] == arm]
                events = teae if arm == "Overall" else [r for r in teae if r["TRT01A"] == arm]
                n = len(members)
                expected = [str(n), incidence(events, n), str(len(events)),
                            incidence([r for r in events if r["AESER"] == "Y"], n),
                            incidence([r for r in events if r["AESEV"] == "SEVERE"], n), ""]
                expected += [incidence([r for r in events if r["AETERM"] == term], n) for term in terms]
                require([r[arm] for r in rows] == expected, f"TEAE counts/percentages mismatch: {arm}")
                return f"{len(rows)} cells; actual treatment; safety denominator {n}; distinct-subject incidence"
            check(f"T2-{arm}", "Table 2 counts, denominators, percentages and rounding", adverse_events)

        def findings(name, records, parameters, label_column, stem):
            records = [r for r in records if r["SAFFL"] == "Y"]
            row_keys = [(p, visit, measure) for p in parameters for visit, _ in VISITS
                        for measure in (["Observed value"] if visit == "1" else
                                        ["Observed value", "Change from baseline"])]
            def structure():
                fields, table = read_csv(root / f"output/tables/{stem}.csv")
                expected_fields = ["PARAMCD", label_column, "Unit", "Visit", "Visit number", "Measure"]
                for arm in ARMS + ["Overall"]:
                    expected_fields += [f"{arm} n", f"{arm} Mean (SD)"]
                require(fields == expected_fields, "Wrong summary columns")
                require([(r["PARAMCD"], r["Visit number"], r["Measure"]) for r in table] == row_keys,
                        "Missing/extra/reordered test, visit or measure rows")
                keys = [(r["USUBJID"], r["PARAMCD"], r["AVISITN"]) for r in records]
                require(len(keys) == len(set(keys)), "Duplicate subject/test/visit in source")
                return f"{len(row_keys)} complete rows; one source result per subject/test/visit"
            check(f"{name}-STRUCT", f"{name} rows and source contribution uniqueness", structure)
            for arm in ARMS + ["Overall"]:
                def calculations(arm=arm):
                    _, table = read_csv(root / f"output/tables/{stem}.csv")
                    for row in table:
                        group = [r for r in records if r["PARAMCD"] == row["PARAMCD"] and
                                 r["AVISITN"] == row["Visit number"] and
                                 (arm == "Overall" or r["TRT01A"] == arm)]
                        require(bool(group), f"No source data: {row['PARAMCD']}/{row['Visit']}/{arm}")
                        require({r["PARAM"] for r in group} == {row[label_column]}, "Test label mismatch")
                        require({r["AVALU"] for r in group} == {row["Unit"]}, "Unit mismatch")
                        require({r["AVISIT"] for r in group} == {row["Visit"]}, "Visit label mismatch")
                        field = "AVAL" if row["Measure"] == "Observed value" else "CHG"
                        contributors = [r for r in group if r[field] != ""]
                        values = [float(r[field]) for r in contributors]
                        require(all(math.isfinite(v) for v in values), "Nonfinite source value")
                        n = len({r["USUBJID"] for r in contributors})
                        require(n == int(row[f"{arm} n"]), "Wrong contributing-subject n")
                        display = re.fullmatch(r"(-?\d+\.\d{2}) \((\d+\.\d{2})\)",
                                               row[f"{arm} Mean (SD)"])
                        require(display is not None, "Mean (SD) must use 2 decimal places")
                        for shown, expected in zip(display.groups(),
                                                   [statistics.mean(values), statistics.stdev(values)]):
                            require(abs(float(shown) - expected) <= 0.00500001,
                                    f"Summary mismatch: {row['PARAMCD']}/{row['Visit']}/{field}/{arm}")
                    return f"{len(table)} n/mean/SD results; units, visits, safety population and rounding checked"
                check(f"{name}-{arm}", f"{name} contributing n, means and sample SD", calculations)
        findings("T3", adlb, ["ALT", "AST", "CREAT", "HGB"], "Lab test", "table_03_laboratory")
        findings("T4", advs, ["SYSBP", "DIABP", "PULSE", "TEMP", "WEIGHT"],
                 "Vital-sign test", "table_04_vital_signs")

        def listing():
            fields, rows = read_csv(root / "output/listings/listing_01_subject_ae.csv")
            expected_fields = ["USUBJID", "Actual treatment", "Event term", "Start date", "End date",
                               "Severity", "Serious", "Outcome", "Treatment-emergent", "ASEQ"]
            require(fields == expected_fields, "Wrong listing columns")
            ordered = sorted(adae, key=lambda r: (ARMS.index(r["TRT01A"]), r["USUBJID"],
                                                 date.fromisoformat(r["ASTDT"]), int(r["ASEQ"])))
            expected = [[r["USUBJID"], r["TRT01A"], r["AETERM"], r["ASTDT"], r["AENDT"],
                         r["AESEV"], r["AESER"], r["AEOUT"],
                         "Yes" if r["TRTEMFL"] == "Y" else "No", r["ASEQ"]] for r in ordered]
            require([[r[f] for f in fields] for r in rows] == expected,
                    "Missing/duplicated/mismatched/reordered ADAE listing records")
            require(len({(r["USUBJID"], r["ASEQ"]) for r in rows}) == len(adae), "Listing key duplication")
            return f"All {len(adae)} ADAE records exactly once, all fields and sort keys checked"
        check("L1", "Listing 1 completeness, values and sort order", listing)

        def figure_values():
            fields, rows = read_csv(root / "output/figures/figure_01_weight_change.csv")
            require(fields == ["AVISITN", "AVISIT", "WEEK", "TRT01A", "N", "MEAN_CHG", "UNIT"],
                    "Wrong plotted-summary columns")
            visits = [("2", "Week 2", 2), ("3", "Week 4", 4), ("4", "Week 8", 8), ("5", "Week 12", 12)]
            require([(r["AVISITN"], r["TRT01A"]) for r in rows] ==
                    [(v, arm) for v, _, _ in visits for arm in ARMS], "Wrong figure visit/treatment order")
            for row in rows:
                visit, label, week = next(v for v in visits if v[0] == row["AVISITN"])
                require(row["AVISIT"] == label and int(row["WEEK"]) == week and row["UNIT"] == "kg",
                        "Wrong figure visit label, week, or unit")
                group = [r for r in advs if r["SAFFL"] == "Y" and r["PARAMCD"] == "WEIGHT" and
                         r["AVISITN"] == visit and r["TRT01A"] == row["TRT01A"] and r["CHG"] != ""]
                require(all(r["AVALU"] == "kg" for r in group), "Wrong source weight unit")
                n = len({r["USUBJID"] for r in group})
                require(n == len(group) == int(row["N"]), "Wrong plotted contributing-subject n")
                mean = statistics.mean(float(r["CHG"]) for r in group)
                require(abs(float(row["MEAN_CHG"]) - mean) <= 0.00005001, "Wrong plotted mean")
                require(float(row["MEAN_CHG"]) == round(float(row["MEAN_CHG"]), 4), "Mean exceeds 4 decimals")
            return "12 means/counts independently calculated; chronological visits, actual treatment and kg checked"
        check("F1-DATA", "Figure 1 plotted summaries", figure_values)

        def figure_artifact():
            image = (root / "output/figures/figure_01_weight_change.png").read_bytes()
            treatment_pixels, count_pixels = png_content_evidence(image)
            return (f"1800 x 1100 PNG; treatment pixels {treatment_pixels}; "
                    f"count-grid pixels {count_pixels}; SHA256 {hashlib.sha256(image).hexdigest()}")
        check("F1-PNG", "Figure 1 PNG structure and visible content", figure_artifact)

        def figure_labels():
            code = (root / "programs/tlf/figure_01_weight_change.R").read_text(encoding="utf-8")
            require('main = "Figure 1.' in code, "Figure title lacks Figure 1 numbering")
            for label in ("Mean change from baseline (kg)", "ABC101 safety population; actual treatment",
                          "Contributing subjects (n)"):
                require(label in code, f"Missing figure label: {label}")
            return "Figure generator labels checked; PNG still requires visual inspection"
        check("F1-LABELS", "Figure numbering, population and units", figure_labels)

        if args.reviewed_figure_sha256:
            def visual_review():
                image = root / "output/figures/figure_01_weight_change.png"
                require(hashlib.sha256(image.read_bytes()).hexdigest() ==
                        args.reviewed_figure_sha256.lower(),
                        "Visual-review fingerprint does not match current PNG")
                return "Operator recorded visual review of this exact PNG: title, line/legend treatment mapping, visits, kg axis and count grid"
            check("F1-VISUAL", "Recorded independent visual inspection", visual_review)

        markdown_specs = [
            ("T1-DISPLAY", "table_01_demographics", "tables",
             "# Table 1. Demographic characteristics by planned treatment",
             ["intent-to-treat population", "denominator", "sample standard deviation"]),
            ("T2-DISPLAY", "table_02_teae", "tables",
             "# Table 2. Treatment-emergent adverse events by actual treatment",
             ["safety population", "30 days after treatment end, inclusive", "distinct subjects", "not externally coded"]),
            ("T3-DISPLAY", "table_03_laboratory", "tables",
             "# Table 3. Laboratory results and change from baseline by actual treatment",
             ["safety population", "distinct subjects", "two decimal places", "postbaseline visits"]),
            ("T4-DISPLAY", "table_04_vital_signs", "tables",
             "# Table 4. Vital signs and change from baseline by actual treatment",
             ["safety population", "distinct subjects", "two decimal places", "postbaseline visits"]),
            ("L1-DISPLAY", "listing_01_subject_ae", "listings",
             "# Listing 1. Subject-level adverse events",
             ["all ADAE records", "Yes for TRTEMFL=Y and No for a blank flag"]),
        ]
        for identifier, stem, folder, title, notes in markdown_specs:
            check(identifier, "Displayed rows, labels and definitions",
                  lambda stem=stem, folder=folder, title=title, notes=notes:
                  markdown_check(stem, folder, title, notes))

    passed = sum(r[2] == "PASS" for r in results)
    failed = len(results) - passed
    status = "PASS" if not failed else "FAIL"
    report = ["# ABC101 Independent Final TLF QC Report", "",
              f"**Overall status:** {status}", "",
              f"**Checks:** {len(results)} total; {passed} passed; {failed} failed", "",
              "Recalculations use ADaM sources and Python's standard library. Production R summaries are not sourced or executed.",
              "", "| Check | Description | Status | Evidence |", "|---|---|---:|---|"]
    report += ["| " + " | ".join(str(v).replace("|", r"\|").replace("\n", " ") for v in r) + " |"
               for r in results]
    report += ["", "## Display review scope", "",
               "Markdown rows, titles, units, population notes and numeric precision are checked above.",
               "The PNG dimensions, treatment-color pixels, count-grid ink and generator labels are checked automatically. Visual inspection is recorded only when --reviewed-figure-sha256 is supplied after viewing the exact image; this is an operator attestation, not an automated interpretation of every plotted value.",
               "", "## Input fingerprints", ""]
    for name in datasets:
        path = root / f"data/adam/{name}.csv"
        report.append(f"- {name.upper()}: `{hashlib.sha256(path.read_bytes()).hexdigest()}`")
    report_path = root / "output/qc/tlf_validation_report.md"
    report_path.parent.mkdir(parents=True, exist_ok=True)
    report_path.write_text("\n".join(report) + "\n", encoding="utf-8")
    print(f"Final TLF QC: {status} ({passed} passed, {failed} failed)")
    return 1 if failed else 0


if __name__ == "__main__":
    raise SystemExit(main())
