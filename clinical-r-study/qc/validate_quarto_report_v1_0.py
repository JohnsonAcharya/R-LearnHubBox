"""Validate the rendered ABC101 synthetic Quarto report against current outputs.

Run after rendering: python qc/validate_quarto_report_v1_0.py
Read-only; uses Python's standard library. This checks content, not browser layout.
"""

import base64
import csv
from datetime import date, timedelta
import hashlib
from html.parser import HTMLParser
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
QMD = ROOT / "reports/ABC101_synthetic_review_v1.0.qmd"
HTML = QMD.with_suffix(".html")


class ReportParser(HTMLParser):
    def __init__(self):
        super().__init__()
        self.tables = []
        self.table = None
        self.row = None
        self.cell = None
        self.images = []
        self.links = []
        self.headings = []
        self.heading = None

    def handle_starttag(self, tag, attrs):
        attributes = dict(attrs)
        if tag == "table":
            self.table = []
        elif tag == "tr" and self.table is not None:
            self.row = []
        elif tag in {"td", "th"} and self.row is not None:
            self.cell = []
        elif tag == "img":
            self.images.append(attributes)
        elif tag == "a":
            self.links.append(attributes.get("href", ""))
        elif tag in {"h1", "h2"}:
            self.heading = []

    def handle_data(self, value):
        if self.cell is not None:
            self.cell.append(value)
        if self.heading is not None:
            self.heading.append(value)

    def handle_endtag(self, tag):
        if tag in {"td", "th"} and self.cell is not None:
            self.row.append(" ".join("".join(self.cell).split()))
            self.cell = None
        elif tag == "tr" and self.row is not None:
            self.table.append(self.row)
            self.row = None
        elif tag == "table" and self.table is not None:
            self.tables.append(self.table)
            self.table = None
        elif tag in {"h1", "h2"} and self.heading is not None:
            self.headings.append(" ".join("".join(self.heading).split()))
            self.heading = None


def read_csv(relative):
    with (ROOT / relative).open(newline="", encoding="utf-8-sig") as handle:
        return list(csv.reader(handle))


def read_dict_csv(relative):
    with (ROOT / relative).open(newline="", encoding="utf-8-sig") as handle:
        return list(csv.DictReader(handle))


def text(relative):
    return (ROOT / relative).read_text(encoding="utf-8")


def main():
    assert QMD.is_file() and HTML.is_file(), "Render the .qmd before validating"
    source = QMD.read_text(encoding="utf-8")
    rendered = HTML.read_text(encoding="utf-8")
    assert "SYNTHETIC DATA" in source and "not an FDA submission" in source
    assert "SYNTHETIC DATA" in rendered and "not an FDA submission" in rendered
    assert "flowchart LR" in rendered, "Data-flow diagram missing"

    parser = ReportParser()
    parser.feed(rendered)
    assert len(parser.tables) == 5, f"Expected 4 TLF tables plus QC table; got {len(parser.tables)}"
    table_names = ("table_01_demographics", "table_02_teae",
                   "table_03_laboratory", "table_04_vital_signs")
    for number, name in enumerate(table_names):
        expected = read_csv(f"output/tables/{name}.csv")
        assert parser.tables[number] == expected, f"Rendered Table {number + 1} differs from {name}.csv"
        assert f"output/tables/{name}.md" in source, f"Quarto source does not include {name}.md"

    adsl = read_dict_csv("data/adam/adsl.csv")
    adae = read_dict_csv("data/adam/adae.csv")
    listing = read_csv("output/listings/listing_01_subject_ae.csv")
    figure = read_dict_csv("output/figures/figure_01_weight_change.csv")
    assert len(adsl) == 500 and all(r["ITTFL"] == r["SAFFL"] == "Y" for r in adsl)
    assert len(adae) == 750 and len(listing) - 1 == len(adae)
    assert len(figure) == 12 and [int(r["WEEK"]) for r in figure] == [2] * 3 + [4] * 3 + [8] * 3 + [12] * 3
    assert {r["UNIT"] for r in figure} == {"kg"}
    treatments = ["Placebo", "Drug 10 mg", "Drug 20 mg"]
    assert [r["TRT01A"] for r in figure] == treatments * 4
    for row in adae:
        expected = "Y" if (date.fromisoformat(row["TRTSDT"]) <= date.fromisoformat(row["ASTDT"])
                           <= date.fromisoformat(row["TRTEDT"]) + timedelta(days=30)) else ""
        assert row["TRTEMFL"] == expected, "ADAE TEAE rule differs from report text"

    assert len(parser.images) == 1, "Expected one embedded Figure 1 image"
    image_src = parser.images[0].get("src", "")
    assert image_src.startswith("data:image/png;base64,"), "Figure 1 is not embedded"
    embedded = base64.b64decode(image_src.split(",", 1)[1], validate=True)
    png = (ROOT / "output/figures/figure_01_weight_change.png").read_bytes()
    assert embedded == png, "Embedded Figure 1 differs from current PNG"

    for label, path, count in (("SDTM", "output/qc/sdtm_validation_report.md", 45),
                               ("ADaM", "output/qc/adam_validation_report.md", 52),
                               ("TLF", "output/qc/tlf_validation_report.md", 36)):
        report = text(path)
        assert "**Overall status:** PASS" in report
        assert f"**Checks:** {count} total; {count} passed; 0 failed" in report
        assert f"{count}/{count}" in rendered, f"{label} QC count missing from rendered report"
    assert hashlib.sha256(png).hexdigest() in text("output/qc/tlf_validation_report.md"), \
        "Final TLF QC does not identify the exact embedded PNG"
    conformance = text("output/qc/ABC101_synthetic_conformance_findings_v1.0.md")
    assert "8/8 PASS" in conformance and "9 separately listed" in conformance
    assert "8/8" in rendered and "9" in rendered
    spec = text("docs/ABC101_dataset_spec_validation_v1.0.md")
    assert "182/182" in spec and "48,000" in spec
    assert "182/182" in rendered and "48,000" in rendered

    local = [href for href in parser.links if href.startswith("../")]
    missing = [href for href in local if not (HTML.parent / href).exists()]
    assert not missing, f"Broken report links: {missing}"
    for heading in ("Table 1.", "Table 2.", "Table 3.", "Table 4.",
                    "Listing 1", "Figure 1", "Open conformance gaps"):
        assert any(h.startswith(heading) for h in parser.headings), f"Missing heading {heading}"
    print("PASS: 4 rendered tables match CSVs cell-for-cell; 750 listing records; "
          "12 figure rows and exact embedded PNG; QC/conformance figures; "
          f"{len(local)} local links and report headings")


if __name__ == "__main__":
    main()
