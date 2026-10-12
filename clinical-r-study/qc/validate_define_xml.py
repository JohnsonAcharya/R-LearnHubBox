"""Validate synthetic Define-XML against specs, CSVs, references, and optional XSD.

Run: python qc/validate_define_xml.py [--schema PATH_TO_define2-1-0.xsd]
The XSD option requires lxml. This script is read-only.
"""

import argparse
import csv
from pathlib import Path
import re
import xml.etree.ElementTree as ET

ROOT = Path(__file__).resolve().parents[1]
ODM = "http://www.cdisc.org/ns/odm/v1.3"
DEFINE = "http://www.cdisc.org/ns/def/v2.1"
XLINK = "http://www.w3.org/1999/xlink"
SETS = {
    "sdtm": (
        ("DM", "AE", "LB", "VS", "EX"),
        "docs/ABC101_synthetic_SDTM_dataset_specifications_v1.0.md",
        "SDTMIG", "3.4",
    ),
    "adam": (
        ("ADSL", "ADAE", "ADLB", "ADVS"),
        "docs/ABC101_synthetic_ADaM_dataset_specifications_v1.0.md",
        "ADaMIG", "1.3",
    ),
}


def tag(namespace, name):
    return f"{{{namespace}}}{name}"


def spec_variables(text, domain):
    section = re.search(rf"^## {domain} —[^\n]*\n(.*?)(?=^## |\Z)", text, re.M | re.S)
    assert section, f"No {domain} section"
    variables = []
    for line in section.group(1).splitlines():
        cells = [cell.strip().replace("`", "").replace("**", "") for cell in line.split("|")[1:-1]]
        if len(cells) == 4 and cells[1] in {"C", "N", "ISO", "Date"}:
            variables.append(tuple(cells))
    return variables


def description(element):
    text = element.find(f"{tag(ODM, 'Description')}/{tag(ODM, 'TranslatedText')}")
    assert text is not None and text.text
    return text.text


def check(layer, domains, spec_path, standard_name, standard_version, schema):
    path = ROOT / "metadata" / "v1.0" / layer / "define.xml"
    tree = ET.parse(path)
    root = tree.getroot()
    assert root.tag == tag(ODM, "ODM")
    assert root.get(tag(DEFINE, "Context")) == "Other"
    mdv = root.find(f"{tag(ODM, 'Study')}/{tag(ODM, 'MetaDataVersion')}")
    assert mdv is not None and mdv.get(tag(DEFINE, "DefineVersion")) == "2.1.0"
    standards = mdv.find(tag(DEFINE, "Standards"))
    standard = standards.find(tag(DEFINE, "Standard"))
    assert standard.get("Name") == standard_name and standard.get("Version") == standard_version
    assert standard.get("OID") == f"STD.{standard_name}"
    specs = (ROOT / spec_path).read_text(encoding="utf-8")
    item_defs = {item.get("OID"): item for item in mdv.findall(tag(ODM, "ItemDef"))}
    method_defs = {item.get("OID"): item for item in mdv.findall(tag(ODM, "MethodDef"))}
    leaves = {item.get("ID"): item for item in mdv.findall(tag(DEFINE, "leaf"))}
    groups = mdv.findall(tag(ODM, "ItemGroupDef"))
    assert len(groups) == len(domains)
    assert len(item_defs) == len(mdv.findall(tag(ODM, "ItemDef")))
    assert len(method_defs) == len(mdv.findall(tag(ODM, "MethodDef")))
    assert len(leaves) == len(domains)
    assert [group.get("Name") for group in groups] == list(domains)
    all_oids = [standard.get("OID")] + [node.get("OID") for node in groups]
    all_oids += list(item_defs) + list(method_defs)
    assert len(all_oids) == len(set(all_oids)), f"{layer}: duplicate OID"

    variable_count = 0
    nonmethod_count = 0
    referenced_methods = set()
    for group in groups:
        domain = group.get("Name")
        assert group.get(tag(DEFINE, "StandardOID")) == standard.get("OID")
        leaf = leaves[group.get(tag(DEFINE, "ArchiveLocationID"))]
        href = leaf.get(tag(XLINK, "href"))
        assert href and not href.lower().endswith(".xpt")
        csv_path = (path.parent / href).resolve(strict=True)
        assert csv_path == (ROOT / "data" / layer / f"{domain.lower()}.csv").resolve()
        with csv_path.open(newline="", encoding="utf-8-sig") as handle:
            reader = csv.DictReader(handle)
            columns = reader.fieldnames
            data = list(reader)
        variables = spec_variables(specs, domain)
        refs = group.findall(tag(ODM, "ItemRef"))
        assert len(refs) == len(columns) == len(variables)
        assert columns == [variable[0] for variable in variables]
        for ordinal, (ref, (name, declared_type, label, mapping)) in enumerate(zip(refs, variables), 1):
            assert ref.get("OrderNumber") == str(ordinal)
            item = item_defs[ref.get("ItemOID")]
            assert item.get("Name") == name and description(item) == label
            origin = item.find(tag(DEFINE, "Origin"))
            assert origin is not None and mapping in description(origin)
            assigned = mapping.startswith("Constant ")
            predecessor = mapping.startswith("Copy ")
            assert origin.get("Type") == ("Assigned" if assigned else ("Predecessor" if predecessor else "Derived"))
            if assigned or predecessor:
                nonmethod_count += 1
                assert ref.get("MethodOID") is None
            else:
                method = method_defs[ref.get("MethodOID")]
                assert description(method) == mapping
                referenced_methods.add(ref.get("MethodOID"))
            nonempty = [record[name] for record in data if record[name] != ""]
            assert nonempty
            if declared_type == "Date":
                assert item.get("DataType") == "date"
            elif declared_type in {"C", "ISO"}:
                assert item.get("DataType") == "text"
                assert int(item.get("Length")) == max(map(len, nonempty))
            else:
                is_integer = all(float(value).is_integer() for value in nonempty)
                assert item.get("DataType") == ("integer" if is_integer else "float")
                assert int(item.get("Length")) == max(map(len, nonempty))
            variable_count += 1
    assert len(item_defs) == variable_count
    assert len(method_defs) == variable_count - nonmethod_count
    assert referenced_methods == set(method_defs), f"{layer}: orphan method"
    if schema:
        from lxml import etree
        xsd = etree.XMLSchema(etree.parse(str(schema)))
        document = etree.parse(str(path))
        if not xsd.validate(document):
            raise AssertionError(f"{layer}: XSD invalid: {xsd.error_log}")
    print(
        f"PASS {layer.upper()}: {len(groups)} datasets, {variable_count} variables, "
        f"{len(method_defs)} methods, {len(leaves)} resolved CSV references; "
        f"XSD {'PASS' if schema else 'not run'}"
    )


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--schema", type=Path, help="Local Define-XML 2.1 entry XSD")
    args = parser.parse_args()
    if args.schema and not args.schema.is_file():
        parser.error(f"Schema not found: {args.schema}")
    for layer_name, details in SETS.items():
        check(layer_name, *details, args.schema)
