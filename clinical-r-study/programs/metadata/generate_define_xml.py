"""Generate versioned, non-submission Define-XML v2.1 for ABC101 CSV datasets.

Run from any directory: python programs/metadata/generate_define_xml.py
The two Markdown dataset specifications are the variable metadata source.
Existing datasets are read for observed lengths and are never changed.
"""

import csv
from datetime import date
from pathlib import Path
import re
import xml.etree.ElementTree as ET

ROOT = Path(__file__).resolve().parents[2]
ODM = "http://www.cdisc.org/ns/odm/v1.3"
DEFINE = "http://www.cdisc.org/ns/def/v2.1"
XLINK = "http://www.w3.org/1999/xlink"
XML = "http://www.w3.org/XML/1998/namespace"
ET.register_namespace("", ODM)
ET.register_namespace("def", DEFINE)
ET.register_namespace("xlink", XLINK)

SETS = {
    "sdtm": {
        "domains": ("DM", "AE", "LB", "VS", "EX"),
        "spec": "docs/ABC101_synthetic_SDTM_dataset_specifications_v1.0.md",
        "standard": ("SDTMIG", "3.4"),
        "labels": {
            "DM": ("Demographics", "One record per subject"),
            "AE": ("Adverse Events", "One record per subject per event"),
            "LB": ("Laboratory Test Results", "One record per subject per test per visit"),
            "VS": ("Vital Signs", "One record per subject per test per visit"),
            "EX": ("Exposure", "One exposure interval per subject"),
        },
    },
    "adam": {
        "domains": ("ADSL", "ADAE", "ADLB", "ADVS"),
        "spec": "docs/ABC101_synthetic_ADaM_dataset_specifications_v1.0.md",
        "standard": ("ADaMIG", "1.3"),
        "labels": {
            "ADSL": ("Subject-Level Analysis Dataset", "One record per subject"),
            "ADAE": ("Adverse Events Analysis Dataset", "One record per subject per adverse event"),
            "ADLB": ("Laboratory Analysis Dataset", "One record per subject per source lab record"),
            "ADVS": ("Vital Signs Analysis Dataset", "One record per subject per source vital-sign record"),
        },
    },
}


def tag(namespace, name):
    return f"{{{namespace}}}{name}"


def add_description(parent, value):
    description = ET.SubElement(parent, tag(ODM, "Description"))
    ET.SubElement(description, tag(ODM, "TranslatedText"), {tag(XML, "lang"): "en"}).text = value


def plain(value):
    return value.replace("`", "").replace("**", "").strip()


def spec_variables(text, domain):
    section = re.search(rf"^## {domain} —[^\n]*\n(.*?)(?=^## |\Z)", text, re.M | re.S)
    if not section:
        raise ValueError(f"Missing specification section {domain}")
    variables = []
    for line in section.group(1).splitlines():
        cells = [cell.strip() for cell in line.split("|")[1:-1]]
        if len(cells) == 4 and cells[1] in {"C", "N", "ISO", "Date"}:
            variables.append(tuple(plain(cell) for cell in cells))
    if not variables or len({row[0] for row in variables}) != len(variables):
        raise ValueError(f"Invalid variable table for {domain}")
    return variables


def read_dataset(path):
    with path.open(newline="", encoding="utf-8-sig") as handle:
        reader = csv.DictReader(handle)
        return reader.fieldnames, list(reader)


def item_attributes(variable, declared_type, rows):
    present = [row[variable] for row in rows if row[variable] != ""]
    if not present:
        raise ValueError(f"No supported values for {variable}")
    if declared_type == "Date":
        for value in present:
            date.fromisoformat(value)
        return {"DataType": "date"}
    if declared_type in {"C", "ISO"}:
        if declared_type == "ISO":
            for value in present:
                date.fromisoformat(value)
        return {"DataType": "text", "Length": str(max(map(len, present)))}
    if all(float(value).is_integer() for value in present):
        return {"DataType": "integer", "Length": str(max(map(len, present)))}
    precision = max(len(value.partition(".")[2]) for value in present)
    return {
        "DataType": "float",
        "Length": str(max(map(len, present))),
        "SignificantDigits": str(precision),
    }


def build_layer(layer, config):
    output = ROOT / "metadata" / "v1.0" / layer / "define.xml"
    output.parent.mkdir(parents=True, exist_ok=True)
    spec = (ROOT / config["spec"]).read_text(encoding="utf-8")
    root = ET.Element(tag(ODM, "ODM"), {
        "FileType": "Snapshot",
        "FileOID": f"ABC101.{layer.upper()}.SYNTHETIC.v1.0",
        "CreationDateTime": "2026-10-13T00:00:00",
        "ODMVersion": "1.3.2",
        tag(DEFINE, "Context"): "Other",
    })
    study = ET.SubElement(root, tag(ODM, "Study"), {"OID": "STUDY.ABC101.SYNTHETIC"})
    global_variables = ET.SubElement(study, tag(ODM, "GlobalVariables"))
    ET.SubElement(global_variables, tag(ODM, "StudyName")).text = "ABC101 synthetic review demonstration"
    ET.SubElement(global_variables, tag(ODM, "StudyDescription")).text = (
        "Generated type 2 diabetes example; no real subjects or clinical protocol."
    )
    ET.SubElement(global_variables, tag(ODM, "ProtocolName")).text = (
        "ABC101-SYNTHETIC-NO-PROTOCOL"
    )
    metadata = ET.SubElement(study, tag(ODM, "MetaDataVersion"), {
        "OID": f"MDV.ABC101.{layer.upper()}.v1.0",
        "Name": f"ABC101 synthetic {layer.upper()} CSV metadata v1.0",
        tag(DEFINE, "DefineVersion"): "2.1.0",
    })
    standard_name, standard_version = config["standard"]
    standard_oid = f"STD.{standard_name}"
    standards = ET.SubElement(metadata, tag(DEFINE, "Standards"))
    ET.SubElement(standards, tag(DEFINE, "Standard"), {
        "OID": standard_oid, "Name": standard_name, "Version": standard_version,
        "Type": "IG", "Status": "Final",
    })

    all_definitions = []
    for domain in config["domains"]:
        csv_path = ROOT / "data" / layer / f"{domain.lower()}.csv"
        header, rows = read_dataset(csv_path)
        variables = spec_variables(spec, domain)
        if header != [row[0] for row in variables]:
            raise ValueError(f"{domain}: specification and CSV columns/order disagree")
        label, structure = config["labels"][domain]
        group = ET.SubElement(metadata, tag(ODM, "ItemGroupDef"), {
            "OID": f"IG.{domain}",
            "Name": domain,
            "Repeating": "No" if domain in {"DM", "ADSL"} else "Yes",
            "IsReferenceData": "No",
            tag(DEFINE, "Structure"): structure,
            tag(DEFINE, "StandardOID"): standard_oid,
            tag(DEFINE, "ArchiveLocationID"): f"LF.{domain}",
        })
        add_description(group, label)
        for ordinal, (variable, declared_type, variable_label, mapping) in enumerate(variables, 1):
            assigned = mapping.startswith("Constant ")
            predecessor = mapping.startswith("Copy ")
            required_key = variable in {"STUDYID", "DOMAIN", "USUBJID", "ASEQ", f"{domain}SEQ"}
            item_ref_attributes = {
                "ItemOID": f"IT.{domain}.{variable}",
                "OrderNumber": str(ordinal),
                "Mandatory": "Yes" if required_key else "No",
            }
            if not assigned and not predecessor:
                item_ref_attributes["MethodOID"] = f"MT.{domain}.{variable}"
            ET.SubElement(group, tag(ODM, "ItemRef"), item_ref_attributes)
            all_definitions.append((domain, variable, declared_type, variable_label, mapping, rows, assigned, predecessor))

    for domain, variable, declared_type, variable_label, mapping, rows, assigned, predecessor in all_definitions:
        item = ET.SubElement(metadata, tag(ODM, "ItemDef"), {
            "OID": f"IT.{domain}.{variable}",
            "Name": variable,
            **item_attributes(variable, declared_type, rows),
        })
        add_description(item, variable_label)
        origin = ET.SubElement(item, tag(DEFINE, "Origin"), {
            "Type": "Assigned" if assigned else ("Predecessor" if predecessor else "Derived"),
        })
        add_description(origin, f"Synthetic {domain} output. {mapping}")

    for domain, variable, _, _, mapping, _, assigned, predecessor in all_definitions:
        if assigned or predecessor:
            continue
        method = ET.SubElement(metadata, tag(ODM, "MethodDef"), {
            "OID": f"MT.{domain}.{variable}",
            "Name": f"{domain}.{variable} mapping",
            "Type": "Computation",
        })
        add_description(method, mapping)

    for domain in config["domains"]:
        relative = (Path("..") / ".." / ".." / "data" / layer / f"{domain.lower()}.csv").as_posix()
        leaf = ET.SubElement(metadata, tag(DEFINE, "leaf"), {
            "ID": f"LF.{domain}",
            tag(XLINK, "href"): relative,
        })
        ET.SubElement(leaf, tag(DEFINE, "title")).text = f"{domain.lower()}.csv"

    ET.indent(root, space="  ")
    ET.ElementTree(root).write(output, encoding="utf-8", xml_declaration=True)
    print(f"{output.relative_to(ROOT)}: {len(config['domains'])} datasets, {len(all_definitions)} variables")


if __name__ == "__main__":
    for layer_name, layer_config in SETS.items():
        build_layer(layer_name, layer_config)
