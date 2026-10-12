# ABC101 synthetic Define-XML preparation and validation — v1.0

> **SYNTHETIC REVIEW DEMONSTRATION.** The XML files described here are metadata for the project's CSV outputs. They are not submission-ready Define-XML and do not establish SDTM, ADaM, terminology, or FDA conformance.

| Control | Value |
| --- | --- |
| Report version / date | 1.0 / 2026-10-13 |
| Source specifications | [SDTM v1.0](ABC101_synthetic_SDTM_dataset_specifications_v1.0.md), [ADaM v1.0](ABC101_synthetic_ADaM_dataset_specifications_v1.0.md) |
| Standards assumption | [Study design v1.0](ABC101_synthetic_study_design_and_standards_v1.0.md) |
| Metadata outputs | [SDTM Define-XML](../metadata/v1.0/sdtm/define.xml), [ADaM Define-XML](../metadata/v1.0/adam/define.xml) |

## Metadata availability before generation

The committed specifications and CSVs support the nine dataset names, descriptions, row grains, keys, 182 ordered variable names and labels, logical data types, record counts, local CSV locations, and variable-level source/derivation text. The code supports the SDTMIG and ADaMIG version selections already recorded in the design assumptions. Numeric precision and text lengths can be measured from the **current CSV values**; those measurements are observations, not planned XPT storage attributes.

The following metadata is **missing or not established** by the current project:

| Missing information | Why it cannot be asserted from current files |
| --- | --- |
| Submission transport and final physical attributes | There are CSVs, no SAS V5 XPT files, validated transport lengths, SAS formats, or submission file names. |
| CDISC controlled-terminology codelist identifiers and value-level mappings | Package 58 was selected as a target, but current values were not mapped to its codelists. Observed categories alone do not prove controlled terminology. |
| Medical coding dictionary | `AEDECOD` is uppercase `AETERM`; it is not a MedDRA-coded preferred term. |
| Real source-origin documentation | The raw inputs are generated rather than captured on CRFs; there is no annotated CRF, source system record, or investigator collection provenance. |
| Actual protocol identity | ABC101 is a synthetic study identifier; no approved protocol exists. |
| Complete regulatory metadata | Dataset and variable core/expected status, value-level metadata, external dictionaries, analysis-results metadata, CDISC conformance results, and reviewer guides have not been established. |

## Implemented metadata

The reproducible [generator](../programs/metadata/generate_define_xml.py) reads the two committed specification tables and nine current CSVs. It writes separate SDTM and ADaM XML files under `metadata/v1.0/`. Each contains ODM 1.3.2 / Define-XML 2.1.0 structure, a synthetic study description, the selected implementation-guide reference, dataset labels and grains, ordered variables and labels, and CSV file links. A copied value is marked `Predecessor`, a constant `Assigned`, and a calculation `Derived` with a method whose text comes from its specification row. Only identifier and sequence keys are marked mandatory; this does not assert the complete SDTMIG or ADaMIG core/expected-variable classification.

`def:Context="Other"` identifies this as **non-submission CSV metadata**. `ProtocolName="ABC101-SYNTHETIC-NO-PROTOCOL"` is an explicit placeholder required by the ODM structure, not a claim that a protocol exists. The fixed `CreationDateTime` of 2026-10-13 00:00:00 is the synthetic metadata-version timestamp chosen for reproducible output, not the time of a clinical event. Text and numeric `Length` and numeric `SignificantDigits` values are the maximum representation observed in the current CSVs; they are **not XPT design limits**. The XML does not declare external code lists or package-58 conformance.

| XML file | Datasets | Variables | Derived-variable methods | Resolved CSV links |
| --- | ---: | ---: | ---: | ---: |
| SDTM | 5 | 79 | 31 | 5 |
| ADaM | 4 | 103 | 48 | 4 |

## Validation performed

Run `python programs/metadata/generate_define_xml.py` to regenerate the metadata. Run `python qc/validate_dataset_specs.py` to recheck all source-to-dataset mappings, then `python qc/validate_define_xml.py` for XML-to-specification and CSV-reference checks. The [Define-XML validator](../qc/validate_define_xml.py) checks XML structure, unique definition IDs, item and method references, variable names/order/labels, origin and method text, observed data types/lengths, and all nine relative dataset paths and CSV headers. On 2026-10-13, all checks passed: **182/182 variables and 9/9 CSV references**. The underlying dataset-specification validator also passed on all 48,000 output rows.

Both XML files additionally **passed XSD validation** with `qc/validate_define_xml.py --schema <path-to-define2-1-0.xsd>` using `lxml` and the public schema copy in [CDISC's DataExchange-RWD-Lineage repository](https://github.com/cdisc-org/DataExchange-RWD-Lineage/tree/ff3c2fee622b69035526c9bf507d02d59e351a0e/tools/schema), commit `ff3c2fee622b69035526c9bf507d02d59e351a0e`. The entry schema is `tools/schema/define2-1-0.xsd` and depends on sibling schema files and `tools/cdisc-odm-1.3.2/`. Its `define-enumerations.xsd` identifies a 2021-03-26 terminology basis, so this XSD pass verifies core XML structure and **does not validate the selected 2024-09-27 package 58**. CDISC's [Define-XML v2.1.8 release page](https://www.cdisc.org/standards/data-exchange/define-xml/define-xml-v2-1-8) identifies the later package-58 schema update; that exact package was not available for this check.

## Remaining gaps

- The XML links to CSV files and uses `Context="Other"`. No XPT datasets or submission-context Define-XML were generated or tested.
- Package-58 codelist IDs, allowed values, and NCI aliases were not added or validated. MedDRA coding remains absent.
- Observed lengths and decimals must not be treated as prospective transport attributes; type/length checks against XPT remain open.
- A real protocol name, aCRF origins, collection provenance, value-level metadata, analysis-results metadata, and full CDISC/FDA business-rule conformance remain unavailable or untested.
- `Predecessor`, `Assigned`, and `Derived` are based on the current specification text and synthetic source lineage. An independent regulatory origin review remains open.

These gaps are documented here instead of being filled with invented clinical or submission facts. No reviewer guide or dataset was changed.
