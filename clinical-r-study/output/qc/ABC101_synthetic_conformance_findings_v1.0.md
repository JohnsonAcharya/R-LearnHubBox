# ABC101 synthetic conformance screening findings — v1.0

> **SYNTHETIC REVIEW DEMONSTRATION.** This is a limited, read-only standards screening of current CSVs and Define-XML. It is not a CDISC CORE, FDA business/validator-rule, XPT, or submission conformance certificate.

| Control | Value |
| --- | --- |
| Version / date | 1.0 / 2026-10-13 |
| Program | [validate_conformance_v1_0.py](../../qc/validate_conformance_v1_0.py) |
| Inputs | 5 SDTM CSVs, 4 ADaM CSVs, 2 Define-XML files, committed specifications |
| Limited executable screening | 8/8 PASS; 0 failed check(s) |
| Open/untested areas | 9 separately listed below |
| Full standards conformance | **NOT ASSESSED** |

## Rule sources and version boundaries

The [study-design assumptions](../../docs/ABC101_synthetic_study_design_and_standards_v1.0.md) select standards using an assumed 2025-01-01 start. FDA's March 2025 catalog is a retrospective support-date reference, not a snapshot from that start date. The October 2026 technical guide is the current review-process guidance at this report date, not a replacement for that version-selection basis. Relevant sources:

- [FDA Data Standards Catalog v11.0, March 2025 (version selection for assumed 2025-01-01 start)](https://www.fda.gov/regulatory-information/search-fda-guidance-documents/data-standards-catalog).
- [CDISC SDTM v2.0 / SDTMIG v3.4; SDTM/SDTMIG Conformance Rules v2.0, 2021-11-29](https://www.cdisc.org/standards/foundational/sdtmig/sdtm-and-sdtmig-conformance-rules-v2-0).
- [CDISC ADaM v2.1 / ADaMIG v1.3; ADaM Conformance Rules v5.0, 2023-10-06](https://www.cdisc.org/standards/foundational/adam/adam-conformance-rules-v5-0).
- [CDISC Define-XML v2.1; Conformance Rules for Define-XML v2.1, 2021-03-30](https://www.cdisc.org/standards/foundational/define-xml/conformance-rules-define-xml-v2-1).
- [CDISC Define-XML v2.1.8 schema package, 2024-12-20 (package-58 update; not locally available for this run)](https://www.cdisc.org/standards/data-exchange/define-xml/define-xml-v2-1-8).
- [CDISC terminology package 58, 2024-09-27 (selected target; not loaded)](https://www.cdisc.org/standards/terminology/controlled-terminology).
- [FDA Business Rules v1.5, May 2019; FDA Validator Rules v1.6, December 2022](https://www.fda.gov/industry/fda-data-standards-advisory-board/study-data-standards-resources).
- [FDA Study Data Technical Conformance Guide, October 2026 (current guidance at this report date)](https://www.fda.gov/media/153632/download).

The CDISC conformance-rule files require sign-in on their publication pages and are not in this checkout. FDA's public resources identify the Business/Validator rule versions, but their spreadsheet rules were not executed here. No published rule ID is assigned to a local check below. Rule applicability can also depend on study design, dataset class, metadata, and submission context.

## Executable screening (local IDs only)

| ID | Area | Result | Check | Evidence |
| --- | --- | --- | --- | --- |
| SC-01 | CSV structure | **PASS** | Basic dataset/variable name screening | 9 CSV names and 182 column names fit the basic 8-character uppercase pattern. |
| SC-02 | SDTM/ADaM | **PASS** | Identifiers and subject links | STUDYID, USUBJID, DOMAIN (SDTM), and DM subject links agree. |
| SC-03 | SDTM/ADaM | **PASS** | Declared record-key uniqueness | All 9 declared dataset keys are populated and unique. |
| SC-04 | SDTM | **PASS** | Current full-date syntax and calendar validity | 27,000 populated SDTM --DTC values parse as complete ISO calendar dates. |
| SC-05 | SDTM | **PASS** | Findings test-code/name length screening | LB/VS test codes are nonblank and <=8 characters; test names are nonblank and <=40. |
| SC-06 | ADaM | **PASS** | Record-level SDTM source traceability | ADAE/ADLB/ADVS retain every source row with matching SRCDOM/SRCVAR/SRCSEQ. |
| SC-07 | Define-XML | **PASS** | XML-to-specification/CSV references and optional XSD | Existing Define validator passed: 9 CSV links, 182 ordered items, origins/methods; XSD not rerun (no schema supplied). |
| SC-08 | Define-XML | **PASS** | Declared XML and IG versions/context | Both XMLs declare Define 2.1.0, selected IG versions, and Context=Other. |

These checks inspect standard-facing properties that the current files expose. SC-02, SC-03, and SC-06 overlap internal QC; passing them is not an additional standards-conformance determination. SC-07 reuses the existing [Define validator](../../qc/validate_define_xml.py). When no `--schema` is supplied, XML-to-data integrity is checked, but XSD validation is **not rerun**. A prior structural XSD result and its older terminology-enumeration limit are documented in the [Define preparation report](../../docs/ABC101_synthetic_define_xml_preparation_v1.0.md).

## Observed gaps and untestable areas

| ID | Area | Disposition | Finding or reason |
| --- | --- | --- | --- |
| G-01 | Submission transport | **OPEN** | The nine datasets exist only as CSVs; no XPT files or physical transport attributes were validated. |
| G-02 | Trial design/start date | **OPEN** | No TS dataset or actual protocol/start-date record exists; the 2025-01-01 date is an assumption. |
| G-03 | Controlled terminology | **NOT TESTED** | Package 58 codelist IDs and allowed values are not represented in the XML; terminology cannot be checked against the selected package. |
| G-04 | Medical coding | **OPEN** | AEDECOD is the uppercased source term; no MedDRA dictionary/version or coded-term validation exists. |
| G-05 | Origins and source documents | **NOT TESTED** | There is no real aCRF, protocol, source system, or collection-time evidence to verify origin and predose status. |
| G-06 | Analysis metadata | **OPEN** | ADaM XML has no analysis-results metadata or SAP-backed endpoint definitions. |
| G-07 | Complete published rule execution | **NOT TESTED** | The CDISC/FDA rule workbooks and a configured standards validator were not run; no official rule IDs or pass rate are asserted. |
| G-08 | Core/expected/permissible and value-level rules | **NOT TESTED** | The local specifications and XML do not classify all variables by published core status or define value-level metadata; the relevant formal rules were not evaluated. |
| G-09 | FDA technical rejection criteria | **NOT TESTED** | There is no eCTD submission, study tagging file, or ts.xpt; receipt-level checks cannot run against this checkout. |

`OPEN` means a visible package gap or known limitation. `NOT TESTED` means source inputs, terminology data, business context, or the official rules/validator are unavailable. These dispositions are not official FDA/CDISC rule severities. The generated metadata declares `def:Context=Other` and points to CSVs, so an XML reference PASS does not make it submission-ready. The current [SDTM specifications](../../docs/ABC101_synthetic_SDTM_dataset_specifications_v1.0.md) and [ADaM specifications](../../docs/ABC101_synthetic_ADaM_dataset_specifications_v1.0.md) describe project mappings, not full core/expected/permissible-variable conformance.

## Separate internal QC evidence

The existing [SDTM QC](sdtm_validation_report.md) (45/45), [ADaM QC](adam_validation_report.md) (52/52), [final TLF QC](tlf_validation_report.md) (36/36 for the currently reviewed PNG), [specification mapping check](../../docs/ABC101_dataset_spec_validation_v1.0.md), and [Define-XML reference check](../../docs/ABC101_synthetic_define_xml_preparation_v1.0.md) establish consistency of this synthetic pipeline. They do not certify the published conformance rule sets, controlled terminology, or submission transport.

## Reproduction and next boundary

Run `python qc/validate_conformance_v1_0.py` from any directory. Supply `--schema PATH` only for a locally available Define-XML 2.1 entry XSD with its dependencies; note that a structural XSD pass does not check package-58 terminology. The program writes this findings report and does not change datasets, metadata, TLFs, or reviewer guides. Full CDISC/FDA rule execution, transport conversion, and a Quarto report remain outside this step.
