# ABC101 synthetic clinical Study Data Reviewer's Guide (cSDRG) — v1.0

> **SYNTHETIC REVIEW DEMONSTRATION ONLY.** ABC101 contains generated records, not patients or an actual clinical trial. This guide describes the present SDTM-like CSVs for navigation and review; it is not a regulatory submission, a protocol, an annotated CRF, or a claim of CDISC conformance.

| Document control | Value |
| --- | --- |
| Study identifier | ABC101 (synthetic) |
| Version / date | 1.0 / 2026-10-13 |
| Scope | Current raw-to-SDTM programs, five SDTM CSVs, metadata, and internal QC |
| Change history | v1.0: initial synthetic cSDRG |
| Companion | [Synthetic ADRG v1.0](ABC101_synthetic_ADRG_v1.0.md) |

## Review context

The [study-design and standards assumptions v1.0](ABC101_synthetic_study_design_and_standards_v1.0.md) select 2025-01-01 as an **assumed** study start solely for standards selection. The target is SDTM v2.0 / SDTMIG v3.4 and controlled-terminology package 58; selection does not establish implementation or conformance. There are 500 generated subjects in Placebo, Drug 10 mg, and Drug 20 mg. Planned and actual arms match in the present data. Each subject has one generated exposure interval, 2025-01-01 through 2025-03-26 inclusive, with `EXDOSE=1`, `EXDOSU=TABLET`, and `EXDOSFRQ=QD`. The mg strengths reside in treatment labels, not in `EXDOSE`.

LB and VS visits are Baseline, Week 2, Week 4, Week 8, and Week 12 (`VISITNUM` 1–5), at days 0, 14, 28, 56, and 84 from first exposure. Baseline collections share the first exposure date; there are no collection or dosing times to establish predose status. AE dates are independent of scheduled visits. No protocol, SAP, actual randomization record, CRF, or consent record exists.

## Dataset navigation

Start with [SDTM specifications v1.0](ABC101_synthetic_SDTM_dataset_specifications_v1.0.md) for every ordered column and source mapping. [SDTM Define-XML metadata](../metadata/v1.0/sdtm/define.xml) links the five **CSV** files and provides corresponding variable descriptions; its scope is explained in the [metadata preparation report](ABC101_synthetic_define_xml_preparation_v1.0.md). Join domains on `USUBJID`; sequence keys identify individual records.

| Domain and grain | Key | Current rows | Dataset | Program | Generated input |
| --- | --- | ---: | --- | --- | --- |
| DM — one subject | `USUBJID` | 500 | [dm.csv](../data/sdtm/dm.csv) | [dm.R](../programs/sdtm/dm.R) | [demographics.csv](../data/raw/demographics.csv), [exposure.csv](../data/raw/exposure.csv), plus AE/LB/VS dates |
| AE — one adverse event | `USUBJID`, `AESEQ` | 750 | [ae.csv](../data/sdtm/ae.csv) | [ae.R](../programs/sdtm/ae.R) | [adverse_events.csv](../data/raw/adverse_events.csv) and DM |
| LB — one lab test per visit | `USUBJID`, `LBSEQ` | 10,000 | [lb.csv](../data/sdtm/lb.csv) | [lb.R](../programs/sdtm/lb.R) | [labs.csv](../data/raw/labs.csv) and DM |
| VS — one vital-sign test per visit | `USUBJID`, `VSSEQ` | 12,500 | [vs.csv](../data/sdtm/vs.csv) | [vs.R](../programs/sdtm/vs.R) | [vital_signs.csv](../data/raw/vital_signs.csv) and DM |
| EX — one exposure interval | `USUBJID`, `EXSEQ` | 500 | [ex.csv](../data/sdtm/ex.csv) | [ex.R](../programs/sdtm/ex.R) | [exposure.csv](../data/raw/exposure.csv) and DM |

## Traceability and derivations needing reviewer attention

- **Subjects and treatment:** DM retains the raw `USUBJID`; `SUBJID` removes its `ABC-` prefix. `ARM` comes from raw planned treatment; `ACTARM` comes from raw exposure treatment. DM `RFXSTDTC` and `RFXENDTC` come from exposure. `RFSTDTC`/`RFENDTC` are extrema of generated EX, AE, LB, and VS activity, **not** consent or participation dates.
- **Event records:** AE preserves raw `AESEQ`, term, dates, seriousness, and outcome. `AESEV` is uppercased. `AEDECOD` is merely uppercased `AETERM`; it is **not MedDRA coding**. The current AE `EPOCH` rule assigns `TREATMENT` for an onset within the exposure interval and `FOLLOW-UP` otherwise; there is no pre-treatment branch. Present generated event onsets do not precede first exposure.
- **Findings:** LB and VS preserve their source sequence and one result per subject/test/visit. Standard numeric results convert the generated result text without unit conversion. `LBBLFL`/`VSBLFL` is `Y` for the Baseline visit. Study days use DM `RFSTDTC` with no day zero. Same-day baseline results currently receive `EPOCH=TREATMENT` because timing within the day is unavailable.
- **Exposure:** EX carries one interval per subject, not administration-by-administration evidence. It cannot establish adherence, interruptions, dose changes, or actual mg ingested.

For record-level follow-through into analysis, use the [ADRG](ABC101_synthetic_ADRG_v1.0.md): ADSL joins DM and EX; ADAE, ADLB, and ADVS retain their source domain sequence in `SRCSEQ` as well as their own `ASEQ`.

## Validation evidence and open issues

The [SDTM cross-domain QC report](../output/qc/sdtm_validation_report.md) records **PASS, 45/45 checks** for the current five CSVs, including keys, subject links, dates, epochs, visits, and exposure. The [specification validation report](ABC101_dataset_spec_validation_v1.0.md) records an independent mapping check across all nine SDTM and ADaM CSVs: **182/182 documented columns and 48,000 output rows**. The [Define-XML preparation report](ABC101_synthetic_define_xml_preparation_v1.0.md) records 5/5 resolved SDTM CSV references and structural XSD validation with a schema whose terminology enumeration is older than selected package 58. These are internal/structural checks, not published SDTMIG, terminology, or FDA conformance checks.

Known gaps relevant to SDTM review: CSV rather than XPT transport; no validated package-58 codelist mapping; no MedDRA dictionary or coding trace; no real aCRF, source provenance, protocol, or consent; visit-based baseline without confirmed predose time; simplified DM reference dates, AE epoch logic, and EX exposure interval. The XML uses `def:Context="Other"` and links CSVs; its observed lengths are not designed XPT attributes. See the [metadata report](ABC101_synthetic_define_xml_preparation_v1.0.md) for further XML limitations. Other domains and full standards-rule conformance are outside this demonstration.

## Reproducing the documented checks

From the project root, `python qc/validate_dataset_specs.py` checks the specification-to-data mappings and `python qc/validate_define_xml.py` checks metadata and CSV references. [run_all.R](../run_all.R) rebuilds raw, SDTM, ADaM, and displays and reruns the internal QC gates; it **overwrites generated files**. The recorded SDTM report is produced by [validate_sdtm.R](../qc/validate_sdtm.R). The README gives setup and full execution order. This guide describes the committed current files and does not substitute for a conformance assessment.
