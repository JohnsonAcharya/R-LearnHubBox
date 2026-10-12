# ABC101 synthetic dataset specification validation and gaps — v1.0

> **SYNTHETIC REVIEW DEMONSTRATION.** This report checks whether the specifications describe the present programs and CSVs. It does not certify CDISC or FDA submission conformance.

| Control | Value |
| --- | --- |
| Report version / date | 1.0 / 2026-10-13 |
| Specifications | [SDTM v1.0](ABC101_synthetic_SDTM_dataset_specifications_v1.0.md); [ADaM v1.0](ABC101_synthetic_ADaM_dataset_specifications_v1.0.md) |
| Validator | [qc/validate_dataset_specs.py](../qc/validate_dataset_specs.py) |
| Command | `python qc/validate_dataset_specs.py` from the project root |
| Result | **PASS: 9 datasets, 182/182 documented columns, 48,000 output rows** |

## Verification method and results

The read-only validator parses every variable table in the two specifications and checks exact CSV column names and order. It reconstructs **every output value** from the appropriate raw, SDTM, or ADSL source, using independent Python expressions for joins, constant assignments, date arithmetic, categorical mappings, unit mappings, epochs, baseline selection, and analysis calculations. It checks source/output key equality and uniqueness; numeric outputs are compared at a tolerance of 1e-8, and identifiers, dates, flags, categories, and text are compared exactly. Blank analysis values and flags are checked explicitly. It creates or changes no datasets.

| Dataset | Rows | Documented columns | Mapping check |
| --- | ---: | ---: | --- |
| SDTM DM | 500 | 15 | PASS |
| SDTM AE | 750 | 14 | PASS |
| SDTM LB | 10,000 | 18 | PASS |
| SDTM VS | 12,500 | 18 | PASS |
| SDTM EX | 500 | 14 | PASS |
| ADaM ADSL | 500 | 15 | PASS |
| ADaM ADAE | 750 | 28 | PASS |
| ADaM ADLB | 10,000 | 30 | PASS |
| ADaM ADVS | 12,500 | 30 | PASS |

The earlier [SDTM QC report](../output/qc/sdtm_validation_report.md) and [ADaM QC report](../output/qc/adam_validation_report.md) also record passing cross-domain checks for the current outputs. They were inspected, not regenerated, for this documentation step. The new checker verifies the specification-to-output agreement at variable and record level; both methods are internal checks and do not test a published CDISC conformance rule set.

## Confirmed limitations and open gaps

| Priority | Gap | Evidence in current implementation | Implication for a future review demonstration |
| --- | --- | --- | --- |
| High | AE terminology is not medically coded. | SDTM AE derives `AEDECOD = toupper(AETERM)`; ADAE copies it. No MedDRA dictionary/version or coding trace exists. | Label `AEDECOD` as a placeholder until coded terms and their dictionary provenance can be represented honestly. |
| High | Baseline predose status is unknown. | LB/VS set `--BLFL = Y` from `VISIT = Baseline`; all baseline dates equal first exposure date, with no collection or dose time. | The current baseline and change rules are reproducible, but cannot demonstrate a confirmed predose baseline. |
| High | CDISC controlled terminology and conformance are unverified. | Package 58 is selected in the design assumptions; programs use local value mappings. Existing QC checks internal consistency, not package-58 codelists, SDTMIG/ADaMIG rules, or FDA validation rules. | Add standards-based terminology and conformance checks in a later step before any submission-format claim. |
| Medium | AE epoch logic lacks a pre-treatment case. | AE uses `TREATMENT` for starts in the exposure interval and `FOLLOW-UP` otherwise. Current generator starts AEs on/after first dose, so no current record is affected. | If pre-exposure AEs are introduced, the present rule would label them `FOLLOW-UP`; revise the logic then. |
| Medium | Study reference and population rules are simplified. | DM reference dates are extrema of generated activities, not consent dates. ADSL assigns `ITTFL = Y` and `SAFFL = Y` to all subjects by construction. | These are synthetic conventions, not evidence of real enrollment, randomization, or safety eligibility. |
| Medium | Exposure is an interval rather than administration-level evidence. | One EX row per subject spans 85 days, with `EXDOSE = 1` and unit `TABLET`. | No missed doses, interruptions, dose changes, or verified mg administered can be traced. |
| Medium | ADaM analysis-day reference varies by dataset. | ADAE recalculates `ASTDY`/`AENDY` from ADSL.TRTSDT; ADLB/ADVS copy SDTM `--DY`, which uses DM.RFSTDTC. Both reference dates happen to coincide now. | Record the distinct rules; reassess if source dates or study design change. |
| Later | Submission metadata and physical format are absent. | Outputs are CSVs. These specifications give logical types, labels, sources, and derivations but no validated XPT lengths, origin/method metadata, codelist identifiers, or machine-readable Define-XML. | Those details require separate design and conformance work. No Define-XML or datasets were produced or changed here. |

The gaps above are **not failed mapping checks**: the specifications correctly describe the current files. They identify where the synthetic implementation cannot support a stronger regulatory interpretation. The study-level assumptions and selected version baseline are in the [design document](ABC101_synthetic_study_design_and_standards_v1.0.md).
