# ABC101 synthetic Analysis Data Reviewer's Guide (ADRG) — v1.0

> **SYNTHETIC REVIEW DEMONSTRATION ONLY.** All subjects and results are generated. This guide explains the current ADaM-like CSVs and descriptive tables, listing, and figure. It is not an approved SAP, a clinical interpretation, a regulatory submission, or a claim of ADaM conformance.

| Document control | Value |
| --- | --- |
| Study identifier | ABC101 (synthetic) |
| Version / date | 1.0 / 2026-10-13 |
| Scope | Four ADaM CSVs, Tables 1–4, Listing 1, Figure 1, metadata, and internal QC |
| Change history | v1.0: initial synthetic ADRG |
| Companion | [Synthetic cSDRG v1.0](ABC101_synthetic_cSDRG_v1.0.md) |

## Analysis conventions

The [study-design and standards assumptions v1.0](ABC101_synthetic_study_design_and_standards_v1.0.md) select ADaM v2.1 / ADaMIG v1.3 and controlled-terminology package 58 as **targets**, not demonstrated conformance. ADSL assigns `ITTFL=Y` and `SAFFL=Y` to all 500 generated subjects; all have one EX interval. `TRT01P` comes from DM `ARM`, while `TRT01A` comes from DM `ACTARM`, checked against EX `EXTRT`. The arms match in the current data. Table 1 groups by **planned** treatment and `ITTFL`; safety summaries group by **actual** treatment and `SAFFL`.

Treatment starts 2025-01-01 and ends 2025-03-26 for all current subjects. A treatment-emergent AE has `ASTDT` from `TRTSDT` through `TRTEDT+30 days`, **inclusive** (current upper boundary 2025-04-25). Its end date, severity, and seriousness do not change `TRTEMFL`. All AEs remain in ADAE, including those with a blank treatment-emergent flag. LB/VS baseline is the single record flagged at the Baseline visit for each subject and test. Its value becomes `BASE` for that subject/test; postbaseline `CHG=round(AVAL-BASE,4)` and `PCHG=round(100*CHG/BASE,4)` when `BASE` is nonzero. Baseline `CHG`/`PCHG` are blank. No collection or dose times confirm that Baseline is predose.

## Analysis dataset navigation and source trace

The [ADaM specifications v1.0](ABC101_synthetic_ADaM_dataset_specifications_v1.0.md) list every output column and derivation. [ADaM Define-XML metadata](../metadata/v1.0/adam/define.xml) describes the current **CSV** files; the [metadata report](ABC101_synthetic_define_xml_preparation_v1.0.md) defines its limits. Join subject-level fields on `USUBJID`. For event/finding records, `ASEQ` equals the SDTM source sequence and `SRCDOM`/`SRCVAR`/`SRCSEQ` identify that source row.

| Dataset and grain | Current rows | Source and trace | Dataset | Program |
| --- | ---: | --- | --- | --- |
| ADSL — one subject, key `USUBJID` | 500 | SDTM [DM](../data/sdtm/dm.csv) + [EX](../data/sdtm/ex.csv); planned/actual arms and exposure dates | [adsl.csv](../data/adam/adsl.csv) | [adsl.R](../programs/adam/adsl.R) |
| ADAE — one AE, key `USUBJID+ASEQ` | 750 | SDTM [AE](../data/sdtm/ae.csv) + ADSL; `ASEQ=AESEQ`, `SRCSEQ=AESEQ` | [adae.csv](../data/adam/adae.csv) | [adae.R](../programs/adam/adae.R) |
| ADLB — one LB result, key `USUBJID+ASEQ` | 10,000 | SDTM [LB](../data/sdtm/lb.csv) + ADSL; `ASEQ=LBSEQ`, `SRCSEQ=LBSEQ`, baseline `BLSEQ` | [adlb.csv](../data/adam/adlb.csv) | [adlb.R](../programs/adam/adlb.R) |
| ADVS — one VS result, key `USUBJID+ASEQ` | 12,500 | SDTM [VS](../data/sdtm/vs.csv) + ADSL; `ASEQ=VSSEQ`, `SRCSEQ=VSSEQ`, baseline `BLSEQ` | [advs.csv](../data/adam/advs.csv) | [advs.R](../programs/adam/advs.R) |

For example, a Listing 1 row keyed by `USUBJID, ASEQ` locates one ADAE row; its `SRCSEQ` locates the AE row with the same subject and `AESEQ`, which in turn locates the generated raw event. For Table 3, identify its `PARAMCD`, `AVISITN`, and treatment, then inspect matching safety ADLB rows, their source `LBSEQ`, and their `BLSEQ` baseline rows. Figure 1 follows the same pattern through ADVS `PARAMCD=WEIGHT` and `CHG`. The analysis-day references differ: ADAE calculates `ASTDY`/`AENDY` from ADSL `TRTSDT`; ADLB/ADVS copy SDTM `--DY`, which is relative to DM `RFSTDTC`. Those reference dates coincide in the current data.

## Display navigation

Each table and the listing has both a machine-readable CSV and a Markdown display. No efficacy analysis or inferential testing is present.

| Display | Population, treatment, and calculation | Files | Program |
| --- | --- | --- | --- |
| Table 1 — demographics | `ITTFL=Y`, `TRT01P`; subject N, age in years, and sex; percentages use arm N | [CSV](../output/tables/table_01_demographics.csv), [Markdown](../output/tables/table_01_demographics.md) | [R](../programs/tlf/table_01_demographics.R) |
| Table 2 — TEAEs | `SAFFL=Y`, `TRT01A`, `TRTEMFL=Y`; distinct-subject incidence, TEAE record count, seriousness/severity, event terms; percentages use safety arm N | [CSV](../output/tables/table_02_teae.csv), [Markdown](../output/tables/table_02_teae.md) | [R](../programs/tlf/table_02_teae.R) |
| Table 3 — laboratory | `SAFFL=Y`, `TRT01A`; 4 tests, Baseline and Weeks 2/4/8/12; contributing subject n and mean (sample SD) for `AVAL`, and postbaseline `CHG`, with units | [CSV](../output/tables/table_03_laboratory.csv), [Markdown](../output/tables/table_03_laboratory.md) | [R](../programs/tlf/table_03_laboratory.R) |
| Table 4 — vital signs | `SAFFL=Y`, `TRT01A`; 5 tests and the same visits; contributing n and mean (sample SD) for `AVAL` and postbaseline `CHG`, with units | [CSV](../output/tables/table_04_vital_signs.csv), [Markdown](../output/tables/table_04_vital_signs.md) | [R](../programs/tlf/table_04_vital_signs.R) |
| Listing 1 — subject AEs | **All** 750 ADAE rows, including non-TEAEs; actual treatment, subject, event dates, severity, seriousness, outcome, TEAE status; sorted by actual arm, subject, start date, `ASEQ` | [CSV](../output/listings/listing_01_subject_ae.csv), [Markdown](../output/listings/listing_01_subject_ae.md) | [R](../programs/tlf/listing_01_subject_ae.R) |
| Figure 1 — weight change | `SAFFL=Y`, `PARAMCD=WEIGHT`, Weeks 2/4/8/12; mean `CHG` in kg and contributing n by actual treatment | [Summary CSV](../output/figures/figure_01_weight_change.csv), [PNG](../output/figures/figure_01_weight_change.png) | [R](../programs/tlf/figure_01_weight_change.R) |

The current Table 1 ITT and Table 2 safety arm denominators are Placebo 157, Drug 10 mg 166, and Drug 20 mg 177 (overall 500); their agreement follows the current equal planned/actual assignment and flags, not a general analysis rule. The current Table 2 has 473 TEAE records. Figure 1 has 12 arm/visit summary rows and displays the contributing counts below the plot.

## Validation evidence and known limits

The [ADaM QC report](../output/qc/adam_validation_report.md) records **PASS, 52/52 checks** for keys, source coverage, treatments, baseline, change, and TEAE derivations. The [dataset-specification validation report](ABC101_dataset_spec_validation_v1.0.md) records **182/182 columns and 48,000 rows** checked across SDTM and ADaM. The [Define-XML report](ABC101_synthetic_define_xml_preparation_v1.0.md) records 4/4 resolved ADaM CSV references and structural XSD validation, but not package-58 terminology validation. The [independent final TLF QC report](../output/qc/tlf_validation_report.md) records **PASS, 36/36 checks**: ADaM-based recalculations of Tables 1–4, all 750 listing records, 12 Figure 1 summary cells, displayed labels/units, PNG content checks, and a recorded visual review of the exact PNG with SHA-256 `26330733cab7c043748994ff2141bff557a9a5932dfc98e1147e95d8e0b158d3`. A standard automated run has 35 TLF checks; the 36th is the optional operator visual attestation tied to a PNG hash.

Known limits: there is no protocol or SAP to justify clinical endpoints, estimands, populations, censoring, or inferential methods; all 500 subjects are flagged for both populations by construction. `AEDECOD` is an uppercase term placeholder rather than a MedDRA preferred term. Baseline is visit-based with unverified predose timing. Exposure lacks adherence and actual mg administered. The XML uses non-submission CSV references and observed lengths, without XPT transport, package-58 codelist mappings, or analysis-results metadata (ARM). Existing checks test project logic and displayed output, **not full ADaM/CDISC/FDA conformance**. These limits preclude clinical or regulatory interpretation of the numeric results.

## Reproducing the documented checks

From the project root, `python qc/validate_dataset_specs.py` and `python qc/validate_define_xml.py` recheck mappings and metadata references without changing datasets. [validate_adam.R](../qc/validate_adam.R) and [validate_tlf.py](../qc/validate_tlf.py) generate the corresponding QC reports; the latter accepts `--reviewed-figure-sha256 <hash>` only after independent inspection of that exact PNG. [run_all.R](../run_all.R) rebuilds all generated data and displays and reruns QC, **overwriting generated files**. The README has the full execution order. This guide documents the current files; broader conformance checks are a separate later step.
