# ABC101 clinical R study

This project builds a fully synthetic, 500-subject, three-arm type 2 diabetes study from raw CSVs through SDTM, ADaM, and safety tables, a listing, and a figure. The three treatment labels are Placebo, Drug 10 mg, and Drug 20 mg. All source data are generated locally; no patient data or external data files are needed.

The [versioned synthetic study design and standards assumptions](docs/ABC101_synthetic_study_design_and_standards_v1.0.md) document records the assumed study start date, FDA-supported standards selected for the demonstration, treatment and visit schedule, analysis rules, and limitations. It is not a protocol or a claim that the datasets conform to those standards.

The [synthetic SDTM specifications](docs/ABC101_synthetic_SDTM_dataset_specifications_v1.0.md) and [synthetic ADaM specifications](docs/ABC101_synthetic_ADaM_dataset_specifications_v1.0.md) document the current CSV columns, source mappings, and derivations. Their [validation and gaps report](docs/ABC101_dataset_spec_validation_v1.0.md) records the cell-level checks and remaining limitations. Run the read-only checker from the project root with `python qc/validate_dataset_specs.py`.

The versioned synthetic Define-XML metadata files are [SDTM](metadata/v1.0/sdtm/define.xml) and [ADaM](metadata/v1.0/adam/define.xml). Their [preparation and validation report](docs/ABC101_synthetic_define_xml_preparation_v1.0.md) explains the supported metadata, schema check, and open gaps. Regenerate with `python programs/metadata/generate_define_xml.py`; check specification and dataset references with `python qc/validate_define_xml.py`. These XML files describe CSVs for a non-submission demonstration.

The versioned synthetic reviewer guides are the [clinical Study Data Reviewer's Guide (cSDRG) v1.0](docs/ABC101_synthetic_cSDRG_v1.0.md) for raw-to-SDTM navigation and the [Analysis Data Reviewer's Guide (ADRG) v1.0](docs/ABC101_synthetic_ADRG_v1.0.md) for ADaM and Tables 1–4, Listing 1, and Figure 1. They describe the committed synthetic files, source traceability, QC evidence, and known gaps. Neither guide is a protocol, SAP, or submission-readiness claim.

For a limited, read-only standards screening, run `python qc/validate_conformance_v1_0.py`. The versioned [findings report](output/qc/ABC101_synthetic_conformance_findings_v1.0.md) identifies the relevant CDISC and FDA rule sources, the eight local checks supported by the present CSVs and Define-XML, and the untested rule areas and submission gaps. This does not execute the official rule workbooks or establish conformance. An optional local Define-XML 2.1 entry schema may be supplied with `--schema PATH` for structural XSD validation.

The versioned [Quarto source](reports/ABC101_synthetic_review_v1.0.qmd) renders to a [synthetic HTML review report](reports/ABC101_synthetic_review_v1.0.html). It includes the generated Tables 1–4, links to the full Listing 1 and Figure 1 summary, embeds the current Figure 1 PNG, and summarizes internal QC and open conformance gaps. Its [read-only report checker](qc/validate_quarto_report_v1_0.py) verifies the rendered tables, image bytes, quoted counts, QC results, and local links against the current outputs.

## Requirements

- R with base packages and PNG graphics support. The full workflow was tested with R 4.5.1. No contributed R packages are required; in particular, `gtsummary::tbl_summary()` is not used.
- Python 3 on `PATH` for independent final TLF QC. The workflow was tested with Python 3.12.4 and uses only its standard library.
- Quarto for the optional HTML review report. Rendering was tested with Quarto 1.3.353 bundled with RStudio; Quarto is not needed by `run_all.R`.
- A writable checkout. The runner creates the output directories if needed and overwrites generated files.

## Run from a fresh checkout

From the `clinical-r-study` directory, run:

```sh
Rscript run_all.R
```

The runner also works when called by its path from another directory. On Windows, if R is installed but `Rscript` is not on `PATH`, use its full path in PowerShell, for example:

```powershell
& 'C:\Program Files\R\R-4.5.1\bin\Rscript.exe' .\run_all.R
```

`run_all.R` executes each script in the following order and stops at the first error or failed QC gate:

| Stage | Programs, in execution order |
| --- | --- |
| Raw | `R/data_generation/generate_raw_demographics.R`, `generate_adverse_events.R`, `generate_labs.R`, `generate_vital_signs.R`, `generate_exposure.R` (the latter four are in the same directory) |
| SDTM | `programs/sdtm/dm.R`, `ae.R`, `lb.R`, `vs.R`, `ex.R`; then `qc/validate_sdtm.R` |
| ADaM | `programs/adam/adsl.R`, `adae.R`, `adlb.R`, `advs.R`; then `qc/validate_adam.R` |
| Displays | `programs/tlf/table_01_demographics.R`, `table_02_teae.R`, `table_03_laboratory.R`, `table_04_vital_signs.R`, `listing_01_subject_ae.R`, `figure_01_weight_change.R` |
| Final QC | `qc/validate_tlf.py`, run with Python 3 |

The SDTM and ADaM gates check cross-domain/source consistency and derivations. Final TLF QC independently recalculates results from ADaM and checks the displayed tables, listing, and figure summary. A successful automated run reports 45 SDTM checks, 52 ADaM checks, and 35 final TLF checks, all passing.

## Outputs

| Location | Contents |
| --- | --- |
| `data/raw/` | Demographics, adverse events, labs, vital signs, exposure CSVs |
| `data/sdtm/` | DM, AE, LB, VS, EX CSVs |
| `data/adam/` | ADSL, ADAE, ADLB, ADVS CSVs |
| `output/tables/` | Tables 1–4 as CSV and Markdown |
| `output/listings/` | Subject-level AE listing as CSV and Markdown |
| `output/figures/` | Figure 1 as PNG and its plotted summary CSV |
| `output/qc/` | SDTM, ADaM, and final TLF validation reports as Markdown |
| `reports/` | Versioned Quarto source and rendered synthetic HTML review report |

The PNG is also reviewed visually when preparing a final result. The automated runner does not claim a visual inspection. After inspecting the exact image, an operator can bind that review to the PNG by passing its SHA-256 hash to `python qc/validate_tlf.py --reviewed-figure-sha256 <hash>`; this adds the `F1-VISUAL` check to the report. The optional HTML files that may exist in `output/` are not produced by the runner.

## Render the synthetic review report

From the project root, after the generated outputs and QC reports exist, run:

```sh
python qc/validate_conformance_v1_0.py
quarto render reports/ABC101_synthetic_review_v1.0.qmd --to html
python qc/validate_quarto_report_v1_0.py
```

On Windows, if Quarto is installed with RStudio but not on `PATH`, replace the `quarto` command with `& 'C:\Program Files\RStudio\resources\app\bin\quarto\bin\quarto.exe'` in PowerShell. The Quarto report is a snapshot of the current outputs; rerender it after regenerating any table, listing, figure, or QC report. The checked-in snapshot cites the current **36/36** TLF QC report, which includes a visual attestation for the exact Figure 1 PNG. A fresh `run_all.R` run records **35/35** automated TLF checks. To reproduce the visual-attestation count after a fresh run, inspect the new PNG and pass its SHA-256 hash to `qc/validate_tlf.py` as described above before rendering. The report checker will flag a stale snapshot if the QC count or embedded PNG differs.

## Scope and limitations

This is an educational programming example, not a clinical study, validated submission package, or basis for medical conclusions. The subjects, treatments, events, measurements, and results are synthetic. The generators use fixed random seeds; byte-for-byte results were checked with the tested R and Python versions above, but different R versions or graphics devices can produce different random samples or PNG bytes. Exposure is simplified to one daily tablet from 2025-01-01 through Week 12 for every subject, and actual treatment matches the planned assignment. The data and outputs cover selected safety domains and descriptive summaries only; they do not model clinical site operations, efficacy endpoints, or the full scope of CDISC submission metadata and controlled terminology.
