# ABC101 synthetic study design and standards assumptions — v1.0

> **SYNTHETIC REVIEW DEMONSTRATION ONLY.** This is a description of generated data and a target standards baseline. It is not a clinical protocol, statistical analysis plan, actual study record, claim of CDISC conformance, or FDA submission.

| Document control | Value |
| --- | --- |
| Study identifier | ABC101 |
| Document version | 1.0 |
| Issued | 2026-10-10 |
| Assumed study start for standards selection | 2025-01-01 |
| Scope | Existing raw, SDTM-like, ADaM-like, and safety-display demonstration |
| Change history | v1.0: initial explicit design and standards assumptions |

## Date convention and regulatory scope

For this demonstration, **2025-01-01 is the assumed study start date used solely to select standards versions**. It is the first generated exposure date for every subject. The project has no informed-consent, screening, enrollment, site, or protocol-start records, so this assumption cannot establish the start date of a real clinical study. The date must be reassessed if those records are ever modeled. FDA says that submitted study-data formats must be supported at the study start date; the version check below applies that rule retrospectively to this synthetic date. The March 2025 catalog is a later publication that records support-start dates; it is not represented as a January 2025 catalog snapshot. [FDA study-data submission page](https://www.fda.gov/industry/study-data-standards-resources/study-data-submission-cder-and-cber); [FDA Data Standards Catalog, March 2025](https://www.fda.gov/regulatory-information/search-fda-guidance-documents/data-standards-catalog).

## Selected standards baseline

The following are **target versions for a possible future review demonstration**, not versions already implemented or validated in the CSVs. FDA support was checked against the CBER/CDER entries in **FDA Data Standards Catalog v11.0**, March 2025, tabs “Submission Data Standards” and “Submission Data Terminologies.” “Support begins” is distinct from “requirement begins”; a version can be supported before its required-use date. The catalog shows no support-end date for the selected entries. Checked 2026-10-10 against the [FDA catalog workbook](https://www.fda.gov/media/185927/download) and the [CDISC Library release list](https://www.cdisc.org/ja/cdisc-library/available-content).

| Component | Selected version | FDA catalog basis for 2025-01-01 | Selection result |
| --- | --- | --- | --- |
| SDTM model and clinical implementation guide | SDTM **v2.0** with SDTMIG **v3.4** | “Submission Data Standards,” rows 37–38: support begins **2023-12-13**; requirement begins **2025-03-15**. CDISC pairs SDTMIG v3.4 with SDTM v2.0. | Both supported by the assumed start date. |
| ADaM model and implementation guide | ADaM **v2.1** with ADaMIG **v1.3** | Rows 6 and 9: ADaM v2.1 support shown as **ongoing**; ADaMIG v1.3 appears in the row supported from **2022-07-18**, with requirement beginning **2024-03-15**. | Model and guide supported by the assumed start date. |
| Dataset metadata exchange | Define-XML **v2.1**; CDISC schema package **v2.1.8** (2024-12-20) selected to align with terminology package 58 | Row 36: core v2.1 support begins **2020-07-07**; required-use dates are **2022-03-15** for NDA and **2023-03-15** for IND. CDISC identifies package v2.1.8 as its package-58 schema update. | Core version supported by the assumed start date; no Define-XML file exists here yet. |
| CDISC controlled terminology | **Package 58**, effective **2024-09-27**, for SDTM, ADaM, and Define-XML terminology | “Submission Data Terminologies,” row 14, permits CDISC terminology versions dated **2011-06-10 or later** for clinical study data. CDISC lists package 58 for all three families; package 59 is effective 2025-03-28. | Package 58 is the most recent listed release for these families before the assumed start date. It is a selected target, not a claim that current values were checked against it. |

CDISC version references: [SDTMIG v3.4](https://www.cdisc.org/standards/foundational/sdtmig/sdtmig-v3-4), [ADaMIG v1.3](https://www.cdisc.org/standards/foundational/adam/adamig-v1-3), [Define-XML v2.1](https://www.cdisc.org/standards/foundational/define-xml/define-xml-v2-1-0), and its [v2.1.8 schema package](https://www.cdisc.org/standards/data-exchange/define-xml/define-xml-v2-1-8). The catalog workbook, rather than these CDISC release pages, is the source of the FDA support dates.

## Synthetic treatment and visit design

- **Subjects and indication:** 500 generated subject IDs, `ABC-001` through `ABC-500`, in an illustrative type 2 diabetes study. There is no real recruitment, diagnosis verification, or randomization record.
- **Treatment arms:** Placebo, Drug 10 mg, and Drug 20 mg. `TRT01P` is sampled in raw demographics. One EX record per subject copies that label to `EXTRT`; DM `ACTARM` and ADSL `TRT01A` derive from EX. Thus planned and actual treatments coincide for every generated subject. Dose is recorded as **one tablet, once daily (`QD`)**, for each arm. The mg strength is in the arm label; the EX dose field does not contain a numeric mg dose.
- **Exposure:** all subjects have `TRTSDT`/`EXSTDTC` **2025-01-01** and `TRTEDT`/`EXENDTC` **2025-03-26** (day 84). The inclusive analysis duration is 85 calendar days. This is a single generated exposure interval, not dose-by-dose adherence evidence.
- **Scheduled measurements:** LB and VS each contain one result per subject, test, and visit. The visit schedule below is generated from offsets from 2025-01-01. Visit numbers 1–5 are used in SDTM and ADaM.

| Visit | Visit number | Offset (days) | Calendar date |
| --- | ---: | ---: | --- |
| Baseline | 1 | 0 | 2025-01-01 |
| Week 2 | 2 | 14 | 2025-01-15 |
| Week 4 | 3 | 28 | 2025-01-29 |
| Week 8 | 4 | 56 | 2025-02-26 |
| Week 12 | 5 | 84 | 2025-03-26 |

AE start dates are generated independently from day 0 through day 180, with an end date 1–14 days later. AE records do not follow the measurement visit schedule.

## Analysis populations and derivation conventions

- **Intent-to-treat (`ITTFL`):** `Y` for all 500 generated DM/ADSL subjects. This is a project convention because all generated subjects are assigned an arm; it does not demonstrate a real randomized population. Table 1 uses planned treatment and this flag.
- **Safety (`SAFFL`):** `Y` for all 500 generated subjects because each has a generated EX record. Safety summaries use actual treatment (`TRT01A`). There is no separate efficacy or per-protocol population.
- **LB and VS baseline:** SDTM `LBBLFL`/`VSBLFL` is `Y` exactly when `VISIT` is `Baseline`. ADLB/ADVS copy this to `ABLFL` and use the single baseline result for each subject and test as `BASE`. For postbaseline records, `CHG = round(AVAL - BASE, 4)`; `PCHG = round(100 × CHG / BASE, 4)` when `BASE` is nonzero, otherwise missing. Baseline rows have missing `CHG` and `PCHG`. This is a visit-based baseline rule. The baseline date equals the first exposure date, and neither collection time nor dose time is present, so **predose timing is unverified**. SDTM currently assigns these same-day records to `EPOCH = TREATMENT`.
- **Treatment-emergent AE (`TRTEMFL`):** `Y` when the AE **start date** is on or after `TRTSDT` and on or before `TRTEDT + 30 days`, inclusive. With the fixed exposure dates, the window is **2025-01-01 through 2025-04-25**. AEs outside it remain in ADAE with an empty `TRTEMFL`; event end date, severity, and seriousness do not change the flag. Table 2 uses treatment-emergent events in the safety population; Listing 1 retains all ADAE events.

## Limitations and boundary of this document

The source values are generated with fixed seeds for programming practice. Treatment assignment is sampled rather than produced by a trial randomization system. Exposure does not capture missed doses, dose changes, or actual mg taken. Visits are complete and fixed; there is no visit window, unscheduled assessment, missing-data strategy, or source-document audit trail. Lab and vital-sign values have simple units and no clinical reference ranges or medically qualified interpretation. AE preferred terms are uppercase source terms, not MedDRA-coded terms; no MedDRA version has been selected. A CDISC terminology release has been pinned above, but the current values have **not** been mapped to or validated against its codelists.

Only DM, AE, LB, VS, and EX and the corresponding ADSL, ADAE, ADLB, and ADVS CSVs are present. No protocol, statistical analysis plan, case report forms, consent records, actual patient data, XPT transport files, Define-XML, or reviewer guides exist in this project. Existing program QC checks internal consistency and TLF calculations; it does not establish FDA submission readiness or full CDISC conformance. This document deliberately contains study-level assumptions only; dataset specifications are outside its scope.

## Verification record

On 2026-10-10, a read-only check compared the statements above with the generator and derivation code and with the existing raw, SDTM, and ADaM CSVs. It confirmed 500 unique subjects with matching planned/actual arm labels and exposure dates, five measurement visits with the listed dates and flags, 10,000 LB and 12,500 VS rows with the stated baseline and change calculations, and 750 ADAE rows with `TRTEMFL` matching the inclusive start-date window. The FDA catalog entries and CDISC release dates were checked separately against the linked primary sources above. This verification is limited to documentation accuracy; it is not a standards conformance assessment.

## Local implementation references

The statements above were checked against [raw demographics](../R/data_generation/generate_raw_demographics.R), [raw adverse events](../R/data_generation/generate_adverse_events.R), [raw labs](../R/data_generation/generate_labs.R), [raw vital signs](../R/data_generation/generate_vital_signs.R), [raw exposure](../R/data_generation/generate_exposure.R), [SDTM LB](../programs/sdtm/lb.R), [SDTM VS](../programs/sdtm/vs.R), [ADSL](../programs/adam/adsl.R), [ADAE](../programs/adam/adae.R), [ADLB](../programs/adam/adlb.R), and [ADVS](../programs/adam/advs.R), and against their existing CSV outputs. The version selection records a future documentation target; it does not alter those programs or outputs.
