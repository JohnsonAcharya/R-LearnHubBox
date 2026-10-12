# ABC101 synthetic SDTM dataset specifications — v1.0

> **SYNTHETIC REVIEW DEMONSTRATION.** These are specifications of the **current CSV outputs and R programs**. They are not an approved SDTM mapping plan, Define-XML, or a statement of SDTMIG conformance.

| Control | Value |
| --- | --- |
| Study | ABC101 |
| Specification version / date | 1.0 / 2026-10-13 |
| Standards target | [Study-design assumptions v1.0](ABC101_synthetic_study_design_and_standards_v1.0.md): SDTM v2.0 / SDTMIG v3.4; terminology package 58 |
| Output location | `data/sdtm/*.csv`; one header row, empty string for missing flags |
| Source convention | `raw.DM` = `data/raw/demographics.csv`; `raw.AE`, `raw.LB`, `raw.VS`, and `raw.EX` name the analogous raw files. `DM` below means the generated SDTM DM CSV. |
| Type convention | C = character; N = numeric in the R output; ISO = ISO 8601 date text. CSV has no enforced physical data types or lengths. |

The output order in each variable table is the CSV column order. “Copy” means retain the source value without recoding. The study-day function used by AE, LB, VS, and EX is `date - DM.RFSTDTC + 1` on/after the reference date and `date - DM.RFSTDTC` before it; there is no day zero. Joins use `USUBJID`. All domains set `STUDYID = ABC101`; the domain code is assigned by its R program. These project specifications describe only present columns, values, and logic.

## DM — Demographics

**Program:** [dm.R](../programs/sdtm/dm.R). **Grain/key:** one row per `USUBJID`. **Sources:** raw.DM, raw.EX, raw.AE, raw.LB, raw.VS. **Current rows:** 500.

| Variable | Type | Label | Source / mapping or derivation |
| --- | --- | --- | --- |
| STUDYID | C | Study identifier | Constant `ABC101`. |
| DOMAIN | C | Domain abbreviation | Constant `DM`. |
| USUBJID | C | Unique subject identifier | Copy raw.DM.USUBJID. |
| SUBJID | C | Subject identifier | Remove leading `ABC-` from raw.DM.USUBJID; leading zeros remain. |
| RFSTDTC | ISO | Subject reference start date | Earliest of raw.EX.EXSTDTC, raw.AE.AESTDTC, raw.LB.LBDTC, and raw.VS.VSDTC for the subject. |
| RFENDTC | ISO | Subject reference end date | Latest of raw.EX.EXENDTC, raw.AE.AEENDTC, raw.LB.LBDTC, and raw.VS.VSDTC for the subject. |
| RFXSTDTC | ISO | First treatment date | Copy raw.EX.EXSTDTC by subject. |
| RFXENDTC | ISO | Last treatment date | Copy raw.EX.EXENDTC by subject. |
| AGE | N | Age | Copy raw.DM.AGE. |
| AGEU | C | Age unit | Constant `YEARS`. |
| SEX | C | Sex | Copy raw.DM.SEX (`M` or `F`). |
| ARMCD | C | Planned arm code | Map raw.DM.TRT01P: Placebo→`PBO`, Drug 10 mg→`DRUG10`, Drug 20 mg→`DRUG20`. |
| ARM | C | Planned arm | Copy raw.DM.TRT01P. |
| ACTARMCD | C | Actual arm code | Apply the ARMCD mapping to raw.EX.EXTRT. |
| ACTARM | C | Actual arm | Copy raw.EX.EXTRT by subject. |

`RFSTDTC`/`RFENDTC` are extrema of generated activity, not consent or actual study participation dates. All current `RFSTDTC` values happen to equal first exposure, but the algorithm is broader.

## AE — Adverse Events

**Program:** [ae.R](../programs/sdtm/ae.R). **Grain/key:** one source event; `USUBJID + AESEQ`. **Sources:** raw.AE and DM. **Current rows:** 750. All raw AE records are retained.

| Variable | Type | Label | Source / mapping or derivation |
| --- | --- | --- | --- |
| STUDYID | C | Study identifier | Constant `ABC101`. |
| DOMAIN | C | Domain abbreviation | Constant `AE`. |
| USUBJID | C | Unique subject identifier | Copy raw.AE.USUBJID. |
| AESEQ | N | Sequence number | Integer conversion of raw.AE.AESEQ. |
| AETERM | C | Reported event term | Copy raw.AE.AETERM. |
| AEDECOD | C | Decoded term placeholder | Uppercase raw.AE.AETERM. **No MedDRA coding occurs.** |
| AESEV | C | Severity | Uppercase raw.AE.AESEV (`MILD`, `MODERATE`, `SEVERE`). |
| AESER | C | Serious event | Copy raw.AE.AESER (`Y` or `N`). |
| AEOUT | C | Outcome | Copy raw.AE.AEOUT. |
| AESTDTC | ISO | Event start date | Copy raw.AE.AESTDTC. |
| AEENDTC | ISO | Event end date | Copy raw.AE.AEENDTC. |
| AESTDY | N | Event start study day | Study-day function on AESTDTC relative to DM.RFSTDTC. |
| AEENDY | N | Event end study day | Study-day function on AEENDTC relative to DM.RFSTDTC. |
| EPOCH | C | Event epoch | `TREATMENT` if AESTDTC is within DM.RFXSTDTC–RFXENDTC inclusive; otherwise `FOLLOW-UP`. There is no PRE-TREATMENT branch in this program. |

## LB — Laboratory Test Results

**Program:** [lb.R](../programs/sdtm/lb.R). **Grain/key:** one result per subject, test, visit; `USUBJID + LBSEQ`. **Sources:** raw.LB and DM. **Current rows:** 10,000.

| Variable | Type | Label | Source / mapping or derivation |
| --- | --- | --- | --- |
| STUDYID | C | Study identifier | Constant `ABC101`. |
| DOMAIN | C | Domain abbreviation | Constant `LB`. |
| USUBJID | C | Unique subject identifier | Copy raw.LB.USUBJID. |
| LBSEQ | N | Sequence number | Integer conversion of raw.LB.LBSEQ. |
| LBTESTCD | C | Lab test code | Copy raw.LB.LBTESTCD. |
| LBTEST | C | Lab test name | Copy raw.LB.LBTEST. |
| LBCAT | C | Lab category | `ALT`, `AST`, `CREAT`→`CHEMISTRY`; `HGB`→`HEMATOLOGY`. |
| LBORRES | C | Original result | Copy raw.LB.LBORRES as text. |
| LBORRESU | C | Original result unit | Copy raw.LB.LBORRESU. |
| LBSTRESC | C | Standard result, character | Copy raw.LB.LBORRES; no unit conversion. |
| LBSTRESN | N | Standard result, numeric | Numeric conversion of raw.LB.LBORRES. |
| LBSTRESU | C | Standard unit | `ALT`/`AST`→`U/L`; `CREAT`→`mg/dL`; `HGB`→`g/dL`. Must match original unit. |
| LBBLFL | C | Baseline record flag | `Y` for `VISIT = Baseline`; empty otherwise. |
| VISITNUM | N | Visit number | Baseline→1, Week 2→2, Week 4→3, Week 8→4, Week 12→5. |
| VISIT | C | Visit name | Copy raw.LB.VISIT. |
| LBDTC | ISO | Collection date | Copy raw.LB.LBDTC. |
| LBDY | N | Collection study day | Study-day function on LBDTC relative to DM.RFSTDTC. |
| EPOCH | C | Collection epoch | `PRE-TREATMENT` before DM.RFXSTDTC; `TREATMENT` through DM.RFXENDTC inclusive; `FOLLOW-UP` afterward. |

## VS — Vital Signs

**Program:** [vs.R](../programs/sdtm/vs.R). **Grain/key:** one result per subject, test, visit; `USUBJID + VSSEQ`. **Sources:** raw.VS and DM. **Current rows:** 12,500.

| Variable | Type | Label | Source / mapping or derivation |
| --- | --- | --- | --- |
| STUDYID | C | Study identifier | Constant `ABC101`. |
| DOMAIN | C | Domain abbreviation | Constant `VS`. |
| USUBJID | C | Unique subject identifier | Copy raw.VS.USUBJID. |
| VSSEQ | N | Sequence number | Integer conversion of raw.VS.VSSEQ. |
| VSTESTCD | C | Vital-sign test code | Copy raw.VS.VSTESTCD. |
| VSTEST | C | Vital-sign test name | Copy raw.VS.VSTEST. |
| VSCAT | C | Vital-sign category | Constant `VITAL SIGNS`. |
| VSORRES | C | Original result | Copy raw.VS.VSORRES as text. |
| VSORRESU | C | Original result unit | Copy raw.VS.VSORRESU. |
| VSSTRESC | C | Standard result, character | Copy raw.VS.VSORRES; no unit conversion. |
| VSSTRESN | N | Standard result, numeric | Numeric conversion of raw.VS.VSORRES. |
| VSSTRESU | C | Standard unit | `SYSBP`/`DIABP`→`mmHg`; `PULSE`→`bpm`; `TEMP`→`C`; `WEIGHT`→`kg`. Must match original unit. |
| VSBLFL | C | Baseline record flag | `Y` for `VISIT = Baseline`; empty otherwise. |
| VISITNUM | N | Visit number | Baseline→1, Week 2→2, Week 4→3, Week 8→4, Week 12→5. |
| VISIT | C | Visit name | Copy raw.VS.VISIT. |
| VSDTC | ISO | Collection date | Copy raw.VS.VSDTC. |
| VSDY | N | Collection study day | Study-day function on VSDTC relative to DM.RFSTDTC. |
| EPOCH | C | Collection epoch | `PRE-TREATMENT` before DM.RFXSTDTC; `TREATMENT` through DM.RFXENDTC inclusive; `FOLLOW-UP` afterward. |

## EX — Exposure

**Program:** [ex.R](../programs/sdtm/ex.R). **Grain/key:** one interval per subject; `USUBJID + EXSEQ`. **Sources:** raw.EX and DM. **Current rows:** 500.

| Variable | Type | Label | Source / mapping or derivation |
| --- | --- | --- | --- |
| STUDYID | C | Study identifier | Constant `ABC101`. |
| DOMAIN | C | Domain abbreviation | Constant `EX`. |
| USUBJID | C | Unique subject identifier | Copy raw.EX.USUBJID. |
| EXSEQ | N | Sequence number | Integer conversion of raw.EX.EXSEQ. |
| EXTRT | C | Treatment name | Copy raw.EX.EXTRT; checked against DM.ACTARM. |
| EXCAT | C | Exposure category | Constant `STUDY TREATMENT`. |
| EXDOSE | N | Dose | Numeric conversion of raw.EX.EXDOSE (currently 1). |
| EXDOSU | C | Dose unit | Uppercase raw.EX.EXDOSU (currently `TABLET`). |
| EXDOSFRQ | C | Dose frequency | Copy raw.EX.EXDOSFRQ (currently `QD`). |
| EXSTDTC | ISO | Exposure start date | Copy raw.EX.EXSTDTC. |
| EXENDTC | ISO | Exposure end date | Copy raw.EX.EXENDTC. |
| EXSTDY | N | Exposure start study day | Study-day function on EXSTDTC relative to DM.RFSTDTC. |
| EXENDY | N | Exposure end study day | Study-day function on EXENDTC relative to DM.RFSTDTC. |
| EPOCH | C | Exposure epoch | Constant `TREATMENT`. |

## Scope notes

These five domains are the entire current synthetic SDTM layer. The selected standards versions are a future target; controlled-terminology compliance, external dictionaries, variable attributes such as XPT lengths/labels, missing required trial-design and other domains, and submission conformance remain unverified. The companion [validation and gaps report](ABC101_dataset_spec_validation_v1.0.md) records the checks and known gaps. No dataset is changed by these specifications.
