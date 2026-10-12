# ABC101 synthetic ADaM dataset specifications — v1.0

> **SYNTHETIC REVIEW DEMONSTRATION.** These are specifications of the **current CSV outputs and R programs**. They are not an approved analysis plan, Define-XML, or a statement of ADaM conformance.

| Control | Value |
| --- | --- |
| Study | ABC101 |
| Specification version / date | 1.0 / 2026-10-13 |
| Standards target | [Study-design assumptions v1.0](ABC101_synthetic_study_design_and_standards_v1.0.md): ADaM v2.1 / ADaMIG v1.3; terminology package 58 |
| Output location | `data/adam/*.csv`; one header row, empty string for missing flags and analysis values |
| Source convention | `DM`, `AE`, `LB`, and `VS` are the corresponding `data/sdtm/` files; `EX` is SDTM exposure; `ADSL` is `data/adam/adsl.csv`. |
| Type convention | C = character; N = numeric in the R output; Date = R Date serialized as ISO date text. CSV has no enforced physical data types or lengths. |

The output order in each variable table is the CSV column order. All joins use `USUBJID`; event and finding traceability uses the source sequence. “Copy” means retain the source value. These project specifications document the current calculations, not prospective clinical rules.

## ADSL — Subject-Level Analysis Dataset

**Program:** [adsl.R](../programs/adam/adsl.R). **Grain/key:** one row per `USUBJID`. **Sources:** DM and EX. **Current rows:** 500. Numeric treatment codes are sponsor-defined: Placebo=0, Drug 10 mg=1, Drug 20 mg=2.

| Variable | Type | Label | Source / mapping or derivation |
| --- | --- | --- | --- |
| STUDYID | C | Study identifier | Copy DM.STUDYID. |
| USUBJID | C | Unique subject identifier | Copy DM.USUBJID. |
| SUBJID | C | Subject identifier | Copy DM.SUBJID. |
| AGE | N | Age | Integer conversion of DM.AGE. |
| AGEU | C | Age unit | Copy DM.AGEU. |
| SEX | C | Sex | Copy DM.SEX. |
| TRT01P | C | Planned treatment | Copy DM.ARM. |
| TRT01PN | N | Planned treatment code | Map DM.ARM to sponsor codes 0/1/2. |
| TRT01A | C | Actual treatment | Copy DM.ACTARM; must equal EX.EXTRT. |
| TRT01AN | N | Actual treatment code | Map DM.ACTARM to sponsor codes 0/1/2. |
| TRTSDT | Date | Treatment start date | Convert EX.EXSTDTC to Date; checked against DM.RFXSTDTC. |
| TRTEDT | Date | Treatment end date | Convert EX.EXENDTC to Date; checked against DM.RFXENDTC. |
| TRTDURD | N | Treatment duration in days | `TRTEDT - TRTSDT + 1` (inclusive; currently 85). |
| ITTFL | C | Intent-to-treat flag | Constant `Y` for every generated subject. |
| SAFFL | C | Safety flag | Constant `Y` for every generated, exposed subject. |

## ADAE — Adverse Events Analysis Dataset

**Program:** [adae.R](../programs/adam/adae.R). **Grain/key:** one source AE per `USUBJID + ASEQ`. **Sources:** AE and ADSL. **Current rows:** 750. All source AEs are retained, including non-treatment-emergent events. Join AE to ADSL by `USUBJID`; `ASEQ = AESEQ`.

| Variable | Type | Label | Source / mapping or derivation |
| --- | --- | --- | --- |
| STUDYID | C | Study identifier | Copy AE.STUDYID. |
| USUBJID | C | Unique subject identifier | Copy AE.USUBJID. |
| SUBJID | C | Subject identifier | Copy ADSL.SUBJID. |
| TRT01P | C | Planned treatment | Copy ADSL.TRT01P. |
| TRT01PN | N | Planned treatment code | Integer conversion of ADSL.TRT01PN. |
| TRT01A | C | Actual treatment | Copy ADSL.TRT01A. |
| TRT01AN | N | Actual treatment code | Integer conversion of ADSL.TRT01AN. |
| TRTSDT | Date | Treatment start date | Convert ADSL.TRTSDT to Date. |
| TRTEDT | Date | Treatment end date | Convert ADSL.TRTEDT to Date. |
| SAFFL | C | Safety flag | Copy ADSL.SAFFL. |
| ASEQ | N | Analysis sequence | Integer conversion of AE.AESEQ. |
| AESEQ | N | Source AE sequence | Integer conversion of AE.AESEQ. |
| AETERM | C | Reported event term | Copy AE.AETERM. |
| AEDECOD | C | Event term placeholder | Copy AE.AEDECOD; inherited uppercase term, not MedDRA coding. |
| AESEV | C | Severity | Copy AE.AESEV. |
| AESER | C | Serious event | Copy AE.AESER. |
| AEOUT | C | Outcome | Copy AE.AEOUT. |
| AESTDTC | C | Source start date text | Copy AE.AESTDTC. |
| AEENDTC | C | Source end date text | Copy AE.AEENDTC. |
| ASTDT | Date | Analysis start date | Convert AE.AESTDTC to Date. |
| AENDT | Date | Analysis end date | Convert AE.AEENDTC to Date. |
| ASTDY | N | Analysis start day | Relative to ADSL.TRTSDT: `date - TRTSDT + 1` on/after treatment start, `date - TRTSDT` before it. |
| AENDY | N | Analysis end day | Apply the same treatment-relative rule to AENDT. |
| EPOCH | C | Event epoch | Copy AE.EPOCH. |
| TRTEMFL | C | Treatment-emergent flag | `Y` when `TRTSDT ≤ ASTDT ≤ TRTEDT + 30 days`; empty otherwise. Inclusive boundary; based on event start only. |
| SRCDOM | C | Source domain | Constant `AE`. |
| SRCVAR | C | Source sequence variable | Constant `AESEQ`. |
| SRCSEQ | N | Source sequence value | Integer conversion of AE.AESEQ. |

## ADLB — Laboratory Analysis Dataset

**Program:** [adlb.R](../programs/adam/adlb.R). **Grain/key:** one LB record per `USUBJID + ASEQ`; `ASEQ = LBSEQ`. **Sources:** LB and ADSL. **Current rows:** 10,000. The baseline lookup key is `USUBJID + LBTESTCD`; exactly one LB row flagged `LBBLFL = Y` supplies `BASE` and `BLSEQ` to each record of that subject/test.

| Variable | Type | Label | Source / mapping or derivation |
| --- | --- | --- | --- |
| STUDYID | C | Study identifier | Copy LB.STUDYID. |
| USUBJID | C | Unique subject identifier | Copy LB.USUBJID. |
| SUBJID | C | Subject identifier | Copy ADSL.SUBJID. |
| TRT01P | C | Planned treatment | Copy ADSL.TRT01P. |
| TRT01PN | N | Planned treatment code | Integer conversion of ADSL.TRT01PN. |
| TRT01A | C | Actual treatment | Copy ADSL.TRT01A. |
| TRT01AN | N | Actual treatment code | Integer conversion of ADSL.TRT01AN. |
| TRTSDT | Date | Treatment start date | Convert ADSL.TRTSDT to Date. |
| TRTEDT | Date | Treatment end date | Convert ADSL.TRTEDT to Date. |
| ITTFL | C | Intent-to-treat flag | Copy ADSL.ITTFL. |
| SAFFL | C | Safety flag | Copy ADSL.SAFFL. |
| ASEQ | N | Analysis sequence | Integer conversion of LB.LBSEQ. |
| LBSEQ | N | Source LB sequence | Integer conversion of LB.LBSEQ. |
| PARAMCD | C | Analysis parameter code | Copy LB.LBTESTCD. |
| PARAM | C | Analysis parameter | Copy LB.LBTEST. |
| PARCAT1 | C | Parameter category | Copy LB.LBCAT. |
| AVAL | N | Analysis value | Numeric conversion of LB.LBSTRESN. |
| AVALU | C | Analysis value unit | Copy LB.LBSTRESU. |
| ADT | Date | Analysis date | Convert LB.LBDTC to Date. |
| ADY | N | Analysis day | Integer conversion of LB.LBDY; this is already relative to DM.RFSTDTC. |
| AVISIT | C | Analysis visit | Copy LB.VISIT. |
| AVISITN | N | Analysis visit number | Numeric conversion of LB.VISITNUM. |
| ABLFL | C | Baseline flag | Copy LB.LBBLFL (`Y` or empty). |
| BLSEQ | N | Baseline source sequence | LBSEQ of the baseline row for subject + LBTESTCD. |
| BASE | N | Baseline value | LBSTRESN of that baseline row. |
| CHG | N | Change from baseline | Empty at baseline; postbaseline `round(AVAL - BASE, 4)`. |
| PCHG | N | Percent change from baseline | Empty at baseline or if BASE=0; otherwise `round(100 × CHG / BASE, 4)`, using the already rounded CHG. |
| SRCDOM | C | Source domain | Constant `LB`. |
| SRCVAR | C | Source sequence variable | Constant `LBSEQ`. |
| SRCSEQ | N | Source sequence value | Integer conversion of LB.LBSEQ. |

## ADVS — Vital Signs Analysis Dataset

**Program:** [advs.R](../programs/adam/advs.R). **Grain/key:** one VS record per `USUBJID + ASEQ`; `ASEQ = VSSEQ`. **Sources:** VS and ADSL. **Current rows:** 12,500. The baseline lookup key is `USUBJID + VSTESTCD`; exactly one VS row flagged `VSBLFL = Y` supplies `BASE` and `BLSEQ` to each record of that subject/test.

| Variable | Type | Label | Source / mapping or derivation |
| --- | --- | --- | --- |
| STUDYID | C | Study identifier | Copy VS.STUDYID. |
| USUBJID | C | Unique subject identifier | Copy VS.USUBJID. |
| SUBJID | C | Subject identifier | Copy ADSL.SUBJID. |
| TRT01P | C | Planned treatment | Copy ADSL.TRT01P. |
| TRT01PN | N | Planned treatment code | Integer conversion of ADSL.TRT01PN. |
| TRT01A | C | Actual treatment | Copy ADSL.TRT01A. |
| TRT01AN | N | Actual treatment code | Integer conversion of ADSL.TRT01AN. |
| TRTSDT | Date | Treatment start date | Convert ADSL.TRTSDT to Date. |
| TRTEDT | Date | Treatment end date | Convert ADSL.TRTEDT to Date. |
| ITTFL | C | Intent-to-treat flag | Copy ADSL.ITTFL. |
| SAFFL | C | Safety flag | Copy ADSL.SAFFL. |
| ASEQ | N | Analysis sequence | Integer conversion of VS.VSSEQ. |
| VSSEQ | N | Source VS sequence | Integer conversion of VS.VSSEQ. |
| PARAMCD | C | Analysis parameter code | Copy VS.VSTESTCD. |
| PARAM | C | Analysis parameter | Copy VS.VSTEST. |
| PARCAT1 | C | Parameter category | Copy VS.VSCAT. |
| AVAL | N | Analysis value | Numeric conversion of VS.VSSTRESN. |
| AVALU | C | Analysis value unit | Copy VS.VSSTRESU. |
| ADT | Date | Analysis date | Convert VS.VSDTC to Date. |
| ADY | N | Analysis day | Integer conversion of VS.VSDY; this is already relative to DM.RFSTDTC. |
| AVISIT | C | Analysis visit | Copy VS.VISIT. |
| AVISITN | N | Analysis visit number | Numeric conversion of VS.VISITNUM. |
| ABLFL | C | Baseline flag | Copy VS.VSBLFL (`Y` or empty). |
| BLSEQ | N | Baseline source sequence | VSSEQ of the baseline row for subject + VSTESTCD. |
| BASE | N | Baseline value | VSSTRESN of that baseline row. |
| CHG | N | Change from baseline | Empty at baseline; postbaseline `round(AVAL - BASE, 4)`. |
| PCHG | N | Percent change from baseline | Empty at baseline or if BASE=0; otherwise `round(100 × CHG / BASE, 4)`, using the already rounded CHG. |
| SRCDOM | C | Source domain | Constant `VS`. |
| SRCVAR | C | Source sequence variable | Constant `VSSEQ`. |
| SRCSEQ | N | Source sequence value | Integer conversion of VS.VSSEQ. |

## Scope notes

`ADY` in ADLB/ADVS is copied from the SDTM study day based on DM.RFSTDTC, while ADAE `ASTDY`/`AENDY` are recalculated relative to ADSL.TRTSDT. Those references happen to agree in this generated data but are different rules. Baseline is selected by visit flag on the first treatment date; predose timing cannot be shown. Current ADaM flags, terminology, dataset structures, and physical attributes have not been tested for full CDISC conformance. The companion [validation and gaps report](ABC101_dataset_spec_validation_v1.0.md) records the checks and known gaps. No dataset is changed by these specifications.
