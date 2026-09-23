# ABC101 ADaM Validation Report

**Overall status:** PASS

**Datasets:** ADSL, ADAE, ADLB, ADVS  
**Checks:** 52 total; 52 passed; 0 failed

| Check | Description | Status | Detail |
|---|---|---:|---|
| ADSL-01 | ADSL expected columns | PASS | 15 of 15 present |
| ADSL-02 | ADSL row count matches source | PASS | 500 analysis rows; 500 source rows |
| ADSL-03 | ADSL analysis key is unique | PASS | USUBJID |
| ADSL-04 | ADSL has valid STUDYID | PASS | Expected ABC101 |
| ADAE-01 | ADAE expected columns | PASS | 28 of 28 present |
| ADAE-02 | ADAE row count matches source | PASS | 750 analysis rows; 750 source rows |
| ADAE-03 | ADAE analysis key is unique | PASS | USUBJID + ASEQ |
| ADAE-04 | ADAE has valid STUDYID | PASS | Expected ABC101 |
| ADLB-01 | ADLB expected columns | PASS | 30 of 30 present |
| ADLB-02 | ADLB row count matches source | PASS | 10000 analysis rows; 10000 source rows |
| ADLB-03 | ADLB analysis key is unique | PASS | USUBJID + ASEQ |
| ADLB-04 | ADLB has valid STUDYID | PASS | Expected ABC101 |
| ADVS-01 | ADVS expected columns | PASS | 30 of 30 present |
| ADVS-02 | ADVS row count matches source | PASS | 12500 analysis rows; 12500 source rows |
| ADVS-03 | ADVS analysis key is unique | PASS | USUBJID + ASEQ |
| ADVS-04 | ADVS has valid STUDYID | PASS | Expected ABC101 |
| ADSL-05 | ADSL covers every DM and EX subject | PASS | 500 ADSL subjects |
| ADAE-05 | ADAE subjects exist in ADSL | PASS | 0 orphan subjects |
| ADAE-06 | ADAE covers each SDTM source record | PASS | 750 analysis keys; 750 source keys |
| ADLB-05 | ADLB subjects exist in ADSL | PASS | 0 orphan subjects |
| ADLB-06 | ADLB covers each SDTM source record | PASS | 10000 analysis keys; 10000 source keys |
| ADVS-05 | ADVS subjects exist in ADSL | PASS | 0 orphan subjects |
| ADVS-06 | ADVS covers each SDTM source record | PASS | 12500 analysis keys; 12500 source keys |
| ADSL-X01 | Demographics match DM | PASS | SUBJID, AGE, AGEU, SEX |
| ADSL-X02 | Planned and actual treatments match DM and EX | PASS | TRT01P and TRT01A |
| ADSL-X03 | Numeric treatment codes are consistent | PASS | Placebo=0, Drug 10 mg=1, Drug 20 mg=2 |
| ADSL-X04 | Treatment dates and duration match EX | PASS | Inclusive treatment duration |
| ADSL-X05 | Population flags match this study's enrolled and exposed subjects | PASS | 500 ITT; 500 safety |
| ADAE-X01 | ADAE subject and treatment fields match ADSL | PASS | SUBJID, TRT01P, TRT01PN, TRT01A, TRT01AN, TRTSDT, TRTEDT, SAFFL |
| ADLB-X01 | ADLB subject and treatment fields match ADSL | PASS | SUBJID, TRT01P, TRT01PN, TRT01A, TRT01AN, TRTSDT, TRTEDT, SAFFL, ITTFL |
| ADVS-X01 | ADVS subject and treatment fields match ADSL | PASS | SUBJID, TRT01P, TRT01PN, TRT01A, TRT01AN, TRTSDT, TRTEDT, SAFFL, ITTFL |
| ADAE-X02 | Event fields match SDTM AE | PASS | AETERM, AEDECOD, AESEV, AESER, AEOUT, AESTDTC, AEENDTC, EPOCH |
| ADAE-X03 | Analysis and source dates and study days agree | PASS | ASTDT, AENDT, ASTDY, AENDY |
| ADAE-X04 | AE record traceability is complete | PASS | SRCDOM=AE; SRCVAR=AESEQ; SRCSEQ=AESEQ |
| ADAE-X05 | TRTEMFL uses the inclusive 30-day window | PASS | 473 treatment-emergent records |
| ADAE-X06 | TEAE flag and safety population are consistent | PASS | 473 flagged events |
| ADLB-X02 | ADLB parameter, value, visit, and date match source | PASS | PARAM, AVAL, AVALU, ADT, ADY, AVISIT |
| ADLB-X03 | ADLB record traceability matches source | PASS | SRCDOM=LB; SRCVAR=LBSEQ |
| ADLB-X04 | ADLB has one baseline per subject and test | PASS | 2000 source baseline records for 2000 subject-test groups |
| ADLB-X05 | ADLB BASE and BLSEQ trace to source baseline | PASS | BASE, BLSEQ, ABLFL |
| ADLB-X06 | ADLB baseline change fields are blank | PASS | 2000 baseline rows |
| ADLB-X07 | ADLB postbaseline CHG equals AVAL minus BASE | PASS | 8000 postbaseline rows; rounded to 4 decimals |
| ADLB-X08 | ADLB postbaseline PCHG uses BASE denominator | PASS | 8000 nonzero-baseline rows; 0 zero-baseline rows |
| ADLB-X09 | ADLB postbaseline dates follow baseline and treatment start | PASS | 8000 postbaseline dates checked |
| ADVS-X02 | ADVS parameter, value, visit, and date match source | PASS | PARAM, AVAL, AVALU, ADT, ADY, AVISIT |
| ADVS-X03 | ADVS record traceability matches source | PASS | SRCDOM=VS; SRCVAR=VSSEQ |
| ADVS-X04 | ADVS has one baseline per subject and test | PASS | 2500 source baseline records for 2500 subject-test groups |
| ADVS-X05 | ADVS BASE and BLSEQ trace to source baseline | PASS | BASE, BLSEQ, ABLFL |
| ADVS-X06 | ADVS baseline change fields are blank | PASS | 2500 baseline rows |
| ADVS-X07 | ADVS postbaseline CHG equals AVAL minus BASE | PASS | 10000 postbaseline rows; rounded to 4 decimals |
| ADVS-X08 | ADVS postbaseline PCHG uses BASE denominator | PASS | 10000 nonzero-baseline rows; 0 zero-baseline rows |
| ADVS-X09 | ADVS postbaseline dates follow baseline and treatment start | PASS | 10000 postbaseline dates checked |

## Dataset summary

| Dataset | Rows | Subjects |
|---|---:|---:|
| ADSL | 500 | 500 |
| ADAE | 750 | 390 |
| ADLB | 10000 | 500 |
| ADVS | 12500 | 500 |

Treatment-emergent AE records: 473
ADLB baseline records: 2000
ADVS baseline records: 2500

## Interpretation

All implemented ADaM and source traceability checks passed.

