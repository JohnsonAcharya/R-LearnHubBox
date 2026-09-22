# ABC101 SDTM Cross-Domain Validation Report

**Overall status:** PASS

**Domains:** DM, AE, LB, VS, EX  
**Checks:** 45 total; 45 passed; 0 failed

| Check | Description | Status | Details |
|---|---|---:|---|
| DM-01 | DM contains all expected variables | PASS | 15 of 15 expected variables present |
| DM-02 | DM has the expected record count | PASS | 500 records; expected 500 |
| DM-03 | DM key is unique | PASS | Key: USUBJID |
| DM-04 | DM has valid study and domain identifiers | PASS | Expected STUDYID=ABC101 and matching DOMAIN |
| AE-01 | AE contains all expected variables | PASS | 14 of 14 expected variables present |
| AE-02 | AE has the expected record count | PASS | 750 records; expected 750 |
| AE-03 | AE key is unique | PASS | Key: USUBJID + AESEQ |
| AE-04 | AE has valid study and domain identifiers | PASS | Expected STUDYID=ABC101 and matching DOMAIN |
| LB-01 | LB contains all expected variables | PASS | 18 of 18 expected variables present |
| LB-02 | LB has the expected record count | PASS | 10000 records; expected 10000 |
| LB-03 | LB key is unique | PASS | Key: USUBJID + LBSEQ |
| LB-04 | LB has valid study and domain identifiers | PASS | Expected STUDYID=ABC101 and matching DOMAIN |
| VS-01 | VS contains all expected variables | PASS | 18 of 18 expected variables present |
| VS-02 | VS has the expected record count | PASS | 12500 records; expected 12500 |
| VS-03 | VS key is unique | PASS | Key: USUBJID + VSSEQ |
| VS-04 | VS has valid study and domain identifiers | PASS | Expected STUDYID=ABC101 and matching DOMAIN |
| EX-01 | EX contains all expected variables | PASS | 14 of 14 expected variables present |
| EX-02 | EX has the expected record count | PASS | 500 records; expected 500 |
| EX-03 | EX key is unique | PASS | Key: USUBJID + EXSEQ |
| EX-04 | EX has valid study and domain identifiers | PASS | Expected STUDYID=ABC101 and matching DOMAIN |
| REF-AE | AE subjects exist in DM | PASS | 0 orphan subjects |
| REF-LB | LB subjects exist in DM | PASS | 0 orphan subjects |
| REF-VS | VS subjects exist in DM | PASS | 0 orphan subjects |
| REF-EX | EX subjects exist in DM | PASS | 0 orphan subjects |
| DM-X01 | DM reference start equals first known study activity | PASS | 0 subjects differ |
| DM-X02 | DM reference end equals last known study activity | PASS | 0 subjects differ |
| DM-X03 | DM exposure dates agree with EX | PASS | Compared RFXSTDTC/RFXENDTC with EX interval |
| DM-X04 | DM actual arm agrees with EX treatment | PASS | 0 records differ |
| AE-X01 | AE dates are valid and ordered | PASS | 0 end dates precede start dates |
| AE-X02 | AE study days agree with DM RFSTDTC | PASS | Validated AESTDY and AEENDY |
| AE-X03 | AE epochs agree with DM exposure dates | PASS | 0 records differ |
| AE-X04 | AE dates fall within each subject reference period | PASS | 0 AE end dates exceed RFENDTC |
| LB-X01 | LB dates and study days are valid | PASS | Validated LBDTC and LBDY |
| LB-X02 | LB epochs agree with DM exposure dates | PASS | 0 records differ |
| LB-X03 | LB standard character and numeric results agree | PASS | Compared LBSTRESC with LBSTRESN |
| LB-X04 | LB baseline flag agrees with Baseline visit | PASS | 0 records differ |
| VS-X01 | VS dates and study days are valid | PASS | Validated VSDTC and VSDY |
| VS-X02 | VS epochs agree with DM exposure dates | PASS | 0 records differ |
| VS-X03 | VS standard character and numeric results agree | PASS | Compared VSSTRESC with VSSTRESN |
| VS-X04 | VS baseline flag agrees with Baseline visit | PASS | 0 records differ |
| LB-X05 | LB has 20 scheduled records per DM subject | PASS | 20 to 20 |
| VS-X05 | VS has 25 scheduled records per DM subject | PASS | 25 to 25 |
| EX-X01 | EX dates are valid and ordered | PASS | 0 end dates precede start dates |
| EX-X02 | EX study days agree with DM RFSTDTC | PASS | Validated EXSTDY and EXENDY |
| EX-X03 | EX contains one treatment interval per DM subject | PASS | 500 EX records for 500 DM subjects |

## Interpretation

All implemented structural and cross-domain checks passed.
