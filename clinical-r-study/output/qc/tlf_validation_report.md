# ABC101 Independent Final TLF QC Report

**Overall status:** PASS

**Checks:** 36 total; 36 passed; 0 failed

Recalculations use ADaM sources and Python's standard library. Production R summaries are not sourced or executed.

| Check | Description | Status | Evidence |
|---|---|---:|---|
| ADSL | ADSL input and keys | PASS | 500 records; unique keys |
| ADAE | ADAE input and keys | PASS | 750 records; unique keys |
| ADLB | ADLB input and keys | PASS | 10000 records; unique keys |
| ADVS | ADVS input and keys | PASS | 12500 records; unique keys |
| MAP-ADAE | ADaM treatment/population agreement | PASS | Actual treatment and safety flags agree with ADSL |
| MAP-ADLB | ADaM treatment/population agreement | PASS | Actual treatment and safety flags agree with ADSL |
| MAP-ADVS | ADaM treatment/population agreement | PASS | Actual treatment and safety flags agree with ADSL |
| TEAE-RULE | ADAE treatment-emergent window | PASS | Inclusive onset window recalculated using ADSL treatment dates |
| T1-Placebo | Table 1 counts, age, sex, denominator and rounding | PASS | 8 cells; planned treatment; ITT denominator 157; 1-decimal display |
| T1-Drug 10 mg | Table 1 counts, age, sex, denominator and rounding | PASS | 8 cells; planned treatment; ITT denominator 166; 1-decimal display |
| T1-Drug 20 mg | Table 1 counts, age, sex, denominator and rounding | PASS | 8 cells; planned treatment; ITT denominator 177; 1-decimal display |
| T1-Overall | Table 1 counts, age, sex, denominator and rounding | PASS | 8 cells; planned treatment; ITT denominator 500; 1-decimal display |
| T2-Placebo | Table 2 counts, denominators, percentages and rounding | PASS | 14 cells; actual treatment; safety denominator 157; distinct-subject incidence |
| T2-Drug 10 mg | Table 2 counts, denominators, percentages and rounding | PASS | 14 cells; actual treatment; safety denominator 166; distinct-subject incidence |
| T2-Drug 20 mg | Table 2 counts, denominators, percentages and rounding | PASS | 14 cells; actual treatment; safety denominator 177; distinct-subject incidence |
| T2-Overall | Table 2 counts, denominators, percentages and rounding | PASS | 14 cells; actual treatment; safety denominator 500; distinct-subject incidence |
| T3-STRUCT | T3 rows and source contribution uniqueness | PASS | 36 complete rows; one source result per subject/test/visit |
| T3-Placebo | T3 contributing n, means and sample SD | PASS | 36 n/mean/SD results; units, visits, safety population and rounding checked |
| T3-Drug 10 mg | T3 contributing n, means and sample SD | PASS | 36 n/mean/SD results; units, visits, safety population and rounding checked |
| T3-Drug 20 mg | T3 contributing n, means and sample SD | PASS | 36 n/mean/SD results; units, visits, safety population and rounding checked |
| T3-Overall | T3 contributing n, means and sample SD | PASS | 36 n/mean/SD results; units, visits, safety population and rounding checked |
| T4-STRUCT | T4 rows and source contribution uniqueness | PASS | 45 complete rows; one source result per subject/test/visit |
| T4-Placebo | T4 contributing n, means and sample SD | PASS | 45 n/mean/SD results; units, visits, safety population and rounding checked |
| T4-Drug 10 mg | T4 contributing n, means and sample SD | PASS | 45 n/mean/SD results; units, visits, safety population and rounding checked |
| T4-Drug 20 mg | T4 contributing n, means and sample SD | PASS | 45 n/mean/SD results; units, visits, safety population and rounding checked |
| T4-Overall | T4 contributing n, means and sample SD | PASS | 45 n/mean/SD results; units, visits, safety population and rounding checked |
| L1 | Listing 1 completeness, values and sort order | PASS | All 750 ADAE records exactly once, all fields and sort keys checked |
| F1-DATA | Figure 1 plotted summaries | PASS | 12 means/counts independently calculated; chronological visits, actual treatment and kg checked |
| F1-PNG | Figure 1 PNG structure and visible content | PASS | 1800 x 1100 PNG; treatment pixels {'Placebo': 4734, 'Drug 10 mg': 5124, 'Drug 20 mg': 5525}; count-grid pixels 6320; SHA256 26330733cab7c043748994ff2141bff557a9a5932dfc98e1147e95d8e0b158d3 |
| F1-LABELS | Figure numbering, population and units | PASS | Figure generator labels checked; PNG still requires visual inspection |
| F1-VISUAL | Recorded independent visual inspection | PASS | Operator recorded visual review of this exact PNG: title, line/legend treatment mapping, visits, kg axis and count grid |
| T1-DISPLAY | Displayed rows, labels and definitions | PASS | All 8 displayed rows match CSV; title and definitions checked |
| T2-DISPLAY | Displayed rows, labels and definitions | PASS | All 14 displayed rows match CSV; title and definitions checked |
| T3-DISPLAY | Displayed rows, labels and definitions | PASS | All 36 displayed rows match CSV; title and definitions checked |
| T4-DISPLAY | Displayed rows, labels and definitions | PASS | All 45 displayed rows match CSV; title and definitions checked |
| L1-DISPLAY | Displayed rows, labels and definitions | PASS | All 750 displayed rows match CSV; title and definitions checked |

## Display review scope

Markdown rows, titles, units, population notes and numeric precision are checked above.
The PNG dimensions, treatment-color pixels, count-grid ink and generator labels are checked automatically. Visual inspection is recorded only when --reviewed-figure-sha256 is supplied after viewing the exact image; this is an operator attestation, not an automated interpretation of every plotted value.

## Input fingerprints

- ADSL: `73779bec4e718b1970bce9c16c6a82c174fa2892e3f6945f905b9719d4cf203f`
- ADAE: `101557c62c6402d851a07e007625788c0dbd26c02fe9b94633241f822d1288be`
- ADLB: `23555be9bed84f6744ae49315e282e293be7d01969c2c4fd1e9d50fdeb31dcb3`
- ADVS: `563a170d710a99aa7711b1201a405308149887a41a7b41d87775f9772663cdd7`
