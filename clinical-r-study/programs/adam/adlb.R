# ============================================================
# Program: adlb.R
#
# Purpose:
#   Create the ADaM Laboratory Analysis Dataset (ADLB)
#   for ABC101. Run from the project root.
#
# Inputs:
#   data/sdtm/lb.csv
#   data/adam/adsl.csv
#
# Output:
#   data/adam/adlb.csv
#
# Baseline: the one LB record flagged LBBLFL = Y for each
# subject and lab test. CHG and PCHG apply only after baseline.
# CHG and PCHG are rounded to four decimal places. PCHG is
# missing when BASE is zero.
# ============================================================

lb <- read.csv(
  "data/sdtm/lb.csv",
  stringsAsFactors = FALSE,
  colClasses = "character"
)

adsl <- read.csv(
  "data/adam/adsl.csv",
  stringsAsFactors = FALSE,
  colClasses = "character"
)

lb_required <- c(
  "STUDYID", "DOMAIN", "USUBJID", "LBSEQ", "LBTESTCD",
  "LBTEST", "LBCAT", "LBSTRESC", "LBSTRESN", "LBSTRESU",
  "LBBLFL", "VISITNUM", "VISIT", "LBDTC", "LBDY"
)
adsl_required <- c(
  "STUDYID", "USUBJID", "SUBJID", "TRT01P", "TRT01PN",
  "TRT01A", "TRT01AN", "TRTSDT", "TRTEDT", "ITTFL", "SAFFL"
)

stopifnot(all(lb_required %in% names(lb)))
stopifnot(all(adsl_required %in% names(adsl)))
stopifnot(!anyNA(lb[lb_required]))
stopifnot(!anyNA(adsl[adsl_required]))
stopifnot(!any(lb[setdiff(lb_required, "LBBLFL")] == ""))
stopifnot(!any(adsl[adsl_required] == ""))
stopifnot(!anyDuplicated(lb[c("USUBJID", "LBSEQ")]))
stopifnot(!anyDuplicated(adsl$USUBJID))
stopifnot(all(lb$STUDYID == "ABC101"), all(lb$DOMAIN == "LB"))
stopifnot(all(lb$USUBJID %in% adsl$USUBJID))
stopifnot(all(lb$LBBLFL %in% c("Y", "")))

adsl_index <- match(lb$USUBJID, adsl$USUBJID)
lb_sequence <- as.integer(lb$LBSEQ)
analysis_value <- as.numeric(lb$LBSTRESN)
analysis_date <- as.Date(lb$LBDTC)
treatment_start <- as.Date(adsl$TRTSDT[adsl_index])
analysis_day <- as.integer(lb$LBDY)
visit_number <- as.numeric(lb$VISITNUM)

stopifnot(!anyNA(lb_sequence), all(lb_sequence > 0L))
stopifnot(!anyNA(analysis_value), all(is.finite(analysis_value)))
stopifnot(!anyNA(analysis_date), !anyNA(treatment_start))
stopifnot(!anyNA(analysis_day), !anyNA(visit_number))
stopifnot(all(analysis_value == as.numeric(lb$LBSTRESC)))

# Match each record to its single baseline by subject and test --------------

parameter_key <- paste(lb$USUBJID, lb$LBTESTCD, sep = "|")
baseline_rows <- which(lb$LBBLFL == "Y")
baseline_key <- parameter_key[baseline_rows]

stopifnot(!anyDuplicated(baseline_key))
stopifnot(setequal(unique(parameter_key), baseline_key))

baseline_index <- baseline_rows[match(parameter_key, baseline_key)]
baseline_date <- analysis_date[baseline_index]
baseline_value <- analysis_value[baseline_index]
baseline_unit <- lb$LBSTRESU[baseline_index]
baseline_sequence <- lb_sequence[baseline_index]

stopifnot(!anyNA(baseline_index))
stopifnot(all(lb$LBSTRESU == baseline_unit))
stopifnot(all(baseline_date <= treatment_start))
stopifnot(all(analysis_date[lb$LBBLFL != "Y"] >
              baseline_date[lb$LBBLFL != "Y"]))
stopifnot(all(analysis_date[lb$LBBLFL != "Y"] >
              treatment_start[lb$LBBLFL != "Y"]))

postbaseline <- lb$LBBLFL != "Y"
change <- rep(NA_real_, nrow(lb))
percent_change <- rep(NA_real_, nrow(lb))

change[postbaseline] <- round(
  analysis_value[postbaseline] - baseline_value[postbaseline],
  4
)

percent_eligible <- postbaseline & baseline_value != 0
percent_change[percent_eligible] <- round(
  100 * change[percent_eligible] / baseline_value[percent_eligible],
  4
)

# Create ADLB ---------------------------------------------------------------

adlb <- data.frame(
  STUDYID = lb$STUDYID,
  USUBJID = lb$USUBJID,
  SUBJID = adsl$SUBJID[adsl_index],
  TRT01P = adsl$TRT01P[adsl_index],
  TRT01PN = as.integer(adsl$TRT01PN[adsl_index]),
  TRT01A = adsl$TRT01A[adsl_index],
  TRT01AN = as.integer(adsl$TRT01AN[adsl_index]),
  TRTSDT = treatment_start,
  TRTEDT = as.Date(adsl$TRTEDT[adsl_index]),
  ITTFL = adsl$ITTFL[adsl_index],
  SAFFL = adsl$SAFFL[adsl_index],
  ASEQ = lb_sequence,
  LBSEQ = lb_sequence,
  PARAMCD = lb$LBTESTCD,
  PARAM = lb$LBTEST,
  PARCAT1 = lb$LBCAT,
  AVAL = analysis_value,
  AVALU = lb$LBSTRESU,
  ADT = analysis_date,
  ADY = analysis_day,
  AVISIT = lb$VISIT,
  AVISITN = visit_number,
  ABLFL = lb$LBBLFL,
  BLSEQ = baseline_sequence,
  BASE = baseline_value,
  CHG = change,
  PCHG = percent_change,
  SRCDOM = "LB",
  SRCVAR = "LBSEQ",
  SRCSEQ = lb_sequence,
  stringsAsFactors = FALSE
)

adlb <- adlb[order(adlb$USUBJID, adlb$ASEQ), ]
row.names(adlb) <- NULL

stopifnot(nrow(adlb) == nrow(lb))
stopifnot(!anyDuplicated(adlb[c("USUBJID", "ASEQ")]))
stopifnot(all(adlb$ASEQ == adlb$LBSEQ))
stopifnot(all(adlb$SRCSEQ == adlb$LBSEQ))
stopifnot(all(adlb$BLSEQ > 0L))
stopifnot(all(adlb$ABLFL == "Y" | !is.na(adlb$CHG)))
stopifnot(all(is.na(adlb$CHG[adlb$ABLFL == "Y"])))
stopifnot(all(is.na(adlb$PCHG[adlb$ABLFL == "Y"])))

write.csv(
  adlb,
  "data/adam/adlb.csv",
  row.names = FALSE,
  na = ""
)
