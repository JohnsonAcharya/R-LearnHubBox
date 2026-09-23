# ============================================================
# Program: advs.R
#
# Purpose:
#   Create the ADaM Vital Signs Analysis Dataset (ADVS)
#   for ABC101. Run from the project root.
#
# Inputs:
#   data/sdtm/vs.csv
#   data/adam/adsl.csv
#
# Output:
#   data/adam/advs.csv
#
# Baseline: the one VS record flagged VSBLFL = Y for each
# subject and vital-sign test. CHG and PCHG apply only after
# baseline. Both are rounded to four decimal places. PCHG is
# missing when BASE is zero.
# ============================================================

vs <- read.csv(
  "data/sdtm/vs.csv",
  stringsAsFactors = FALSE,
  colClasses = "character"
)

adsl <- read.csv(
  "data/adam/adsl.csv",
  stringsAsFactors = FALSE,
  colClasses = "character"
)

vs_required <- c(
  "STUDYID", "DOMAIN", "USUBJID", "VSSEQ", "VSTESTCD",
  "VSTEST", "VSCAT", "VSSTRESC", "VSSTRESN", "VSSTRESU",
  "VSBLFL", "VISITNUM", "VISIT", "VSDTC", "VSDY"
)
adsl_required <- c(
  "STUDYID", "USUBJID", "SUBJID", "TRT01P", "TRT01PN",
  "TRT01A", "TRT01AN", "TRTSDT", "TRTEDT", "ITTFL", "SAFFL"
)

stopifnot(all(vs_required %in% names(vs)))
stopifnot(all(adsl_required %in% names(adsl)))
stopifnot(!anyNA(vs[vs_required]))
stopifnot(!anyNA(adsl[adsl_required]))
stopifnot(!any(vs[setdiff(vs_required, "VSBLFL")] == ""))
stopifnot(!any(adsl[adsl_required] == ""))
stopifnot(!anyDuplicated(vs[c("USUBJID", "VSSEQ")]))
stopifnot(!anyDuplicated(adsl$USUBJID))
stopifnot(all(vs$STUDYID == "ABC101"), all(vs$DOMAIN == "VS"))
stopifnot(all(vs$USUBJID %in% adsl$USUBJID))
stopifnot(all(vs$VSBLFL %in% c("Y", "")))

adsl_index <- match(vs$USUBJID, adsl$USUBJID)
vs_sequence <- as.integer(vs$VSSEQ)
analysis_value <- as.numeric(vs$VSSTRESN)
analysis_date <- as.Date(vs$VSDTC)
treatment_start <- as.Date(adsl$TRTSDT[adsl_index])
treatment_end <- as.Date(adsl$TRTEDT[adsl_index])
analysis_day <- as.integer(vs$VSDY)
visit_number <- as.numeric(vs$VISITNUM)

stopifnot(!anyNA(vs_sequence), all(vs_sequence > 0L))
stopifnot(!anyNA(analysis_value), all(is.finite(analysis_value)))
stopifnot(!anyNA(analysis_date), !anyNA(treatment_start))
stopifnot(!anyNA(treatment_end), all(treatment_end >= treatment_start))
stopifnot(!anyNA(analysis_day), !anyNA(visit_number))
stopifnot(all(analysis_value == as.numeric(vs$VSSTRESC)))
stopifnot(all(vs$STUDYID == adsl$STUDYID[adsl_index]))

# Match each record to its single baseline by subject and test --------------

parameter_key <- paste(vs$USUBJID, vs$VSTESTCD, sep = "|")
baseline_rows <- which(vs$VSBLFL == "Y")
baseline_key <- parameter_key[baseline_rows]

stopifnot(!anyDuplicated(baseline_key))
stopifnot(setequal(unique(parameter_key), baseline_key))

baseline_index <- baseline_rows[match(parameter_key, baseline_key)]
baseline_date <- analysis_date[baseline_index]
baseline_value <- analysis_value[baseline_index]
baseline_unit <- vs$VSSTRESU[baseline_index]
baseline_sequence <- vs_sequence[baseline_index]

stopifnot(!anyNA(baseline_index))
stopifnot(all(vs$VSSTRESU == baseline_unit))
stopifnot(all(baseline_date <= treatment_start))

postbaseline <- vs$VSBLFL != "Y"
stopifnot(all(analysis_date[postbaseline] > baseline_date[postbaseline]))
stopifnot(all(analysis_date[postbaseline] > treatment_start[postbaseline]))

change <- rep(NA_real_, nrow(vs))
percent_change <- rep(NA_real_, nrow(vs))

change[postbaseline] <- round(
  analysis_value[postbaseline] - baseline_value[postbaseline],
  4
)

percent_eligible <- postbaseline & baseline_value != 0
percent_change[percent_eligible] <- round(
  100 * change[percent_eligible] / baseline_value[percent_eligible],
  4
)

# Create ADVS ---------------------------------------------------------------

advs <- data.frame(
  STUDYID = vs$STUDYID,
  USUBJID = vs$USUBJID,
  SUBJID = adsl$SUBJID[adsl_index],
  TRT01P = adsl$TRT01P[adsl_index],
  TRT01PN = as.integer(adsl$TRT01PN[adsl_index]),
  TRT01A = adsl$TRT01A[adsl_index],
  TRT01AN = as.integer(adsl$TRT01AN[adsl_index]),
  TRTSDT = treatment_start,
  TRTEDT = treatment_end,
  ITTFL = adsl$ITTFL[adsl_index],
  SAFFL = adsl$SAFFL[adsl_index],
  ASEQ = vs_sequence,
  VSSEQ = vs_sequence,
  PARAMCD = vs$VSTESTCD,
  PARAM = vs$VSTEST,
  PARCAT1 = vs$VSCAT,
  AVAL = analysis_value,
  AVALU = vs$VSSTRESU,
  ADT = analysis_date,
  ADY = analysis_day,
  AVISIT = vs$VISIT,
  AVISITN = visit_number,
  ABLFL = vs$VSBLFL,
  BLSEQ = baseline_sequence,
  BASE = baseline_value,
  CHG = change,
  PCHG = percent_change,
  SRCDOM = "VS",
  SRCVAR = "VSSEQ",
  SRCSEQ = vs_sequence,
  stringsAsFactors = FALSE
)

advs <- advs[order(advs$USUBJID, advs$ASEQ), ]
row.names(advs) <- NULL

stopifnot(nrow(advs) == nrow(vs))
stopifnot(!anyDuplicated(advs[c("USUBJID", "ASEQ")]))
stopifnot(all(advs$ASEQ == advs$VSSEQ))
stopifnot(all(advs$SRCSEQ == advs$VSSEQ))
stopifnot(all(advs$BLSEQ > 0L))
stopifnot(all(advs$ABLFL == "Y" | !is.na(advs$CHG)))
stopifnot(all(is.na(advs$CHG[advs$ABLFL == "Y"])))
stopifnot(all(is.na(advs$PCHG[advs$ABLFL == "Y"])))

write.csv(
  advs,
  "data/adam/advs.csv",
  row.names = FALSE,
  na = ""
)
