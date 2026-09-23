# ============================================================
# Program: adae.R
#
# Purpose:
#   Create the ADaM Adverse Events Analysis Dataset (ADAE)
#   for ABC101.
#
# Inputs:
#   data/sdtm/ae.csv
#   data/adam/adsl.csv
#
# Output:
#   data/adam/adae.csv
#
# Treatment-emergent definition:
#   AE start date is on or after TRTSDT and no later than
#   30 days after TRTEDT.
# ============================================================

ae <- read.csv(
  "data/sdtm/ae.csv",
  stringsAsFactors = FALSE,
  colClasses = "character"
)

adsl <- read.csv(
  "data/adam/adsl.csv",
  stringsAsFactors = FALSE,
  colClasses = "character"
)

# Validate source structure -------------------------------------------------

ae_required <- c(
  "STUDYID", "DOMAIN", "USUBJID", "AESEQ", "AETERM", "AEDECOD",
  "AESEV", "AESER", "AEOUT", "AESTDTC", "AEENDTC", "AESTDY",
  "AEENDY", "EPOCH"
)

adsl_required <- c(
  "STUDYID", "USUBJID", "SUBJID", "TRT01P", "TRT01PN",
  "TRT01A", "TRT01AN", "TRTSDT", "TRTEDT", "SAFFL"
)

stopifnot(all(ae_required %in% names(ae)))
stopifnot(all(adsl_required %in% names(adsl)))
stopifnot(!anyNA(ae[ae_required]))
stopifnot(!anyNA(adsl[adsl_required]))
stopifnot(!any(ae[ae_required] == ""))
stopifnot(!any(adsl[adsl_required] == ""))
stopifnot(!anyDuplicated(ae[c("USUBJID", "AESEQ")]))
stopifnot(!anyDuplicated(adsl$USUBJID))
stopifnot(all(ae$STUDYID == "ABC101"), all(ae$DOMAIN == "AE"))
stopifnot(all(ae$USUBJID %in% adsl$USUBJID))

# Add subject-level analysis information -----------------------------------

adsl_index <- match(ae$USUBJID, adsl$USUBJID)
ae_sequence <- as.integer(ae$AESEQ)
analysis_start_date <- as.Date(ae$AESTDTC)
analysis_end_date <- as.Date(ae$AEENDTC)
treatment_start_date <- as.Date(adsl$TRTSDT[adsl_index])
treatment_end_date <- as.Date(adsl$TRTEDT[adsl_index])
treatment_emergent_end_date <- treatment_end_date + 30L

stopifnot(!anyNA(ae_sequence), all(ae_sequence >= 1L))
stopifnot(!anyNA(analysis_start_date), !anyNA(analysis_end_date))
stopifnot(!anyNA(treatment_start_date), !anyNA(treatment_end_date))
stopifnot(all(analysis_end_date >= analysis_start_date))
stopifnot(all(treatment_end_date >= treatment_start_date))

derive_study_day <- function(event_date, reference_date) {
  difference <- as.integer(event_date - reference_date)
  ifelse(event_date >= reference_date, difference + 1L, difference)
}

analysis_start_day <- derive_study_day(
  analysis_start_date,
  treatment_start_date
)
analysis_end_day <- derive_study_day(
  analysis_end_date,
  treatment_start_date
)

treatment_emergent <-
  analysis_start_date >= treatment_start_date &
  analysis_start_date <= treatment_emergent_end_date

# Create ADAE ---------------------------------------------------------------

adae <- data.frame(
  STUDYID = ae$STUDYID,
  USUBJID = ae$USUBJID,
  SUBJID = adsl$SUBJID[adsl_index],
  TRT01P = adsl$TRT01P[adsl_index],
  TRT01PN = as.integer(adsl$TRT01PN[adsl_index]),
  TRT01A = adsl$TRT01A[adsl_index],
  TRT01AN = as.integer(adsl$TRT01AN[adsl_index]),
  TRTSDT = treatment_start_date,
  TRTEDT = treatment_end_date,
  SAFFL = adsl$SAFFL[adsl_index],
  ASEQ = ae_sequence,
  AESEQ = ae_sequence,
  AETERM = ae$AETERM,
  AEDECOD = ae$AEDECOD,
  AESEV = ae$AESEV,
  AESER = ae$AESER,
  AEOUT = ae$AEOUT,
  AESTDTC = ae$AESTDTC,
  AEENDTC = ae$AEENDTC,
  ASTDT = analysis_start_date,
  AENDT = analysis_end_date,
  ASTDY = analysis_start_day,
  AENDY = analysis_end_day,
  EPOCH = ae$EPOCH,
  TRTEMFL = ifelse(treatment_emergent, "Y", ""),
  SRCDOM = "AE",
  SRCVAR = "AESEQ",
  SRCSEQ = ae_sequence,
  stringsAsFactors = FALSE
)

adae <- adae[order(adae$USUBJID, adae$ASEQ), ]
row.names(adae) <- NULL

# Validate the completed dataset -------------------------------------------

stopifnot(nrow(adae) == nrow(ae))
stopifnot(!anyDuplicated(adae[c("USUBJID", "ASEQ")]))
stopifnot(!anyNA(adae))
stopifnot(all(adae$ASEQ == adae$AESEQ))
stopifnot(all(adae$ASTDY == as.integer(ae$AESTDY[match(
  paste(adae$USUBJID, adae$AESEQ),
  paste(ae$USUBJID, ae$AESEQ)
)])))
stopifnot(all(adae$AENDY == as.integer(ae$AEENDY[match(
  paste(adae$USUBJID, adae$AESEQ),
  paste(ae$USUBJID, ae$AESEQ)
)])))
stopifnot(all(adae$TRTEMFL %in% c("Y", "")))
stopifnot(all(adae$SRCDOM == "AE"))
stopifnot(all(adae$SRCVAR == "AESEQ"))
stopifnot(all(adae$SRCSEQ == adae$AESEQ))

dir.create("data/adam", recursive = TRUE, showWarnings = FALSE)

write.csv(
  adae,
  "data/adam/adae.csv",
  row.names = FALSE,
  na = ""
)
