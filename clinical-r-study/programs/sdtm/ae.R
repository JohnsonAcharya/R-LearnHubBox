# ============================================================
# Program: ae.R
#
# Purpose:
#   Create the SDTM Adverse Events (AE) domain for ABC101.
#
# Inputs:
#   data/raw/adverse_events.csv
#   data/sdtm/dm.csv
#
# Output:
#   data/sdtm/ae.csv
# ============================================================

raw_ae <- read.csv(
  "data/raw/adverse_events.csv",
  stringsAsFactors = FALSE
)

dm <- read.csv(
  "data/sdtm/dm.csv",
  stringsAsFactors = FALSE
)

# Validate source structure -------------------------------------------------

ae_required <- c(
  "USUBJID", "AESEQ", "AETERM", "AESTDTC",
  "AEENDTC", "AESEV", "AESER", "AEOUT"
)
dm_required <- c("USUBJID", "RFSTDTC", "RFXSTDTC", "RFXENDTC")

stopifnot(all(ae_required %in% names(raw_ae)))
stopifnot(all(dm_required %in% names(dm)))
stopifnot(!anyNA(raw_ae[ae_required]))
stopifnot(!anyNA(dm[dm_required]))
stopifnot(!anyDuplicated(dm$USUBJID))
stopifnot(!anyDuplicated(raw_ae[c("USUBJID", "AESEQ")]))
stopifnot(all(raw_ae$USUBJID %in% dm$USUBJID))
stopifnot(all(raw_ae$AESEV %in% c("Mild", "Moderate", "Severe")))
stopifnot(all(raw_ae$AESER %in% c("Y", "N")))
stopifnot(all(raw_ae$AEOUT %in% c("RECOVERED", "RECOVERING", "NOT RECOVERED")))

ae_start_date <- as.Date(raw_ae$AESTDTC)
ae_end_date <- as.Date(raw_ae$AEENDTC)

stopifnot(!anyNA(ae_start_date))
stopifnot(!anyNA(ae_end_date))
stopifnot(all(ae_end_date >= ae_start_date))

# Add subject-level reference dates -----------------------------------------

dm_index <- match(raw_ae$USUBJID, dm$USUBJID)
reference_start_date <- as.Date(dm$RFSTDTC[dm_index])
exposure_start_date <- as.Date(dm$RFXSTDTC[dm_index])
exposure_end_date <- as.Date(dm$RFXENDTC[dm_index])

# Study day follows the SDTM convention: there is no Day 0 -----------------

derive_study_day <- function(event_date, reference_date) {
  difference <- as.integer(event_date - reference_date)
  ifelse(event_date >= reference_date, difference + 1L, difference)
}

# Create AE -----------------------------------------------------------------

ae <- data.frame(
  STUDYID = "ABC101",
  DOMAIN = "AE",
  USUBJID = raw_ae$USUBJID,
  AESEQ = as.integer(raw_ae$AESEQ),
  AETERM = raw_ae$AETERM,
  AEDECOD = toupper(raw_ae$AETERM),
  AESEV = toupper(raw_ae$AESEV),
  AESER = raw_ae$AESER,
  AEOUT = raw_ae$AEOUT,
  AESTDTC = raw_ae$AESTDTC,
  AEENDTC = raw_ae$AEENDTC,
  AESTDY = derive_study_day(ae_start_date, reference_start_date),
  AEENDY = derive_study_day(ae_end_date, reference_start_date),
  EPOCH = ifelse(
    ae_start_date >= exposure_start_date & ae_start_date <= exposure_end_date,
    "TREATMENT",
    "FOLLOW-UP"
  ),
  stringsAsFactors = FALSE
)

ae <- ae[order(ae$USUBJID, ae$AESEQ), ]
row.names(ae) <- NULL

# Validate the completed domain --------------------------------------------

stopifnot(nrow(ae) == nrow(raw_ae))
stopifnot(!anyDuplicated(ae[c("USUBJID", "AESEQ")]))
stopifnot(!anyNA(ae))
stopifnot(all(ae$AESEQ >= 1L))
stopifnot(all(ae$AEENDY >= ae$AESTDY))
stopifnot(all(ae$EPOCH %in% c("TREATMENT", "FOLLOW-UP")))

write.csv(
  ae,
  "data/sdtm/ae.csv",
  row.names = FALSE,
  na = ""
)
