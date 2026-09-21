# ============================================================
# Program: ex.R
#
# Purpose:
#   Create the SDTM Exposure (EX) domain for ABC101.
#
# Inputs:
#   data/raw/exposure.csv
#   data/sdtm/dm.csv
#
# Output:
#   data/sdtm/ex.csv
# ============================================================

raw_ex <- read.csv(
  "data/raw/exposure.csv",
  stringsAsFactors = FALSE,
  colClasses = "character"
)

dm <- read.csv(
  "data/sdtm/dm.csv",
  stringsAsFactors = FALSE
)

# Validate source structure -------------------------------------------------

ex_required <- c(
  "USUBJID", "EXSEQ", "EXTRT", "EXDOSE", "EXDOSU",
  "EXDOSFRQ", "EXSTDTC", "EXENDTC"
)
dm_required <- c(
  "USUBJID", "RFSTDTC", "RFXSTDTC", "RFXENDTC", "ACTARM"
)

stopifnot(all(ex_required %in% names(raw_ex)))
stopifnot(all(dm_required %in% names(dm)))
stopifnot(!anyNA(raw_ex[ex_required]))
stopifnot(!anyNA(dm[dm_required]))
stopifnot(!any(raw_ex[ex_required] == ""))
stopifnot(!anyDuplicated(dm$USUBJID))
stopifnot(!anyDuplicated(raw_ex[c("USUBJID", "EXSEQ")]))
stopifnot(all(raw_ex$USUBJID %in% dm$USUBJID))

ex_sequence <- as.integer(raw_ex$EXSEQ)
ex_dose <- as.numeric(raw_ex$EXDOSE)
ex_start_date <- as.Date(raw_ex$EXSTDTC)
ex_end_date <- as.Date(raw_ex$EXENDTC)

stopifnot(!anyNA(ex_sequence), all(ex_sequence >= 1L))
stopifnot(!anyNA(ex_dose), all(ex_dose > 0))
stopifnot(!anyNA(ex_start_date), !anyNA(ex_end_date))
stopifnot(all(ex_end_date >= ex_start_date))
stopifnot(all(raw_ex$EXTRT %in% c("Placebo", "Drug 10 mg", "Drug 20 mg")))
stopifnot(all(raw_ex$EXDOSU == "tablet"))
stopifnot(all(raw_ex$EXDOSFRQ == "QD"))

# Add subject-level reference data ------------------------------------------

dm_index <- match(raw_ex$USUBJID, dm$USUBJID)
reference_start_date <- as.Date(dm$RFSTDTC[dm_index])
dm_exposure_start_date <- as.Date(dm$RFXSTDTC[dm_index])
dm_exposure_end_date <- as.Date(dm$RFXENDTC[dm_index])

stopifnot(all(raw_ex$EXTRT == dm$ACTARM[dm_index]))
stopifnot(all(ex_start_date == dm_exposure_start_date))
stopifnot(all(ex_end_date == dm_exposure_end_date))

derive_study_day <- function(event_date, reference_date) {
  difference <- as.integer(event_date - reference_date)
  ifelse(event_date >= reference_date, difference + 1L, difference)
}

# Create EX -----------------------------------------------------------------

ex <- data.frame(
  STUDYID = "ABC101",
  DOMAIN = "EX",
  USUBJID = raw_ex$USUBJID,
  EXSEQ = ex_sequence,
  EXTRT = raw_ex$EXTRT,
  EXCAT = "STUDY TREATMENT",
  EXDOSE = ex_dose,
  EXDOSU = toupper(raw_ex$EXDOSU),
  EXDOSFRQ = raw_ex$EXDOSFRQ,
  EXSTDTC = raw_ex$EXSTDTC,
  EXENDTC = raw_ex$EXENDTC,
  EXSTDY = derive_study_day(ex_start_date, reference_start_date),
  EXENDY = derive_study_day(ex_end_date, reference_start_date),
  EPOCH = "TREATMENT",
  stringsAsFactors = FALSE
)

ex <- ex[order(ex$USUBJID, ex$EXSEQ), ]
row.names(ex) <- NULL

# Validate the completed domain --------------------------------------------

stopifnot(nrow(ex) == nrow(raw_ex))
stopifnot(!anyDuplicated(ex[c("USUBJID", "EXSEQ")]))
stopifnot(!anyNA(ex))
stopifnot(all(ex$EXENDY >= ex$EXSTDY))
stopifnot(all(ex$EPOCH == "TREATMENT"))

write.csv(
  ex,
  "data/sdtm/ex.csv",
  row.names = FALSE,
  na = ""
)
