# ============================================================
# Program: adsl.R
#
# Purpose:
#   Create the ADaM Subject-Level Analysis Dataset (ADSL)
#   for ABC101.
#
# Inputs:
#   data/sdtm/dm.csv
#   data/sdtm/ex.csv
#
# Output:
#   data/adam/adsl.csv
# ============================================================

dm <- read.csv(
  "data/sdtm/dm.csv",
  stringsAsFactors = FALSE,
  colClasses = "character"
)

ex <- read.csv(
  "data/sdtm/ex.csv",
  stringsAsFactors = FALSE,
  colClasses = "character"
)

# Validate source structure -------------------------------------------------

dm_required <- c(
  "STUDYID", "USUBJID", "SUBJID", "AGE", "AGEU", "SEX",
  "ARM", "ACTARM", "RFXSTDTC", "RFXENDTC"
)
ex_required <- c("STUDYID", "USUBJID", "EXTRT", "EXSTDTC", "EXENDTC")

stopifnot(all(dm_required %in% names(dm)))
stopifnot(all(ex_required %in% names(ex)))
stopifnot(!anyNA(dm[dm_required]))
stopifnot(!anyNA(ex[ex_required]))
stopifnot(!any(dm[dm_required] == ""))
stopifnot(!any(ex[ex_required] == ""))
stopifnot(!anyDuplicated(dm$USUBJID))
stopifnot(!anyDuplicated(ex$USUBJID))
stopifnot(setequal(dm$USUBJID, ex$USUBJID))
stopifnot(all(dm$STUDYID == "ABC101"), all(ex$STUDYID == "ABC101"))

# Align exposure with DM ----------------------------------------------------

ex <- ex[match(dm$USUBJID, ex$USUBJID), ]

stopifnot(all(ex$USUBJID == dm$USUBJID))
stopifnot(all(ex$EXTRT == dm$ACTARM))
stopifnot(all(ex$EXSTDTC == dm$RFXSTDTC))
stopifnot(all(ex$EXENDTC == dm$RFXENDTC))

# Treatment mappings -------------------------------------------------------

treatment_number <- c(
  "Placebo" = 0,
  "Drug 10 mg" = 1,
  "Drug 20 mg" = 2
)

stopifnot(all(dm$ARM %in% names(treatment_number)))
stopifnot(all(dm$ACTARM %in% names(treatment_number)))

treatment_start_date <- as.Date(ex$EXSTDTC)
treatment_end_date <- as.Date(ex$EXENDTC)
age <- as.integer(dm$AGE)

stopifnot(!anyNA(treatment_start_date), !anyNA(treatment_end_date))
stopifnot(all(treatment_end_date >= treatment_start_date))
stopifnot(!anyNA(age), all(age >= 18L))

# Create ADSL ---------------------------------------------------------------

adsl <- data.frame(
  STUDYID = dm$STUDYID,
  USUBJID = dm$USUBJID,
  SUBJID = dm$SUBJID,
  AGE = age,
  AGEU = dm$AGEU,
  SEX = dm$SEX,
  TRT01P = dm$ARM,
  TRT01PN = unname(treatment_number[dm$ARM]),
  TRT01A = dm$ACTARM,
  TRT01AN = unname(treatment_number[dm$ACTARM]),
  TRTSDT = treatment_start_date,
  TRTEDT = treatment_end_date,
  TRTDURD = as.integer(treatment_end_date - treatment_start_date) + 1L,
  ITTFL = "Y",
  SAFFL = "Y",
  stringsAsFactors = FALSE
)

adsl <- adsl[order(adsl$USUBJID), ]
row.names(adsl) <- NULL

# Validate the completed dataset -------------------------------------------

stopifnot(nrow(adsl) == nrow(dm))
stopifnot(!anyDuplicated(adsl$USUBJID))
stopifnot(!anyNA(adsl))
stopifnot(all(adsl$TRTDURD > 0L))
stopifnot(all(adsl$ITTFL == "Y"))
stopifnot(all(adsl$SAFFL == "Y"))

dir.create("data/adam", recursive = TRUE, showWarnings = FALSE)

write.csv(
  adsl,
  "data/adam/adsl.csv",
  row.names = FALSE,
  na = ""
)
