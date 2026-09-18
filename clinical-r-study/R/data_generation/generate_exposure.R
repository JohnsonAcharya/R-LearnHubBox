# ============================================================
# Program: generate_exposure.R
#
# Purpose:
#   Generate synthetic source exposure data for ABC101.
#   Each subject receives one tablet daily of the treatment
#   assigned in demographics, from Baseline through Week 12.
#
# Input:
#   data/raw/demographics.csv
#
# Output:
#   data/raw/exposure.csv
# ============================================================

demographics <- read.csv(
  "data/raw/demographics.csv",
  stringsAsFactors = FALSE
)

required_columns <- c("USUBJID", "TRT01P")
stopifnot(all(required_columns %in% names(demographics)))
stopifnot(!anyNA(demographics[required_columns]))
stopifnot(!anyDuplicated(demographics$USUBJID))

valid_treatments <- c("Placebo", "Drug 10 mg", "Drug 20 mg")
stopifnot(all(demographics$TRT01P %in% valid_treatments))

exposure <- data.frame(
  USUBJID = demographics$USUBJID,
  EXSEQ = 1L,
  EXTRT = demographics$TRT01P,
  EXDOSE = 1L,
  EXDOSU = "tablet",
  EXDOSFRQ = "QD",
  EXSTDTC = as.character(as.Date("2025-01-01")),
  EXENDTC = as.character(as.Date("2025-01-01") + 84L),
  stringsAsFactors = FALSE
)

exposure <- exposure[order(exposure$USUBJID), ]
row.names(exposure) <- NULL

write.csv(
  exposure,
  "data/raw/exposure.csv",
  row.names = FALSE
)
