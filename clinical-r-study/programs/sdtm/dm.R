# ============================================================
# Program: dm.R
#
# Purpose:
#   Create the SDTM Demographics (DM) domain for ABC101.
#
# Inputs:
#   data/raw/demographics.csv
#   data/raw/exposure.csv
#
# Output:
#   data/sdtm/dm.csv
# ============================================================

demographics <- read.csv(
  "data/raw/demographics.csv",
  stringsAsFactors = FALSE
)

exposure <- read.csv(
  "data/raw/exposure.csv",
  stringsAsFactors = FALSE
)

# Validate source structure -------------------------------------------------

dm_required <- c("USUBJID", "SEX", "AGE", "TRT01P")
ex_required <- c("USUBJID", "EXTRT", "EXSTDTC", "EXENDTC")

stopifnot(all(dm_required %in% names(demographics)))
stopifnot(all(ex_required %in% names(exposure)))
stopifnot(!anyNA(demographics[dm_required]))
stopifnot(!anyNA(exposure[ex_required]))
stopifnot(!anyDuplicated(demographics$USUBJID))
stopifnot(!anyDuplicated(exposure$USUBJID))
stopifnot(setequal(demographics$USUBJID, exposure$USUBJID))
stopifnot(all(demographics$SEX %in% c("M", "F")))
stopifnot(all(demographics$AGE >= 18 & demographics$AGE <= 80))
stopifnot(!anyNA(as.Date(exposure$EXSTDTC)))
stopifnot(!anyNA(as.Date(exposure$EXENDTC)))
stopifnot(all(as.Date(exposure$EXENDTC) >= as.Date(exposure$EXSTDTC)))

# Align exposure with the subject-level demographics records ----------------

exposure <- exposure[
  match(demographics$USUBJID, exposure$USUBJID),
]

# Controlled treatment mappings --------------------------------------------

arm_code <- c(
  "Placebo" = "PBO",
  "Drug 10 mg" = "DRUG10",
  "Drug 20 mg" = "DRUG20"
)

stopifnot(all(demographics$TRT01P %in% names(arm_code)))
stopifnot(all(exposure$EXTRT %in% names(arm_code)))

# Create DM -----------------------------------------------------------------

dm <- data.frame(
  STUDYID = "ABC101",
  DOMAIN = "DM",
  USUBJID = demographics$USUBJID,
  SUBJID = sub("^ABC-", "", demographics$USUBJID),
  RFSTDTC = exposure$EXSTDTC,
  RFENDTC = exposure$EXENDTC,
  RFXSTDTC = exposure$EXSTDTC,
  RFXENDTC = exposure$EXENDTC,
  AGE = demographics$AGE,
  AGEU = "YEARS",
  SEX = demographics$SEX,
  ARMCD = unname(arm_code[demographics$TRT01P]),
  ARM = demographics$TRT01P,
  ACTARMCD = unname(arm_code[exposure$EXTRT]),
  ACTARM = exposure$EXTRT,
  stringsAsFactors = FALSE
)

dm <- dm[order(dm$USUBJID), ]
row.names(dm) <- NULL

# Validate the completed domain --------------------------------------------

stopifnot(nrow(dm) == nrow(demographics))
stopifnot(!anyDuplicated(dm$USUBJID))
stopifnot(!anyNA(dm))
stopifnot(all(dm$ARM == demographics$TRT01P[match(dm$USUBJID, demographics$USUBJID)]))
stopifnot(all(dm$ACTARM == exposure$EXTRT[match(dm$USUBJID, exposure$USUBJID)]))

write.csv(
  dm,
  "data/sdtm/dm.csv",
  row.names = FALSE,
  na = ""
)
