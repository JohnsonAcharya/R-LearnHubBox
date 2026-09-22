# ============================================================
# Program: dm.R
#
# Purpose:
#   Create the SDTM Demographics (DM) domain for ABC101.
#
# Inputs:
#   data/raw/demographics.csv
#   data/raw/exposure.csv
#   data/raw/adverse_events.csv
#   data/raw/labs.csv
#   data/raw/vital_signs.csv
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

adverse_events <- read.csv(
  "data/raw/adverse_events.csv",
  stringsAsFactors = FALSE
)

labs <- read.csv(
  "data/raw/labs.csv",
  stringsAsFactors = FALSE
)

vital_signs <- read.csv(
  "data/raw/vital_signs.csv",
  stringsAsFactors = FALSE
)

# Validate source structure -------------------------------------------------

dm_required <- c("USUBJID", "SEX", "AGE", "TRT01P")
ex_required <- c("USUBJID", "EXTRT", "EXSTDTC", "EXENDTC")
ae_required <- c("USUBJID", "AESTDTC", "AEENDTC")
lb_required <- c("USUBJID", "LBDTC")
vs_required <- c("USUBJID", "VSDTC")

stopifnot(all(dm_required %in% names(demographics)))
stopifnot(all(ex_required %in% names(exposure)))
stopifnot(all(ae_required %in% names(adverse_events)))
stopifnot(all(lb_required %in% names(labs)))
stopifnot(all(vs_required %in% names(vital_signs)))
stopifnot(!anyNA(demographics[dm_required]))
stopifnot(!anyNA(exposure[ex_required]))
stopifnot(!anyNA(adverse_events[ae_required]))
stopifnot(!anyNA(labs[lb_required]))
stopifnot(!anyNA(vital_signs[vs_required]))
stopifnot(!anyDuplicated(demographics$USUBJID))
stopifnot(!anyDuplicated(exposure$USUBJID))
stopifnot(setequal(demographics$USUBJID, exposure$USUBJID))
stopifnot(all(adverse_events$USUBJID %in% demographics$USUBJID))
stopifnot(all(labs$USUBJID %in% demographics$USUBJID))
stopifnot(all(vital_signs$USUBJID %in% demographics$USUBJID))
stopifnot(all(demographics$SEX %in% c("M", "F")))
stopifnot(all(demographics$AGE >= 18 & demographics$AGE <= 80))
stopifnot(!anyNA(as.Date(exposure$EXSTDTC)))
stopifnot(!anyNA(as.Date(exposure$EXENDTC)))
stopifnot(all(as.Date(exposure$EXENDTC) >= as.Date(exposure$EXSTDTC)))

# Derive each subject's reference period from all known study activity -------

activity <- rbind(
  data.frame(
    USUBJID = exposure$USUBJID,
    START = exposure$EXSTDTC,
    END = exposure$EXENDTC
  ),
  data.frame(
    USUBJID = adverse_events$USUBJID,
    START = adverse_events$AESTDTC,
    END = adverse_events$AEENDTC
  ),
  data.frame(
    USUBJID = labs$USUBJID,
    START = labs$LBDTC,
    END = labs$LBDTC
  ),
  data.frame(
    USUBJID = vital_signs$USUBJID,
    START = vital_signs$VSDTC,
    END = vital_signs$VSDTC
  )
)

activity$START <- as.Date(activity$START)
activity$END <- as.Date(activity$END)
stopifnot(!anyNA(activity$START), !anyNA(activity$END))

first_activity <- aggregate(START ~ USUBJID, activity, min)
last_activity <- aggregate(END ~ USUBJID, activity, max)

reference_start <- first_activity$START[
  match(demographics$USUBJID, first_activity$USUBJID)
]
reference_end <- last_activity$END[
  match(demographics$USUBJID, last_activity$USUBJID)
]

stopifnot(!anyNA(reference_start), !anyNA(reference_end))
stopifnot(all(reference_end >= reference_start))

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
  RFSTDTC = as.character(reference_start),
  RFENDTC = as.character(reference_end),
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
