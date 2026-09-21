# ============================================================
# Program: vs.R
#
# Purpose:
#   Create the SDTM Vital Signs (VS) domain for ABC101.
#
# Inputs:
#   data/raw/vital_signs.csv
#   data/sdtm/dm.csv
#
# Output:
#   data/sdtm/vs.csv
# ============================================================

raw_vs <- read.csv(
  "data/raw/vital_signs.csv",
  stringsAsFactors = FALSE,
  colClasses = "character"
)

dm <- read.csv(
  "data/sdtm/dm.csv",
  stringsAsFactors = FALSE
)

# Validate source structure -------------------------------------------------

vs_required <- c(
  "USUBJID", "VSSEQ", "VISIT", "VSDTC", "VSTESTCD",
  "VSTEST", "VSORRES", "VSORRESU"
)
dm_required <- c("USUBJID", "RFSTDTC", "RFXSTDTC", "RFXENDTC")

stopifnot(all(vs_required %in% names(raw_vs)))
stopifnot(all(dm_required %in% names(dm)))
stopifnot(!anyNA(raw_vs[vs_required]))
stopifnot(!anyNA(dm[dm_required]))
stopifnot(!any(raw_vs[vs_required] == ""))
stopifnot(!anyDuplicated(dm$USUBJID))
stopifnot(!anyDuplicated(raw_vs[c("USUBJID", "VSSEQ")]))
stopifnot(all(raw_vs$USUBJID %in% dm$USUBJID))

vs_sequence <- as.integer(raw_vs$VSSEQ)
vs_result_numeric <- as.numeric(raw_vs$VSORRES)
vs_date <- as.Date(raw_vs$VSDTC)

stopifnot(!anyNA(vs_sequence), all(vs_sequence >= 1L))
stopifnot(!anyNA(vs_result_numeric))
stopifnot(!anyNA(vs_date))

# Controlled mappings ------------------------------------------------------

standard_unit <- c(
  "DIABP" = "mmHg",
  "PULSE" = "bpm",
  "SYSBP" = "mmHg",
  "TEMP" = "C",
  "WEIGHT" = "kg"
)

visit_number <- c(
  "Baseline" = 1,
  "Week 2" = 2,
  "Week 4" = 3,
  "Week 8" = 4,
  "Week 12" = 5
)

stopifnot(all(raw_vs$VSTESTCD %in% names(standard_unit)))
stopifnot(all(raw_vs$VISIT %in% names(visit_number)))
stopifnot(all(raw_vs$VSORRESU == unname(standard_unit[raw_vs$VSTESTCD])))

# Add subject-level reference dates -----------------------------------------

dm_index <- match(raw_vs$USUBJID, dm$USUBJID)
reference_start_date <- as.Date(dm$RFSTDTC[dm_index])
exposure_start_date <- as.Date(dm$RFXSTDTC[dm_index])
exposure_end_date <- as.Date(dm$RFXENDTC[dm_index])

derive_study_day <- function(event_date, reference_date) {
  difference <- as.integer(event_date - reference_date)
  ifelse(event_date >= reference_date, difference + 1L, difference)
}

epoch <- ifelse(
  vs_date < exposure_start_date,
  "PRE-TREATMENT",
  ifelse(vs_date <= exposure_end_date, "TREATMENT", "FOLLOW-UP")
)

# Create VS -----------------------------------------------------------------

vs <- data.frame(
  STUDYID = "ABC101",
  DOMAIN = "VS",
  USUBJID = raw_vs$USUBJID,
  VSSEQ = vs_sequence,
  VSTESTCD = raw_vs$VSTESTCD,
  VSTEST = raw_vs$VSTEST,
  VSCAT = "VITAL SIGNS",
  VSORRES = raw_vs$VSORRES,
  VSORRESU = raw_vs$VSORRESU,
  VSSTRESC = raw_vs$VSORRES,
  VSSTRESN = vs_result_numeric,
  VSSTRESU = unname(standard_unit[raw_vs$VSTESTCD]),
  VSBLFL = ifelse(raw_vs$VISIT == "Baseline", "Y", ""),
  VISITNUM = unname(visit_number[raw_vs$VISIT]),
  VISIT = raw_vs$VISIT,
  VSDTC = raw_vs$VSDTC,
  VSDY = derive_study_day(vs_date, reference_start_date),
  EPOCH = epoch,
  stringsAsFactors = FALSE
)

vs <- vs[order(vs$USUBJID, vs$VSSEQ), ]
row.names(vs) <- NULL

# Validate the completed domain --------------------------------------------

stopifnot(nrow(vs) == nrow(raw_vs))
stopifnot(!anyDuplicated(vs[c("USUBJID", "VSSEQ")]))
stopifnot(!anyNA(vs))
stopifnot(all(vs$VSBLFL %in% c("Y", "")))
stopifnot(all(vs$EPOCH %in% c("PRE-TREATMENT", "TREATMENT", "FOLLOW-UP")))
stopifnot(all(vs$VSSTRESN == as.numeric(vs$VSSTRESC)))

write.csv(
  vs,
  "data/sdtm/vs.csv",
  row.names = FALSE,
  na = ""
)
