# ============================================================
# Program: lb.R
#
# Purpose:
#   Create the SDTM Laboratory Test Results (LB) domain for
#   ABC101.
#
# Inputs:
#   data/raw/labs.csv
#   data/sdtm/dm.csv
#
# Output:
#   data/sdtm/lb.csv
# ============================================================

raw_lb <- read.csv(
  "data/raw/labs.csv",
  stringsAsFactors = FALSE,
  colClasses = "character"
)

dm <- read.csv(
  "data/sdtm/dm.csv",
  stringsAsFactors = FALSE
)

# Validate source structure -------------------------------------------------

lb_required <- c(
  "USUBJID", "LBSEQ", "VISIT", "LBDTC", "LBTESTCD",
  "LBTEST", "LBORRES", "LBORRESU"
)
dm_required <- c("USUBJID", "RFSTDTC", "RFXSTDTC", "RFXENDTC")

stopifnot(all(lb_required %in% names(raw_lb)))
stopifnot(all(dm_required %in% names(dm)))
stopifnot(!anyNA(raw_lb[lb_required]))
stopifnot(!anyNA(dm[dm_required]))
stopifnot(!any(raw_lb[lb_required] == ""))
stopifnot(!anyDuplicated(dm$USUBJID))
stopifnot(!anyDuplicated(raw_lb[c("USUBJID", "LBSEQ")]))
stopifnot(all(raw_lb$USUBJID %in% dm$USUBJID))

lb_sequence <- as.integer(raw_lb$LBSEQ)
lb_result_numeric <- as.numeric(raw_lb$LBORRES)
lb_date <- as.Date(raw_lb$LBDTC)

stopifnot(!anyNA(lb_sequence), all(lb_sequence >= 1L))
stopifnot(!anyNA(lb_result_numeric))
stopifnot(!anyNA(lb_date))

# Controlled mappings ------------------------------------------------------

test_category <- c(
  "ALT" = "CHEMISTRY",
  "AST" = "CHEMISTRY",
  "CREAT" = "CHEMISTRY",
  "HGB" = "HEMATOLOGY"
)

standard_unit <- c(
  "ALT" = "U/L",
  "AST" = "U/L",
  "CREAT" = "mg/dL",
  "HGB" = "g/dL"
)

visit_number <- c(
  "Baseline" = 1,
  "Week 2" = 2,
  "Week 4" = 3,
  "Week 8" = 4,
  "Week 12" = 5
)

stopifnot(all(raw_lb$LBTESTCD %in% names(test_category)))
stopifnot(all(raw_lb$VISIT %in% names(visit_number)))
stopifnot(all(raw_lb$LBORRESU == unname(standard_unit[raw_lb$LBTESTCD])))

# Add subject-level reference dates -----------------------------------------

dm_index <- match(raw_lb$USUBJID, dm$USUBJID)
reference_start_date <- as.Date(dm$RFSTDTC[dm_index])
exposure_start_date <- as.Date(dm$RFXSTDTC[dm_index])
exposure_end_date <- as.Date(dm$RFXENDTC[dm_index])

derive_study_day <- function(event_date, reference_date) {
  difference <- as.integer(event_date - reference_date)
  ifelse(event_date >= reference_date, difference + 1L, difference)
}

epoch <- ifelse(
  lb_date < exposure_start_date,
  "PRE-TREATMENT",
  ifelse(lb_date <= exposure_end_date, "TREATMENT", "FOLLOW-UP")
)

# Create LB -----------------------------------------------------------------

lb <- data.frame(
  STUDYID = "ABC101",
  DOMAIN = "LB",
  USUBJID = raw_lb$USUBJID,
  LBSEQ = lb_sequence,
  LBTESTCD = raw_lb$LBTESTCD,
  LBTEST = raw_lb$LBTEST,
  LBCAT = unname(test_category[raw_lb$LBTESTCD]),
  LBORRES = raw_lb$LBORRES,
  LBORRESU = raw_lb$LBORRESU,
  LBSTRESC = raw_lb$LBORRES,
  LBSTRESN = lb_result_numeric,
  LBSTRESU = unname(standard_unit[raw_lb$LBTESTCD]),
  LBBLFL = ifelse(raw_lb$VISIT == "Baseline", "Y", ""),
  VISITNUM = unname(visit_number[raw_lb$VISIT]),
  VISIT = raw_lb$VISIT,
  LBDTC = raw_lb$LBDTC,
  LBDY = derive_study_day(lb_date, reference_start_date),
  EPOCH = epoch,
  stringsAsFactors = FALSE
)

lb <- lb[order(lb$USUBJID, lb$LBSEQ), ]
row.names(lb) <- NULL

# Validate the completed domain --------------------------------------------

stopifnot(nrow(lb) == nrow(raw_lb))
stopifnot(!anyDuplicated(lb[c("USUBJID", "LBSEQ")]))
stopifnot(!anyNA(lb))
stopifnot(all(lb$LBBLFL %in% c("Y", "")))
stopifnot(all(lb$EPOCH %in% c("PRE-TREATMENT", "TREATMENT", "FOLLOW-UP")))
stopifnot(all(lb$LBSTRESN == as.numeric(lb$LBSTRESC)))

write.csv(
  lb,
  "data/sdtm/lb.csv",
  row.names = FALSE,
  na = ""
)
