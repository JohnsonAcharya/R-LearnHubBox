# ============================================================
# Program: validate_sdtm.R
#
# Purpose:
#   Run reusable structural and cross-domain validation for
#   SDTM DM, AE, LB, VS, and EX in study ABC101.
#
# Output:
#   output/qc/sdtm_validation_report.md
# ============================================================

read_sdtm <- function(domain) {
  read.csv(
    file.path("data", "sdtm", paste0(tolower(domain), ".csv")),
    stringsAsFactors = FALSE,
    colClasses = "character",
    check.names = FALSE
  )
}

dm <- read_sdtm("DM")
ae <- read_sdtm("AE")
lb <- read_sdtm("LB")
vs <- read_sdtm("VS")
ex <- read_sdtm("EX")

checks <- list()

add_check <- function(check_id, description, passed, details) {
  checks[[length(checks) + 1L]] <<- data.frame(
    Check = check_id,
    Description = description,
    Status = if (isTRUE(passed)) "PASS" else "FAIL",
    Details = as.character(details),
    stringsAsFactors = FALSE
  )
}

has_columns <- function(data, columns) all(columns %in% names(data))
unique_key <- function(data, columns) !anyDuplicated(data[columns])
valid_dates <- function(values) !anyNA(as.Date(values))

study_day <- function(event_date, reference_date) {
  difference <- as.integer(event_date - reference_date)
  ifelse(event_date >= reference_date, difference + 1L, difference)
}

expected_columns <- list(
  DM = c("STUDYID", "DOMAIN", "USUBJID", "SUBJID", "RFSTDTC", "RFENDTC",
         "RFXSTDTC", "RFXENDTC", "AGE", "AGEU", "SEX", "ARMCD", "ARM",
         "ACTARMCD", "ACTARM"),
  AE = c("STUDYID", "DOMAIN", "USUBJID", "AESEQ", "AETERM", "AEDECOD",
         "AESEV", "AESER", "AEOUT", "AESTDTC", "AEENDTC", "AESTDY",
         "AEENDY", "EPOCH"),
  LB = c("STUDYID", "DOMAIN", "USUBJID", "LBSEQ", "LBTESTCD", "LBTEST",
         "LBCAT", "LBORRES", "LBORRESU", "LBSTRESC", "LBSTRESN",
         "LBSTRESU", "LBBLFL", "VISITNUM", "VISIT", "LBDTC", "LBDY",
         "EPOCH"),
  VS = c("STUDYID", "DOMAIN", "USUBJID", "VSSEQ", "VSTESTCD", "VSTEST",
         "VSCAT", "VSORRES", "VSORRESU", "VSSTRESC", "VSSTRESN",
         "VSSTRESU", "VSBLFL", "VISITNUM", "VISIT", "VSDTC", "VSDY",
         "EPOCH"),
  EX = c("STUDYID", "DOMAIN", "USUBJID", "EXSEQ", "EXTRT", "EXCAT",
         "EXDOSE", "EXDOSU", "EXDOSFRQ", "EXSTDTC", "EXENDTC",
         "EXSTDY", "EXENDY", "EPOCH")
)

domains <- list(DM = dm, AE = ae, LB = lb, VS = vs, EX = ex)
expected_rows <- c(DM = 500L, AE = 750L, LB = 10000L, VS = 12500L, EX = 500L)
keys <- list(DM = "USUBJID", AE = c("USUBJID", "AESEQ"),
             LB = c("USUBJID", "LBSEQ"), VS = c("USUBJID", "VSSEQ"),
             EX = c("USUBJID", "EXSEQ"))

for (domain in names(domains)) {
  data <- domains[[domain]]
  add_check(
    paste0(domain, "-01"),
    paste(domain, "contains all expected variables"),
    has_columns(data, expected_columns[[domain]]),
    paste(length(intersect(names(data), expected_columns[[domain]])), "of",
          length(expected_columns[[domain]]), "expected variables present")
  )
  add_check(
    paste0(domain, "-02"),
    paste(domain, "has the expected record count"),
    nrow(data) == expected_rows[[domain]],
    paste(nrow(data), "records; expected", expected_rows[[domain]])
  )
  add_check(
    paste0(domain, "-03"),
    paste(domain, "key is unique"),
    unique_key(data, keys[[domain]]),
    paste("Key:", paste(keys[[domain]], collapse = " + "))
  )
  add_check(
    paste0(domain, "-04"),
    paste(domain, "has valid study and domain identifiers"),
    all(data$STUDYID == "ABC101") && all(data$DOMAIN == domain),
    "Expected STUDYID=ABC101 and matching DOMAIN"
  )
}

# Referential integrity -----------------------------------------------------

for (domain in c("AE", "LB", "VS", "EX")) {
  orphan_subjects <- setdiff(unique(domains[[domain]]$USUBJID), dm$USUBJID)
  add_check(
    paste0("REF-", domain),
    paste(domain, "subjects exist in DM"),
    length(orphan_subjects) == 0L,
    paste(length(orphan_subjects), "orphan subjects")
  )
}

dm_index_ae <- match(ae$USUBJID, dm$USUBJID)
dm_index_lb <- match(lb$USUBJID, dm$USUBJID)
dm_index_vs <- match(vs$USUBJID, dm$USUBJID)
dm_index_ex <- match(ex$USUBJID, dm$USUBJID)

# DM reference-period validation -------------------------------------------

activity <- rbind(
  data.frame(USUBJID = ex$USUBJID, START = ex$EXSTDTC, END = ex$EXENDTC),
  data.frame(USUBJID = ae$USUBJID, START = ae$AESTDTC, END = ae$AEENDTC),
  data.frame(USUBJID = lb$USUBJID, START = lb$LBDTC, END = lb$LBDTC),
  data.frame(USUBJID = vs$USUBJID, START = vs$VSDTC, END = vs$VSDTC)
)

first_activity <- aggregate(as.Date(activity$START), list(activity$USUBJID), min)
last_activity <- aggregate(as.Date(activity$END), list(activity$USUBJID), max)
names(first_activity) <- c("USUBJID", "DATE")
names(last_activity) <- c("USUBJID", "DATE")
first_activity <- first_activity$DATE[match(dm$USUBJID, first_activity$USUBJID)]
last_activity <- last_activity$DATE[match(dm$USUBJID, last_activity$USUBJID)]

add_check("DM-X01", "DM reference start equals first known study activity",
          all(as.Date(dm$RFSTDTC) == first_activity),
          paste(sum(as.Date(dm$RFSTDTC) != first_activity), "subjects differ"))
add_check("DM-X02", "DM reference end equals last known study activity",
          all(as.Date(dm$RFENDTC) == last_activity),
          paste(sum(as.Date(dm$RFENDTC) != last_activity), "subjects differ"))
add_check("DM-X03", "DM exposure dates agree with EX",
          all(dm$RFXSTDTC[dm_index_ex] == ex$EXSTDTC) &&
            all(dm$RFXENDTC[dm_index_ex] == ex$EXENDTC),
          "Compared RFXSTDTC/RFXENDTC with EX interval")
add_check("DM-X04", "DM actual arm agrees with EX treatment",
          all(dm$ACTARM[dm_index_ex] == ex$EXTRT),
          paste(sum(dm$ACTARM[dm_index_ex] != ex$EXTRT), "records differ"))

# Date, study-day, and epoch validation -------------------------------------

ae_start <- as.Date(ae$AESTDTC); ae_end <- as.Date(ae$AEENDTC)
ae_ref <- as.Date(dm$RFSTDTC[dm_index_ae])
ae_ex_start <- as.Date(dm$RFXSTDTC[dm_index_ae]); ae_ex_end <- as.Date(dm$RFXENDTC[dm_index_ae])
ae_epoch <- ifelse(ae_start < ae_ex_start, "PRE-TREATMENT",
                   ifelse(ae_start <= ae_ex_end, "TREATMENT", "FOLLOW-UP"))
add_check("AE-X01", "AE dates are valid and ordered",
          valid_dates(ae$AESTDTC) && valid_dates(ae$AEENDTC) && all(ae_end >= ae_start),
          paste(sum(ae_end < ae_start), "end dates precede start dates"))
add_check("AE-X02", "AE study days agree with DM RFSTDTC",
          all(as.integer(ae$AESTDY) == study_day(ae_start, ae_ref)) &&
            all(as.integer(ae$AEENDY) == study_day(ae_end, ae_ref)),
          "Validated AESTDY and AEENDY")
add_check("AE-X03", "AE epochs agree with DM exposure dates",
          all(ae$EPOCH == ae_epoch),
          paste(sum(ae$EPOCH != ae_epoch), "records differ"))
add_check("AE-X04", "AE dates fall within each subject reference period",
          all(ae_start >= ae_ref) && all(ae_end <= as.Date(dm$RFENDTC[dm_index_ae])),
          paste(sum(ae_end > as.Date(dm$RFENDTC[dm_index_ae])),
                "AE end dates exceed RFENDTC"))

validate_findings <- function(data, prefix, date_var, day_var, result_char,
                              result_num, baseline_flag, dm_index) {
  event_date <- as.Date(data[[date_var]])
  reference_date <- as.Date(dm$RFSTDTC[dm_index])
  exposure_start <- as.Date(dm$RFXSTDTC[dm_index])
  exposure_end <- as.Date(dm$RFXENDTC[dm_index])
  expected_epoch <- ifelse(event_date < exposure_start, "PRE-TREATMENT",
                           ifelse(event_date <= exposure_end, "TREATMENT", "FOLLOW-UP"))
  add_check(paste0(prefix, "-X01"), paste(prefix, "dates and study days are valid"),
            valid_dates(data[[date_var]]) &&
              all(as.integer(data[[day_var]]) == study_day(event_date, reference_date)),
            paste("Validated", date_var, "and", day_var))
  add_check(paste0(prefix, "-X02"), paste(prefix, "epochs agree with DM exposure dates"),
            all(data$EPOCH == expected_epoch),
            paste(sum(data$EPOCH != expected_epoch), "records differ"))
  add_check(paste0(prefix, "-X03"), paste(prefix, "standard character and numeric results agree"),
            all(as.numeric(data[[result_char]]) == as.numeric(data[[result_num]])),
            paste("Compared", result_char, "with", result_num))
  add_check(paste0(prefix, "-X04"), paste(prefix, "baseline flag agrees with Baseline visit"),
            all(data[[baseline_flag]] == ifelse(data$VISIT == "Baseline", "Y", "")),
            paste(sum(data[[baseline_flag]] != ifelse(data$VISIT == "Baseline", "Y", "")),
                  "records differ"))
}

validate_findings(lb, "LB", "LBDTC", "LBDY", "LBSTRESC", "LBSTRESN", "LBBLFL", dm_index_lb)
validate_findings(vs, "VS", "VSDTC", "VSDY", "VSSTRESC", "VSSTRESN", "VSBLFL", dm_index_vs)

lb_subject_counts <- table(lb$USUBJID)
vs_subject_counts <- table(vs$USUBJID)
add_check("LB-X05", "LB has 20 scheduled records per DM subject",
          length(lb_subject_counts) == nrow(dm) && all(lb_subject_counts == 20L),
          paste(range(lb_subject_counts), collapse = " to "))
add_check("VS-X05", "VS has 25 scheduled records per DM subject",
          length(vs_subject_counts) == nrow(dm) && all(vs_subject_counts == 25L),
          paste(range(vs_subject_counts), collapse = " to "))

ex_start <- as.Date(ex$EXSTDTC); ex_end <- as.Date(ex$EXENDTC)
ex_ref <- as.Date(dm$RFSTDTC[dm_index_ex])
add_check("EX-X01", "EX dates are valid and ordered",
          valid_dates(ex$EXSTDTC) && valid_dates(ex$EXENDTC) && all(ex_end >= ex_start),
          paste(sum(ex_end < ex_start), "end dates precede start dates"))
add_check("EX-X02", "EX study days agree with DM RFSTDTC",
          all(as.integer(ex$EXSTDY) == study_day(ex_start, ex_ref)) &&
            all(as.integer(ex$EXENDY) == study_day(ex_end, ex_ref)),
          "Validated EXSTDY and EXENDY")
add_check("EX-X03", "EX contains one treatment interval per DM subject",
          nrow(ex) == nrow(dm) && setequal(ex$USUBJID, dm$USUBJID),
          paste(nrow(ex), "EX records for", nrow(dm), "DM subjects"))

# Write report --------------------------------------------------------------

results <- do.call(rbind, checks)
pass_count <- sum(results$Status == "PASS")
fail_count <- sum(results$Status == "FAIL")
overall_status <- if (fail_count == 0L) "PASS" else "FAIL"

escape_markdown <- function(value) gsub("\\|", "\\\\|", value)
report_rows <- apply(results, 1L, function(row) {
  paste0("| ", paste(escape_markdown(row), collapse = " | "), " |")
})

report <- c(
  "# ABC101 SDTM Cross-Domain Validation Report",
  "",
  paste0("**Overall status:** ", overall_status),
  "",
  paste0("**Domains:** DM, AE, LB, VS, EX  "),
  paste0("**Checks:** ", nrow(results), " total; ", pass_count,
         " passed; ", fail_count, " failed"),
  "",
  "| Check | Description | Status | Details |",
  "|---|---|---:|---|",
  report_rows,
  "",
  "## Interpretation",
  "",
  if (fail_count == 0L) {
    "All implemented structural and cross-domain checks passed."
  } else {
    paste("Failed checks:", paste(results$Check[results$Status == "FAIL"], collapse = ", "))
  }
)

dir.create(file.path("output", "qc"), recursive = TRUE, showWarnings = FALSE)
writeLines(report, file.path("output", "qc", "sdtm_validation_report.md"))

cat(paste0("SDTM validation: ", overall_status, " (", pass_count,
           " passed, ", fail_count, " failed)\n"))

if (fail_count > 0L) {
  stop("SDTM validation failed; see output/qc/sdtm_validation_report.md", call. = FALSE)
}
