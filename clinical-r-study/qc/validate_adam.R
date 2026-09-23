# ============================================================
# Program: validate_adam.R
#
# Purpose:
#   Validate ADSL, ADAE, ADLB, and ADVS together and against
#   their SDTM sources. Run from the project root.
#
# Output:
#   output/qc/adam_validation_report.md
#
# Exit status:
#   Nonzero when any validation check fails.
# ============================================================

read_data <- function(path) {
  read.csv(path, stringsAsFactors = FALSE, colClasses = "character",
           check.names = FALSE)
}

dm <- read_data("data/sdtm/dm.csv")
ae <- read_data("data/sdtm/ae.csv")
lb <- read_data("data/sdtm/lb.csv")
vs <- read_data("data/sdtm/vs.csv")
ex <- read_data("data/sdtm/ex.csv")
adsl <- read_data("data/adam/adsl.csv")
adae <- read_data("data/adam/adae.csv")
adlb <- read_data("data/adam/adlb.csv")
advs <- read_data("data/adam/advs.csv")

checks <- list()
add_check <- function(id, description, passed, detail) {
  checks[[length(checks) + 1L]] <<- data.frame(
    Check = id,
    Description = description,
    Status = if (isTRUE(passed)) "PASS" else "FAIL",
    Detail = as.character(detail),
    stringsAsFactors = FALSE
  )
}

same <- function(left, right) {
  length(left) == length(right) &&
    all(!is.na(left) & !is.na(right) & left == right)
}

same_number <- function(left, right, tolerance = 1e-8) {
  length(left) == length(right) &&
    all(is.finite(left) & is.finite(right) &
          abs(left - right) <= tolerance)
}

blank <- function(values) {
  is.na(values) | values == ""
}

unique_key <- function(data, columns) {
  !anyDuplicated(data[columns])
}

key <- function(data, sequence) {
  paste(data$USUBJID, data[[sequence]], sep = "|")
}

study_day <- function(event_date, reference_date) {
  difference <- as.integer(event_date - reference_date)
  ifelse(event_date >= reference_date, difference + 1L, difference)
}

expected_columns <- list(
  ADSL = c("STUDYID", "USUBJID", "SUBJID", "AGE", "AGEU", "SEX",
           "TRT01P", "TRT01PN", "TRT01A", "TRT01AN", "TRTSDT",
           "TRTEDT", "TRTDURD", "ITTFL", "SAFFL"),
  ADAE = c("STUDYID", "USUBJID", "SUBJID", "TRT01P", "TRT01PN",
           "TRT01A", "TRT01AN", "TRTSDT", "TRTEDT", "SAFFL",
           "ASEQ", "AESEQ", "AETERM", "AEDECOD", "AESEV", "AESER",
           "AEOUT", "AESTDTC", "AEENDTC", "ASTDT", "AENDT",
           "ASTDY", "AENDY", "EPOCH", "TRTEMFL", "SRCDOM",
           "SRCVAR", "SRCSEQ"),
  ADLB = c("STUDYID", "USUBJID", "SUBJID", "TRT01P", "TRT01PN",
           "TRT01A", "TRT01AN", "TRTSDT", "TRTEDT", "ITTFL",
           "SAFFL", "ASEQ", "LBSEQ", "PARAMCD", "PARAM", "PARCAT1",
           "AVAL", "AVALU", "ADT", "ADY", "AVISIT", "AVISITN",
           "ABLFL", "BLSEQ", "BASE", "CHG", "PCHG", "SRCDOM",
           "SRCVAR", "SRCSEQ"),
  ADVS = c("STUDYID", "USUBJID", "SUBJID", "TRT01P", "TRT01PN",
           "TRT01A", "TRT01AN", "TRTSDT", "TRTEDT", "ITTFL",
           "SAFFL", "ASEQ", "VSSEQ", "PARAMCD", "PARAM", "PARCAT1",
           "AVAL", "AVALU", "ADT", "ADY", "AVISIT", "AVISITN",
           "ABLFL", "BLSEQ", "BASE", "CHG", "PCHG", "SRCDOM",
           "SRCVAR", "SRCSEQ")
)

analysis <- list(ADSL = adsl, ADAE = adae, ADLB = adlb, ADVS = advs)
source <- list(ADSL = dm, ADAE = ae, ADLB = lb, ADVS = vs)
sequence <- list(ADAE = "AESEQ", ADLB = "LBSEQ", ADVS = "VSSEQ")

# Structure, counts, keys, and subject coverage -----------------------------

for (name in names(analysis)) {
  data <- analysis[[name]]
  expected <- expected_columns[[name]]
  add_check(paste0(name, "-01"), paste(name, "expected columns"),
            all(expected %in% names(data)),
            paste(length(intersect(expected, names(data))), "of",
                  length(expected), "present"))
  add_check(paste0(name, "-02"), paste(name, "row count matches source"),
            nrow(data) == nrow(source[[name]]),
            paste(nrow(data), "analysis rows;", nrow(source[[name]]),
                  "source rows"))
  key_columns <- if (name == "ADSL") "USUBJID" else c("USUBJID", "ASEQ")
  add_check(paste0(name, "-03"), paste(name, "analysis key is unique"),
            unique_key(data, key_columns),
            paste(key_columns, collapse = " + "))
  add_check(paste0(name, "-04"), paste(name, "has valid STUDYID"),
            all(data$STUDYID == "ABC101"), "Expected ABC101")
}

add_check("ADSL-05", "ADSL covers every DM and EX subject",
          setequal(adsl$USUBJID, dm$USUBJID) &&
            setequal(adsl$USUBJID, ex$USUBJID),
          paste(length(unique(adsl$USUBJID)), "ADSL subjects"))

for (name in c("ADAE", "ADLB", "ADVS")) {
  orphan_count <- length(setdiff(unique(analysis[[name]]$USUBJID), adsl$USUBJID))
  add_check(paste0(name, "-05"), paste(name, "subjects exist in ADSL"),
            orphan_count == 0L, paste(orphan_count, "orphan subjects"))
  source_keys <- key(source[[name]], sequence[[name]])
  analysis_keys <- key(analysis[[name]], "ASEQ")
  add_check(paste0(name, "-06"), paste(name, "covers each SDTM source record"),
            !anyDuplicated(source_keys) && setequal(analysis_keys, source_keys),
            paste(length(unique(analysis_keys)), "analysis keys;",
                  length(unique(source_keys)), "source keys"))
}

# ADSL against DM and EX ----------------------------------------------------

dm_index <- match(adsl$USUBJID, dm$USUBJID)
ex_index <- match(adsl$USUBJID, ex$USUBJID)
arm_number <- c("Placebo" = 0, "Drug 10 mg" = 1, "Drug 20 mg" = 2)

add_check("ADSL-X01", "Demographics match DM",
          same(adsl$SUBJID, dm$SUBJID[dm_index]) &&
            same(adsl$AGE, dm$AGE[dm_index]) &&
            same(adsl$AGEU, dm$AGEU[dm_index]) &&
            same(adsl$SEX, dm$SEX[dm_index]),
          "SUBJID, AGE, AGEU, SEX")
add_check("ADSL-X02", "Planned and actual treatments match DM and EX",
          same(adsl$TRT01P, dm$ARM[dm_index]) &&
            same(adsl$TRT01A, dm$ACTARM[dm_index]) &&
            same(adsl$TRT01A, ex$EXTRT[ex_index]),
          "TRT01P and TRT01A")
add_check("ADSL-X03", "Numeric treatment codes are consistent",
          same(as.numeric(adsl$TRT01PN), unname(arm_number[adsl$TRT01P])) &&
            same(as.numeric(adsl$TRT01AN), unname(arm_number[adsl$TRT01A])),
          "Placebo=0, Drug 10 mg=1, Drug 20 mg=2")
add_check("ADSL-X04", "Treatment dates and duration match EX",
          same(adsl$TRTSDT, ex$EXSTDTC[ex_index]) &&
            same(adsl$TRTEDT, ex$EXENDTC[ex_index]) &&
            same(as.integer(adsl$TRTDURD),
                 as.integer(as.Date(adsl$TRTEDT) -
                              as.Date(adsl$TRTSDT)) + 1L),
          "Inclusive treatment duration")
add_check("ADSL-X05", "Population flags match this study's enrolled and exposed subjects",
          all(adsl$ITTFL == "Y") && all(adsl$SAFFL == "Y"),
          paste(sum(adsl$ITTFL == "Y"), "ITT;",
                sum(adsl$SAFFL == "Y"), "safety"))

# Shared analysis treatment fields -----------------------------------------

for (name in c("ADAE", "ADLB", "ADVS")) {
  data <- analysis[[name]]
  index <- match(data$USUBJID, adsl$USUBJID)
  fields <- c("SUBJID", "TRT01P", "TRT01PN", "TRT01A",
              "TRT01AN", "TRTSDT", "TRTEDT", "SAFFL")
  if (name != "ADAE") fields <- c(fields, "ITTFL")
  agreement <- all(vapply(fields, function(field) {
    same(data[[field]], adsl[[field]][index])
  }, logical(1)))
  add_check(paste0(name, "-X01"),
            paste(name, "subject and treatment fields match ADSL"),
            agreement, paste(fields, collapse = ", "))
}

# ADAE source traceability and treatment-emergent definition ----------------

ae_index <- match(key(adae, "ASEQ"), key(ae, "AESEQ"))
ae_fields <- c("AETERM", "AEDECOD", "AESEV", "AESER", "AEOUT",
               "AESTDTC", "AEENDTC", "EPOCH")
add_check("ADAE-X02", "Event fields match SDTM AE",
          all(vapply(ae_fields, function(field) {
            same(adae[[field]], ae[[field]][ae_index])
          }, logical(1))),
          paste(ae_fields, collapse = ", "))
add_check("ADAE-X03", "Analysis and source dates and study days agree",
          same(adae$ASTDT, ae$AESTDTC[ae_index]) &&
            same(adae$AENDT, ae$AEENDTC[ae_index]) &&
            same(adae$ASTDY, ae$AESTDY[ae_index]) &&
            same(adae$AENDY, ae$AEENDY[ae_index]),
          "ASTDT, AENDT, ASTDY, AENDY")
add_check("ADAE-X04", "AE record traceability is complete",
          same(adae$AESEQ, ae$AESEQ[ae_index]) &&
            same(adae$SRCSEQ, adae$AESEQ) &&
            all(adae$SRCDOM == "AE") && all(adae$SRCVAR == "AESEQ"),
          "SRCDOM=AE; SRCVAR=AESEQ; SRCSEQ=AESEQ")

ae_start <- as.Date(adae$ASTDT)
trt_start <- as.Date(adae$TRTSDT)
trt_end <- as.Date(adae$TRTEDT)
expected_teae <- ae_start >= trt_start & ae_start <= trt_end + 30L
add_check("ADAE-X05", "TRTEMFL uses the inclusive 30-day window",
          same(adae$TRTEMFL, ifelse(expected_teae, "Y", "")),
          paste(sum(expected_teae), "treatment-emergent records"))
add_check("ADAE-X06", "TEAE flag and safety population are consistent",
          all(adae$TRTEMFL %in% c("Y", "")) &&
            all(adae$SAFFL[adae$TRTEMFL == "Y"] == "Y"),
          paste(sum(adae$TRTEMFL == "Y"), "flagged events"))

# ADLB and ADVS baseline and change calculations ----------------------------

validate_findings <- function(name, data, sdtm, source_sequence,
                              source_test, source_name, source_category,
                              source_value, source_unit, source_date,
                              source_day, source_baseline) {
  source_index <- match(key(data, "ASEQ"), key(sdtm, source_sequence))
  add_check(paste0(name, "-X02"),
            paste(name, "parameter, value, visit, and date match source"),
            same(data$PARAMCD, sdtm[[source_test]][source_index]) &&
              same(data$PARAM, sdtm[[source_name]][source_index]) &&
              same(data$PARCAT1, sdtm[[source_category]][source_index]) &&
              same_number(as.numeric(data$AVAL),
                          as.numeric(sdtm[[source_value]][source_index])) &&
              same(data$AVALU, sdtm[[source_unit]][source_index]) &&
              same(data$ADT, sdtm[[source_date]][source_index]) &&
              same(data$ADY, sdtm[[source_day]][source_index]) &&
              same(data$AVISIT, sdtm$VISIT[source_index]) &&
              same(data$AVISITN, sdtm$VISITNUM[source_index]),
            "PARAM, AVAL, AVALU, ADT, ADY, AVISIT")

  add_check(paste0(name, "-X03"),
            paste(name, "record traceability matches source"),
            same(data[[source_sequence]], sdtm[[source_sequence]][source_index]) &&
              same(data$SRCSEQ, data[[source_sequence]]) &&
              all(data$SRCDOM == substring(name, 3L)) &&
              all(data$SRCVAR == source_sequence),
            paste("SRCDOM=", substring(name, 3L),
                  "; SRCVAR=", source_sequence, sep = ""))

  group_key <- paste(sdtm$USUBJID, sdtm[[source_test]], sep = "|")
  baseline_rows <- which(sdtm[[source_baseline]] == "Y")
  baseline_key <- group_key[baseline_rows]
  baseline_count_ok <- !anyDuplicated(baseline_key) &&
    setequal(unique(group_key), baseline_key)
  add_check(paste0(name, "-X04"),
            paste(name, "has one baseline per subject and test"),
            baseline_count_ok,
            paste(length(baseline_rows), "source baseline records for",
                  length(unique(group_key)), "subject-test groups"))

  baseline_index <- baseline_rows[match(
    paste(data$USUBJID, data$PARAMCD, sep = "|"), baseline_key
  )]
  expected_base <- as.numeric(sdtm[[source_value]][baseline_index])
  expected_blseq <- sdtm[[source_sequence]][baseline_index]
  add_check(paste0(name, "-X05"),
            paste(name, "BASE and BLSEQ trace to source baseline"),
            baseline_count_ok &&
              same_number(as.numeric(data$BASE), expected_base) &&
              same(data$BLSEQ, expected_blseq) &&
              same(data$ABLFL, sdtm[[source_baseline]][source_index]),
            "BASE, BLSEQ, ABLFL")

  is_baseline <- data$ABLFL == "Y"
  postbaseline <- !is_baseline
  expected_change <- round(as.numeric(data$AVAL) - expected_base, 4)
  add_check(paste0(name, "-X06"),
            paste(name, "baseline change fields are blank"),
            all(blank(data$CHG[is_baseline])) &&
              all(blank(data$PCHG[is_baseline])),
            paste(sum(is_baseline), "baseline rows"))
  add_check(paste0(name, "-X07"),
            paste(name, "postbaseline CHG equals AVAL minus BASE"),
            same_number(as.numeric(data$CHG[postbaseline]),
                        expected_change[postbaseline]),
            paste(sum(postbaseline), "postbaseline rows; rounded to 4 decimals"))

  percent_eligible <- postbaseline & expected_base != 0
  expected_percent <- round(
    100 * expected_change[percent_eligible] /
      expected_base[percent_eligible], 4
  )
  add_check(paste0(name, "-X08"),
            paste(name, "postbaseline PCHG uses BASE denominator"),
            same_number(as.numeric(data$PCHG[percent_eligible]),
                        expected_percent) &&
              all(blank(data$PCHG[postbaseline & expected_base == 0])),
            paste(sum(percent_eligible), "nonzero-baseline rows;",
                  sum(postbaseline & expected_base == 0), "zero-baseline rows"))

  baseline_date <- as.Date(sdtm[[source_date]][baseline_index])
  add_check(paste0(name, "-X09"),
            paste(name, "postbaseline dates follow baseline and treatment start"),
            all(as.Date(data$ADT[postbaseline]) > baseline_date[postbaseline]) &&
              all(as.Date(data$ADT[postbaseline]) >
                    as.Date(data$TRTSDT[postbaseline])),
            paste(sum(postbaseline), "postbaseline dates checked"))
}

validate_findings("ADLB", adlb, lb, "LBSEQ", "LBTESTCD", "LBTEST",
                  "LBCAT", "LBSTRESN", "LBSTRESU", "LBDTC", "LBDY",
                  "LBBLFL")
validate_findings("ADVS", advs, vs, "VSSEQ", "VSTESTCD", "VSTEST",
                  "VSCAT", "VSSTRESN", "VSSTRESU", "VSDTC", "VSDY",
                  "VSBLFL")

# Report --------------------------------------------------------------------

results <- do.call(rbind, checks)
passed <- sum(results$Status == "PASS")
failed <- sum(results$Status == "FAIL")
status <- if (failed == 0L) "PASS" else "FAIL"
report_rows <- apply(results, 1L, function(row) {
  paste0("| ", paste(gsub("\\|", "\\\\|", row), collapse = " | "), " |")
})
report <- c(
  "# ABC101 ADaM Validation Report",
  "",
  paste0("**Overall status:** ", status),
  "",
  paste0("**Datasets:** ADSL, ADAE, ADLB, ADVS  "),
  paste0("**Checks:** ", nrow(results), " total; ", passed,
         " passed; ", failed, " failed"),
  "",
  "| Check | Description | Status | Detail |",
  "|---|---|---:|---|",
  report_rows,
  "",
  "## Dataset summary",
  "",
  "| Dataset | Rows | Subjects |",
  "|---|---:|---:|",
  vapply(names(analysis), function(name) {
    data <- analysis[[name]]
    paste0("| ", name, " | ", nrow(data), " | ",
           length(unique(data$USUBJID)), " |")
  }, character(1)),
  "",
  paste0("Treatment-emergent AE records: ", sum(adae$TRTEMFL == "Y")),
  paste0("ADLB baseline records: ", sum(adlb$ABLFL == "Y")),
  paste0("ADVS baseline records: ", sum(advs$ABLFL == "Y")),
  "",
  "## Interpretation",
  "",
  if (failed == 0L) {
    "All implemented ADaM and source traceability checks passed."
  } else {
    paste("Failed checks:",
          paste(results$Check[results$Status == "FAIL"], collapse = ", "))
  },
  ""
)

dir.create("output/qc", recursive = TRUE, showWarnings = FALSE)
writeLines(report, "output/qc/adam_validation_report.md")
cat(sprintf("ADaM validation: %s (%d passed, %d failed)\n",
            status, passed, failed))

if (failed > 0L) {
  stop("ADaM validation failed; see output/qc/adam_validation_report.md",
       call. = FALSE)
}
