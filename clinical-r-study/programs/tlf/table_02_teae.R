# ============================================================
# Program: table_02_teae.R
#
# Purpose:
#   Summarize treatment-emergent adverse events (TEAEs) by
#   actual treatment for the safety population in ABC101.
#
# Inputs:
#   data/adam/adsl.csv
#   data/adam/adae.csv
#
# Outputs:
#   output/tables/table_02_teae.csv
#   output/tables/table_02_teae.md
#
# Run from the project root.
# ============================================================

adsl <- read.csv("data/adam/adsl.csv", stringsAsFactors = FALSE,
                 colClasses = "character")
adae <- read.csv("data/adam/adae.csv", stringsAsFactors = FALSE,
                 colClasses = "character")

adsl_required <- c("STUDYID", "USUBJID", "TRT01A", "SAFFL",
                   "TRTSDT", "TRTEDT")
adae_required <- c("STUDYID", "USUBJID", "ASEQ", "TRT01A", "SAFFL",
                   "AETERM", "AESER", "AESEV", "ASTDT", "TRTEMFL")

stopifnot(all(adsl_required %in% names(adsl)))
stopifnot(all(adae_required %in% names(adae)))
stopifnot(!anyNA(adsl[adsl_required]), !anyNA(adae[adae_required]))
stopifnot(!any(adsl[adsl_required] == ""))
stopifnot(!any(adae[setdiff(adae_required, "TRTEMFL")] == ""))
stopifnot(!anyDuplicated(adsl$USUBJID))
stopifnot(!anyDuplicated(adae[c("USUBJID", "ASEQ")]))
stopifnot(all(adsl$STUDYID == "ABC101"), all(adae$STUDYID == "ABC101"))
stopifnot(all(adae$USUBJID %in% adsl$USUBJID))
stopifnot(all(adsl$SAFFL %in% c("Y", "N")))
stopifnot(all(adae$TRTEMFL %in% c("Y", "")))
stopifnot(all(adae$AESER %in% c("Y", "N")))
stopifnot(all(adae$AESEV %in% c("MILD", "MODERATE", "SEVERE")))

adsl_index <- match(adae$USUBJID, adsl$USUBJID)
stopifnot(all(adae$TRT01A == adsl$TRT01A[adsl_index]))
stopifnot(all(adae$SAFFL == adsl$SAFFL[adsl_index]))

# Confirm the treatment-emergent rule before summarizing --------------------

ae_start <- as.Date(adae$ASTDT)
trt_start <- as.Date(adsl$TRTSDT[adsl_index])
trt_end <- as.Date(adsl$TRTEDT[adsl_index])
stopifnot(!anyNA(ae_start), !anyNA(trt_start), !anyNA(trt_end))

expected_flag <- ifelse(ae_start >= trt_start &
                          ae_start <= trt_end + 30L, "Y", "")
stopifnot(all(adae$TRTEMFL == expected_flag))

safety <- adsl[adsl$SAFFL == "Y", ]
teae <- adae[adae$TRTEMFL == "Y", ]
stopifnot(all(teae$SAFFL == "Y"))
stopifnot(all(teae$USUBJID %in% safety$USUBJID))

arm_order <- c("Placebo", "Drug 10 mg", "Drug 20 mg")
stopifnot(all(safety$TRT01A %in% arm_order))
stopifnot(all(arm_order %in% safety$TRT01A))

# Sort synthetic event terms by overall subject incidence -------------------

terms <- unique(teae$AETERM)
term_subject_count <- vapply(terms, function(term) {
  length(unique(teae$USUBJID[teae$AETERM == term]))
}, integer(1))
terms <- terms[order(-term_subject_count, terms)]

subject_count <- function(data) length(unique(data$USUBJID))

summary_for <- function(arm) {
  members <- if (arm == "Overall") safety else
    safety[safety$TRT01A == arm, ]
  events <- if (arm == "Overall") teae else
    teae[teae$TRT01A == arm, ]
  denominator <- nrow(members)
  stopifnot(denominator > 0L)

  incidence <- function(rows) {
    count <- subject_count(rows)
    sprintf("%d (%.1f%%)", count, 100 * count / denominator)
  }

  c(
    as.character(denominator),
    incidence(events),
    as.character(nrow(events)),
    incidence(events[events$AESER == "Y", ]),
    incidence(events[events$AESEV == "SEVERE", ]),
    "",
    vapply(terms, function(term) {
      incidence(events[events$AETERM == term, ])
    }, character(1))
  )
}

table <- data.frame(
  Characteristic = c(
    "Safety population", "Subjects with any TEAE", "TEAE records",
    "Subjects with serious TEAE", "Subjects with severe TEAE",
    "Event term", terms
  ),
  Statistic = c("N", "n (%)", "n", "n (%)", "n (%)", "",
                rep("n (%)", length(terms))),
  stringsAsFactors = FALSE
)

for (arm in c(arm_order, "Overall")) {
  table[[arm]] <- summary_for(arm)
}

stopifnot(sum(as.integer(unlist(table[1L, arm_order], use.names = FALSE))) ==
            nrow(safety))
stopifnot(sum(as.integer(unlist(table[3L, arm_order], use.names = FALSE))) ==
            nrow(teae))

dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)
write.csv(table, "output/tables/table_02_teae.csv",
          row.names = FALSE, na = "")

header <- paste0("| Characteristic | Statistic | ",
                 paste(c(arm_order, "Overall"), collapse = " | "), " |")
separator <- paste(rep("---", ncol(table)), collapse = " | ")
rows <- apply(table, 1L, function(values) {
  paste0("| ", paste(values, collapse = " | "), " |")
})

markdown <- c(
  "# Table 2. Treatment-emergent adverse events by actual treatment",
  "",
  "Study ABC101; safety population",
  "",
  header,
  paste0("| ", separator, " |"),
  rows,
  "",
  "TEAE: event onset from treatment start through 30 days after treatment end, inclusive.",
  "Incidence is the number of distinct subjects with at least one qualifying event;",
  "percentages use the safety population N in the same treatment column.",
  "Subjects may appear in more than one event-term row. Serious and severe rows may overlap.",
  "Event-term labels come from the synthetic source data and are not externally coded.",
  ""
)

writeLines(markdown, "output/tables/table_02_teae.md")
