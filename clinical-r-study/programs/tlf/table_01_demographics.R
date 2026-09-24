# ============================================================
# Program: table_01_demographics.R
#
# Purpose:
#   Summarize age and sex by planned treatment for the
#   intent-to-treat population in study ABC101.
#
# Input:
#   data/adam/adsl.csv
#
# Outputs:
#   output/tables/table_01_demographics.csv
#   output/tables/table_01_demographics.md
#
# Run from the project root.
# ============================================================

adsl <- read.csv(
  "data/adam/adsl.csv",
  stringsAsFactors = FALSE,
  colClasses = "character"
)

required <- c("STUDYID", "USUBJID", "AGE", "AGEU", "SEX",
              "TRT01P", "ITTFL")
stopifnot(all(required %in% names(adsl)))
stopifnot(!anyDuplicated(adsl$USUBJID))
stopifnot(all(adsl$STUDYID == "ABC101"))
stopifnot(all(adsl$ITTFL %in% c("Y", "N")))

itt <- adsl[adsl$ITTFL == "Y", ]
stopifnot(nrow(itt) > 0L)
stopifnot(!anyNA(itt[required]))
stopifnot(!any(itt[required] == ""))
stopifnot(all(itt$AGEU == "YEARS"))
stopifnot(all(itt$SEX %in% c("F", "M")))

arm_order <- c("Placebo", "Drug 10 mg", "Drug 20 mg")
stopifnot(all(itt$TRT01P %in% arm_order))
stopifnot(all(arm_order %in% itt$TRT01P))

age <- suppressWarnings(as.numeric(itt$AGE))
stopifnot(!anyNA(age), all(is.finite(age)), all(age >= 0))

summary_for <- function(rows) {
  group_age <- age[rows]
  group_sex <- itt$SEX[rows]
  n <- length(rows)
  stopifnot(n > 1L)

  count_percent <- function(code) {
    count <- sum(group_sex == code)
    sprintf("%d (%.1f%%)", count, 100 * count / n)
  }

  c(
    as.character(n),
    as.character(length(group_age)),
    sprintf("%.1f (%.1f)", mean(group_age), sd(group_age)),
    sprintf("%.1f", median(group_age)),
    sprintf("%.1f", min(group_age)),
    sprintf("%.1f", max(group_age)),
    count_percent("F"),
    count_percent("M")
  )
}

groups <- lapply(arm_order, function(arm) which(itt$TRT01P == arm))
names(groups) <- arm_order
groups$Overall <- seq_len(nrow(itt))

table <- data.frame(
  Characteristic = c("Subjects", "Age (years)", rep("", 4),
                     "Sex", ""),
  Statistic = c("N", "n", "Mean (SD)", "Median", "Min", "Max",
                "Female, n (%)", "Male, n (%)"),
  stringsAsFactors = FALSE
)

for (arm in names(groups)) {
  table[[arm]] <- summary_for(groups[[arm]])
}

stopifnot(sum(vapply(groups[arm_order], length, integer(1))) == nrow(itt))
stopifnot(all(vapply(groups, function(rows) {
  sum(itt$SEX[rows] == "F") + sum(itt$SEX[rows] == "M") == length(rows)
}, logical(1))))

dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)
write.csv(table, "output/tables/table_01_demographics.csv",
          row.names = FALSE, na = "")

header <- paste0(
  "| Characteristic | Statistic | ",
  paste(names(groups), collapse = " | "), " |"
)
separator <- paste(rep("---", ncol(table)), collapse = " | ")
rows <- apply(table, 1L, function(values) {
  paste0("| ", paste(values, collapse = " | "), " |")
})

markdown <- c(
  "# Table 1. Demographic characteristics by planned treatment",
  "",
  "Study ABC101; intent-to-treat population",
  "",
  header,
  paste0("| ", separator, " |"),
  rows,
  "",
  "Percentages use the number of subjects in each treatment column as the denominator.",
  "Age is summarized in years; SD is the sample standard deviation.",
  ""
)

writeLines(markdown, "output/tables/table_01_demographics.md")
