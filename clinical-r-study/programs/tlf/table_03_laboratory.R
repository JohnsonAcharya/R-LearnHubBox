# ============================================================
# Program: table_03_laboratory.R
#
# Purpose:
#   Summarize observed laboratory values and change from
#   baseline by actual treatment and visit for the ABC101
#   safety population.
#
# Input:
#   data/adam/adlb.csv
#
# Outputs:
#   output/tables/table_03_laboratory.csv
#   output/tables/table_03_laboratory.md
#
# Run from the project root.
# ============================================================

adlb <- read.csv(
  "data/adam/adlb.csv",
  stringsAsFactors = FALSE,
  colClasses = "character"
)

required <- c(
  "STUDYID", "USUBJID", "ASEQ", "SAFFL", "TRT01A",
  "PARAMCD", "PARAM", "AVALU", "AVISIT", "AVISITN",
  "ABLFL", "AVAL", "CHG"
)

stopifnot(all(required %in% names(adlb)))
stopifnot(!anyDuplicated(adlb[c("USUBJID", "ASEQ")]))
stopifnot(all(adlb$STUDYID == "ABC101"))
stopifnot(all(adlb$SAFFL %in% c("Y", "N")))

safety <- adlb[adlb$SAFFL == "Y", ]
stopifnot(nrow(safety) > 0L)
stopifnot(!anyNA(safety[setdiff(required, "CHG")]))
stopifnot(!any(safety[setdiff(required, c("CHG", "ABLFL"))] == ""))
stopifnot(!anyDuplicated(safety[c("USUBJID", "PARAMCD", "AVISITN")]))

arm_order <- c("Placebo", "Drug 10 mg", "Drug 20 mg")
param_order <- c("ALT", "AST", "CREAT", "HGB")
visit_order <- 1:5

stopifnot(all(safety$TRT01A %in% arm_order))
stopifnot(all(safety$PARAMCD %in% param_order))
stopifnot(all(as.integer(safety$AVISITN) %in% visit_order))
stopifnot(all(safety$ABLFL %in% c("Y", "")))
stopifnot(all(!is.na(as.numeric(safety$AVAL))))
stopifnot(all(is.finite(as.numeric(safety$AVAL))))
stopifnot(all(safety$CHG[safety$ABLFL == "Y"] == ""))
stopifnot(all(safety$CHG[safety$ABLFL != "Y"] != ""))
stopifnot(all(!is.na(as.numeric(safety$CHG[safety$ABLFL != "Y"]))))

parameter_info <- lapply(param_order, function(parameter) {
  records <- safety[safety$PARAMCD == parameter, ]
  labels <- unique(records$PARAM)
  units <- unique(records$AVALU)
  stopifnot(length(labels) == 1L, length(units) == 1L)
  list(label = labels, unit = units)
})
names(parameter_info) <- param_order

visit_info <- vapply(visit_order, function(number) {
  labels <- unique(safety$AVISIT[safety$AVISITN == as.character(number)])
  stopifnot(length(labels) == 1L)
  labels
}, character(1))

summarize_group <- function(records, measure) {
  values <- as.numeric(records[[measure]])
  stopifnot(!anyNA(values), all(is.finite(values)))
  n <- length(unique(records$USUBJID))
  stopifnot(n == length(values))

  if (n == 0L) return(c("0", ""))
  if (n == 1L) return(c("1", sprintf("%.2f (NA)", values)))
  c(as.character(n), sprintf("%.2f (%.2f)", mean(values), sd(values)))
}

rows <- list()
for (parameter in param_order) {
  for (visit_number in visit_order) {
    records <- safety[
      safety$PARAMCD == parameter &
        safety$AVISITN == as.character(visit_number),
    ]

    measures <- if (visit_number == 1L) "AVAL" else c("AVAL", "CHG")
    for (measure in measures) {
      stopifnot(nrow(records) > 0L)
      if (measure == "CHG") stopifnot(all(records$ABLFL != "Y"))
      if (measure == "AVAL" && visit_number == 1L) {
        stopifnot(all(records$ABLFL == "Y"))
      }

      row <- data.frame(
        PARAMCD = parameter,
        `Lab test` = parameter_info[[parameter]]$label,
        Unit = parameter_info[[parameter]]$unit,
        Visit = visit_info[[visit_number]],
        `Visit number` = visit_number,
        Measure = if (measure == "AVAL") "Observed value" else
          "Change from baseline",
        check.names = FALSE,
        stringsAsFactors = FALSE
      )

      for (arm in c(arm_order, "Overall")) {
        group <- if (arm == "Overall") records else
          records[records$TRT01A == arm, ]
        result <- summarize_group(group, measure)
        row[[paste0(arm, " n")]] <- result[1L]
        row[[paste0(arm, " Mean (SD)")]] <- result[2L]
      }
      rows[[length(rows) + 1L]] <- row
    }
  }
}

table <- do.call(rbind, rows)
row.names(table) <- NULL
stopifnot(nrow(table) == length(param_order) * (1L + 4L * 2L))

dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)
write.csv(table, "output/tables/table_03_laboratory.csv",
          row.names = FALSE, na = "")

header <- paste0("| ", paste(names(table), collapse = " | "), " |")
separator <- paste0("| ", paste(rep("---", ncol(table)), collapse = " | "), " |")
markdown_rows <- apply(table, 1L, function(values) {
  paste0("| ", paste(values, collapse = " | "), " |")
})

markdown <- c(
  "# Table 3. Laboratory results and change from baseline by actual treatment",
  "",
  "Study ABC101; safety population",
  "",
  header,
  separator,
  markdown_rows,
  "",
  "n is the number of distinct subjects with a nonmissing result for the measure.",
  "Mean (SD) uses the sample standard deviation and is displayed to two decimal places.",
  "Change from baseline is shown only at postbaseline visits.",
  ""
)

writeLines(markdown, "output/tables/table_03_laboratory.md")
