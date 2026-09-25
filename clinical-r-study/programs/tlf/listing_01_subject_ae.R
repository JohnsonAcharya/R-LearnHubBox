# ============================================================
# Program: listing_01_subject_ae.R
#
# Purpose:
#   List every subject-level adverse event in ADaM ADAE,
#   including events that are not treatment-emergent.
#
# Input:
#   data/adam/adae.csv
#
# Outputs:
#   output/listings/listing_01_subject_ae.csv
#   output/listings/listing_01_subject_ae.md
#
# Run from the project root.
# ============================================================

adae <- read.csv(
  "data/adam/adae.csv",
  stringsAsFactors = FALSE,
  colClasses = "character"
)

required <- c(
  "STUDYID", "USUBJID", "TRT01A", "ASEQ", "AETERM",
  "ASTDT", "AENDT", "AESEV", "AESER", "AEOUT", "TRTEMFL"
)

stopifnot(all(required %in% names(adae)))
stopifnot(!anyNA(adae[required]))
stopifnot(!any(adae[setdiff(required, "TRTEMFL")] == ""))
stopifnot(all(adae$STUDYID == "ABC101"))
stopifnot(!anyDuplicated(adae[c("USUBJID", "ASEQ")]))
stopifnot(all(adae$TRTEMFL %in% c("Y", "")))
stopifnot(all(adae$AESER %in% c("Y", "N")))

arm_order <- c("Placebo", "Drug 10 mg", "Drug 20 mg")
stopifnot(all(adae$TRT01A %in% arm_order))

start_date <- as.Date(adae$ASTDT)
end_date <- as.Date(adae$AENDT)
sequence <- as.integer(adae$ASEQ)
stopifnot(!anyNA(start_date), !anyNA(end_date), !anyNA(sequence))
stopifnot(all(end_date >= start_date), all(sequence > 0L))

row_order <- order(
  match(adae$TRT01A, arm_order),
  adae$USUBJID,
  start_date,
  sequence
)
source <- adae[row_order, ]

listing <- data.frame(
  USUBJID = source$USUBJID,
  `Actual treatment` = source$TRT01A,
  `Event term` = source$AETERM,
  `Start date` = source$ASTDT,
  `End date` = source$AENDT,
  Severity = source$AESEV,
  Serious = source$AESER,
  Outcome = source$AEOUT,
  `Treatment-emergent` = ifelse(source$TRTEMFL == "Y", "Yes", "No"),
  ASEQ = sequence[row_order],
  check.names = FALSE,
  stringsAsFactors = FALSE
)

stopifnot(nrow(listing) == nrow(adae))
stopifnot(!anyDuplicated(listing[c("USUBJID", "ASEQ")]))
stopifnot(sum(listing[["Treatment-emergent"]] == "Yes") ==
            sum(adae$TRTEMFL == "Y"))

dir.create("output/listings", recursive = TRUE, showWarnings = FALSE)
write.csv(listing, "output/listings/listing_01_subject_ae.csv",
          row.names = FALSE, na = "")

escape_markdown <- function(value) gsub("\\|", "\\\\|", value)
header <- paste0("| ", paste(names(listing), collapse = " | "), " |")
separator <- paste0("| ", paste(rep("---", ncol(listing)), collapse = " | "), " |")
rows <- apply(listing, 1L, function(values) {
  paste0("| ", paste(escape_markdown(values), collapse = " | "), " |")
})

markdown <- c(
  "# Listing 1. Subject-level adverse events",
  "",
  "Study ABC101; all ADAE records",
  "",
  header,
  separator,
  rows,
  "",
  paste0("Records: ", nrow(listing), "; treatment-emergent: ",
         sum(listing[["Treatment-emergent"]] == "Yes"),
         "; not treatment-emergent: ",
         sum(listing[["Treatment-emergent"]] == "No"), "."),
  "Serious uses the ADAE Y/N indicator. Treatment-emergent status displays Yes for TRTEMFL=Y and No for a blank flag.",
  ""
)

writeLines(markdown, "output/listings/listing_01_subject_ae.md")
