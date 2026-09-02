# ============================================================
# Program: generate_adverse_events.R
#
# Purpose:
#   Generate synthetic source adverse event data for the
#   ABC101 clinical trial.
#
# Study:
#   ABC101
#
# Input:
#   data/raw/demographics.csv
#
# Output:
#   data/raw/adverse_events.csv
#
# Notes:
#   Data are fully synthetic and created for educational
#   and portfolio purposes only.
# ============================================================


# Reproducibility ------------------------------------------------------------

set.seed(456)


# Read subject-level source data ---------------------------------------------

demographics <- read.csv(
  "data/raw/demographics.csv",
  stringsAsFactors = FALSE
)


# Study subjects -------------------------------------------------------------

subjects <- demographics$USUBJID


# Number of adverse events ---------------------------------------------------

n_ae <- 750


# Generate subject IDs -------------------------------------------------------

ae_subjects <- sample(
  subjects,
  n_ae,
  replace = TRUE
)


# Generate AE sequence numbers ----------------------------------------------

ae_data <- data.frame(
  USUBJID = ae_subjects
)


ae_data$AESEQ <- ave(
  ae_data$USUBJID,
  ae_data$USUBJID,
  FUN = seq_along
)


# Generate adverse event terms ----------------------------------------------

ae_data$AETERM <- sample(
  c(
    "Headache",
    "Nausea",
    "Fatigue",
    "Dizziness",
    "Diarrhea",
    "Abdominal Pain",
    "Upper Respiratory Infection",
    "Injection Site Reaction"
  ),
  n_ae,
  replace = TRUE
)


# Generate AE start dates ---------------------------------------------------

ae_data$AESTDTC <- as.character(
  as.Date("2025-01-01") +
    sample(0:180, n_ae, replace = TRUE)
)


# Generate AE end dates -----------------------------------------------------

ae_start_date <- as.Date(ae_data$AESTDTC)

ae_data$AEENDTC <- as.character(
  ae_start_date +
    sample(1:14, n_ae, replace = TRUE)
)


# Generate AE severity ------------------------------------------------------

ae_data$AESEV <- sample(
  c("Mild", "Moderate", "Severe"),
  n_ae,
  replace = TRUE,
  prob = c(0.65, 0.30, 0.05)
)


# Generate serious AE indicator ---------------------------------------------

ae_data$AESER <- sample(
  c("Y", "N"),
  n_ae,
  replace = TRUE,
  prob = c(0.03, 0.97)
)


# Generate AE outcome -------------------------------------------------------

ae_data$AEOUT <- sample(
  c(
    "RECOVERED",
    "RECOVERING",
    "NOT RECOVERED"
  ),
  n_ae,
  replace = TRUE,
  prob = c(0.70, 0.20, 0.10)
)


# Write output --------------------------------------------------------------

write.csv(
  ae_data,
  "data/raw/adverse_events.csv",
  row.names = FALSE
)
