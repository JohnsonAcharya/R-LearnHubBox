# ============================================================
# Program: generate_labs.R
#
# Purpose:
#   Generate synthetic source laboratory data for the
#   ABC101 clinical trial.
#
# Study:
#   ABC101
#
# Input:
#   data/raw/demographics.csv
#
# Output:
#   data/raw/labs.csv
#
# Notes:
#   Data are fully synthetic and created for educational
#   and portfolio purposes only.
# ============================================================


# Reproducibility ------------------------------------------------------------

set.seed(789)


# Read subject-level source data ---------------------------------------------

demographics <- read.csv(
  "data/raw/demographics.csv",
  stringsAsFactors = FALSE
)


# Study subjects -------------------------------------------------------------

subjects <- demographics$USUBJID


# Study visits ---------------------------------------------------------------

visits <- data.frame(
  VISIT = c(
    "Baseline",
    "Week 2",
    "Week 4",
    "Week 8",
    "Week 12"
  ),
  DAYS = c(
    0,
    14,
    28,
    56,
    84
  )
)


# Laboratory tests -----------------------------------------------------------

lab_tests <- data.frame(
  LBTESTCD = c(
    "ALT",
    "AST",
    "CREAT",
    "HGB"
  ),
  
  LBTEST = c(
    "Alanine Aminotransferase",
    "Aspartate Aminotransferase",
    "Creatinine",
    "Hemoglobin"
  ),
  
  LBORRESU = c(
    "U/L",
    "U/L",
    "mg/dL",
    "g/dL"
  ),
  
  stringsAsFactors = FALSE
)


# Create subject × visit × test combinations -------------------------------

labs <- expand.grid(
  USUBJID = subjects,
  VISIT = visits$VISIT,
  LBTESTCD = lab_tests$LBTESTCD,
  stringsAsFactors = FALSE
)


# Add visit day --------------------------------------------------------------

labs$VISITDAY <- visits$DAYS[
  match(labs$VISIT, visits$VISIT)
]


# Add laboratory test name ---------------------------------------------------

labs$LBTEST <- lab_tests$LBTEST[
  match(labs$LBTESTCD, lab_tests$LBTESTCD)
]


# Add laboratory units -------------------------------------------------------

labs$LBORRESU <- lab_tests$LBORRESU[
  match(labs$LBTESTCD, lab_tests$LBTESTCD)
]


# Generate laboratory results -----------------------------------------------

labs$LBORRES <- NA_real_


labs$LBORRES[labs$LBTESTCD == "ALT"] <- round(
  rnorm(
    sum(labs$LBTESTCD == "ALT"),
    mean = 30,
    sd = 10
  ),
  1
)


labs$LBORRES[labs$LBTESTCD == "AST"] <- round(
  rnorm(
    sum(labs$LBTESTCD == "AST"),
    mean = 28,
    sd = 8
  ),
  1
)


labs$LBORRES[labs$LBTESTCD == "CREAT"] <- round(
  rnorm(
    sum(labs$LBTESTCD == "CREAT"),
    mean = 0.9,
    sd = 0.2
  ),
  2
)


labs$LBORRES[labs$LBTESTCD == "HGB"] <- round(
  rnorm(
    sum(labs$LBTESTCD == "HGB"),
    mean = 14,
    sd = 1.5
  ),
  1
)


# Prevent unrealistic negative laboratory values ----------------------------

labs$LBORRES[labs$LBTESTCD == "ALT" & labs$LBORRES < 1] <- 1

labs$LBORRES[labs$LBTESTCD == "AST" & labs$LBORRES < 1] <- 1

labs$LBORRES[labs$LBTESTCD == "CREAT" & labs$LBORRES < 0.3] <- 0.3

labs$LBORRES[labs$LBTESTCD == "HGB" & labs$LBORRES < 5] <- 5


# Generate collection dates -------------------------------------------------

labs$LBDTC <- as.character(
  as.Date("2025-01-01") + labs$VISITDAY
)


# Generate sequence number within subject -----------------------------------

labs <- labs[
  order(
    labs$USUBJID,
    labs$VISITDAY,
    labs$LBTESTCD
  ),
]

labs$LBSEQ <- ave(
  labs$USUBJID,
  labs$USUBJID,
  FUN = seq_along
)


# Select final variables -----------------------------------------------------

labs <- labs[
  ,
  c(
    "USUBJID",
    "LBSEQ",
    "VISIT",
    "LBDTC",
    "LBTESTCD",
    "LBTEST",
    "LBORRES",
    "LBORRESU"
  )
]


# Write output ---------------------------------------------------------------

write.csv(
  labs,
  "data/raw/labs.csv",
  row.names = FALSE
)
