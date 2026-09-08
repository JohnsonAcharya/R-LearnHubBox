# ============================================================
# Program: generate_vital_signs.R
#
# Purpose:
#   Generate synthetic source vital signs data for the
#   ABC101 clinical trial.
#
# Study:
#   ABC101
#
# Input:
#   data/raw/demographics.csv
#
# Output:
#   data/raw/vital_signs.csv
#
# Notes:
#   Data are fully synthetic and created for educational
#   and portfolio purposes only.
# ============================================================


# Reproducibility ------------------------------------------------------------

set.seed(101112)


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
  ),
  stringsAsFactors = FALSE
)


# Vital sign tests -----------------------------------------------------------

vs_tests <- data.frame(
  VSTESTCD = c(
    "SYSBP",
    "DIABP",
    "PULSE",
    "TEMP",
    "WEIGHT"
  ),
  
  VSTEST = c(
    "Systolic Blood Pressure",
    "Diastolic Blood Pressure",
    "Pulse Rate",
    "Temperature",
    "Weight"
  ),
  
  VSORRESU = c(
    "mmHg",
    "mmHg",
    "bpm",
    "C",
    "kg"
  ),
  
  stringsAsFactors = FALSE
)


# Create subject x visit x test combinations -------------------------------

vital_signs <- expand.grid(
  USUBJID = subjects,
  VISIT = visits$VISIT,
  VSTESTCD = vs_tests$VSTESTCD,
  stringsAsFactors = FALSE
)


# Add visit day --------------------------------------------------------------

vital_signs$VISITDAY <- visits$DAYS[
  match(vital_signs$VISIT, visits$VISIT)
]


# Add test name --------------------------------------------------------------

vital_signs$VSTEST <- vs_tests$VSTEST[
  match(vital_signs$VSTESTCD, vs_tests$VSTESTCD)
]


# Add units ------------------------------------------------------------------

vital_signs$VSORRESU <- vs_tests$VSORRESU[
  match(vital_signs$VSTESTCD, vs_tests$VSTESTCD)
]


# Generate vital-sign results -----------------------------------------------

vital_signs$VSORRES <- NA_real_


# Systolic blood pressure
vital_signs$VSORRES[
  vital_signs$VSTESTCD == "SYSBP"
] <- round(
  rnorm(
    sum(vital_signs$VSTESTCD == "SYSBP"),
    mean = 125,
    sd = 12
  ),
  0
)


# Diastolic blood pressure
vital_signs$VSORRES[
  vital_signs$VSTESTCD == "DIABP"
] <- round(
  rnorm(
    sum(vital_signs$VSTESTCD == "DIABP"),
    mean = 78,
    sd = 8
  ),
  0
)


# Pulse rate
vital_signs$VSORRES[
  vital_signs$VSTESTCD == "PULSE"
] <- round(
  rnorm(
    sum(vital_signs$VSTESTCD == "PULSE"),
    mean = 72,
    sd = 10
  ),
  0
)


# Temperature
vital_signs$VSORRES[
  vital_signs$VSTESTCD == "TEMP"
] <- round(
  rnorm(
    sum(vital_signs$VSTESTCD == "TEMP"),
    mean = 36.8,
    sd = 0.3
  ),
  1
)


# Weight
vital_signs$VSORRES[
  vital_signs$VSTESTCD == "WEIGHT"
] <- round(
  rnorm(
    sum(vital_signs$VSTESTCD == "WEIGHT"),
    mean = 75,
    sd = 12
  ),
  1
)


# Prevent unrealistic values -----------------------------------------------

vital_signs$VSORRES[
  vital_signs$VSTESTCD == "SYSBP" &
    vital_signs$VSORRES < 70
] <- 70


vital_signs$VSORRES[
  vital_signs$VSTESTCD == "DIABP" &
    vital_signs$VSORRES < 40
] <- 40


vital_signs$VSORRES[
  vital_signs$VSTESTCD == "PULSE" &
    vital_signs$VSORRES < 40
] <- 40


vital_signs$VSORRES[
  vital_signs$VSTESTCD == "TEMP" &
    vital_signs$VSORRES < 35
] <- 35


vital_signs$VSORRES[
  vital_signs$VSTESTCD == "WEIGHT" &
    vital_signs$VSORRES < 35
] <- 35


# Generate collection dates -------------------------------------------------

vital_signs$VSDTC <- as.character(
  as.Date("2025-01-01") + vital_signs$VISITDAY
)


# Sort records ---------------------------------------------------------------

vital_signs <- vital_signs[
  order(
    vital_signs$USUBJID,
    vital_signs$VISITDAY,
    vital_signs$VSTESTCD
  ),
]


# Generate sequence number within subject -----------------------------------

vital_signs$VSSEQ <- ave(
  vital_signs$USUBJID,
  vital_signs$USUBJID,
  FUN = seq_along
)


# Select final variables -----------------------------------------------------

vital_signs <- vital_signs[
  ,
  c(
    "USUBJID",
    "VSSEQ",
    "VISIT",
    "VSDTC",
    "VSTESTCD",
    "VSTEST",
    "VSORRES",
    "VSORRESU"
  )
]


# Write output ---------------------------------------------------------------

write.csv(
  vital_signs,
  "data/raw/vital_signs.csv",
  row.names = FALSE
)
