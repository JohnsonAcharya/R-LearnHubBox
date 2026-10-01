# Purpose:
#   Generate synthetic raw demographic data for the ABC-101
#   clinical trial portfolio project.
#
# Source:
#   Synthetic data generated for programming demonstration.
#
# Output:
#   data/raw/demographics.csv - Subject-level demographic dataset.
#
# Notes:
#   No real patient-level data are used.
#   Random seed is fixed to ensure reproducibility.

set.seed(123)

n <- 500

raw_dm <- data.frame(
  USUBJID = sprintf("ABC-%03d", 1:n),
  SEX     = sample(c("M", "F"), n, replace = TRUE),
  AGE     = sample(18:80, n, replace = TRUE),
  TRT01P  = sample(
    c("Placebo", "Drug 10 mg", "Drug 20 mg"),
    n,
    replace = TRUE
  )
)
write.csv(
  raw_dm,
  "data/raw/demographics.csv",
  row.names = FALSE
)
