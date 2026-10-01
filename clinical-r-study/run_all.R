# Rebuild the synthetic study and run every automated QC gate.
# Usage: Rscript run_all.R (the script may be called from any directory).

main <- function() {
  file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(file_arg) != 1L) {
    stop("Run this file with Rscript, for example: Rscript run_all.R")
  }
  project_root <- dirname(normalizePath(sub("^--file=", "", file_arg), mustWork = TRUE))
  previous_dir <- getwd()
  on.exit(setwd(previous_dir), add = TRUE)
  setwd(project_root)

  python <- Sys.which("python")
  if (!nzchar(python)) python <- Sys.which("python3")
  if (!nzchar(python)) stop("Python 3 is required on PATH for qc/validate_tlf.py")

  for (directory in c("data/raw", "data/sdtm", "data/adam",
                      "output/tables", "output/listings", "output/figures", "output/qc")) {
    dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  }

  stages <- list(
    "Raw data" = c(
      "R/data_generation/generate_raw_demographics.R",
      "R/data_generation/generate_adverse_events.R",
      "R/data_generation/generate_labs.R",
      "R/data_generation/generate_vital_signs.R",
      "R/data_generation/generate_exposure.R"
    ),
    "SDTM" = c(
      "programs/sdtm/dm.R", "programs/sdtm/ae.R", "programs/sdtm/lb.R",
      "programs/sdtm/vs.R", "programs/sdtm/ex.R", "qc/validate_sdtm.R"
    ),
    "ADaM" = c(
      "programs/adam/adsl.R", "programs/adam/adae.R",
      "programs/adam/adlb.R", "programs/adam/advs.R", "qc/validate_adam.R"
    ),
    "Tables, listing, and figure" = c(
      "programs/tlf/table_01_demographics.R",
      "programs/tlf/table_02_teae.R",
      "programs/tlf/table_03_laboratory.R",
      "programs/tlf/table_04_vital_signs.R",
      "programs/tlf/listing_01_subject_ae.R",
      "programs/tlf/figure_01_weight_change.R"
    )
  )

  for (stage in names(stages)) {
    cat("\n=== ", stage, " ===\n", sep = "")
    for (script in stages[[stage]]) {
      cat("Running ", script, "\n", sep = "")
      sys.source(script, envir = new.env(parent = globalenv()))
    }
  }

  cat("\n=== Independent final TLF QC ===\n")
  status <- system2(python, file.path("qc", "validate_tlf.py"))
  if (!identical(status, 0L)) stop("Independent final TLF QC failed (exit status ", status, ")")
  cat("\nFull workflow completed; all automated QC gates passed.\n")
}

main()
