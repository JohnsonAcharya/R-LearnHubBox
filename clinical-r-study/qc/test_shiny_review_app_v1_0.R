# Read-only smoke and reactive checks for the synthetic ABC101 Shiny viewer.
# Run from the project root: Rscript qc/test_shiny_review_app_v1_0.R

if (!requireNamespace("shiny", quietly = TRUE)) stop("Package 'shiny' is required")

project_dir <- normalizePath(".", mustWork = TRUE)
app_dir <- file.path(project_dir, "app", "ABC101_synthetic_review_v1.0")
stopifnot(file.exists(file.path(app_dir, "app.R")))

inputs <- c(
  list.files(file.path(project_dir, "data/sdtm"), pattern = "\\.csv$", full.names = TRUE),
  list.files(file.path(project_dir, "data/adam"), pattern = "\\.csv$", full.names = TRUE),
  list.files(file.path(project_dir, "output/tables"), pattern = "\\.(csv|md)$", full.names = TRUE),
  list.files(file.path(project_dir, "output/listings"), pattern = "\\.(csv|md)$", full.names = TRUE),
  list.files(file.path(project_dir, "output/figures"), pattern = "\\.(csv|png)$", full.names = TRUE),
  list.files(file.path(project_dir, "output/qc"), pattern = "\\.md$", full.names = TRUE)
)
before <- unname(tools::md5sum(inputs))

old_dir <- getwd()
setwd(app_dir)
on.exit(setwd(old_dir), add = TRUE)
app <- source("app.R", local = TRUE)$value
stopifnot(inherits(app, "shiny.appobj"))
stopifnot(identical(names(source_data), names(source_files)))
stopifnot(nrow(source_data$ADSL) == 500L, nrow(listing_data) == 750L)
stopifnot(nrow(figure_data) == 12L, nrow(gaps) == 9L)
stopifnot(identical(vapply(table_data, nrow, integer(1)),
                    setNames(c(8L, 14L, 36L, 45L), names(table_files))))
stopifnot(identical(arm_counts$Subjects, c(157L, 166L, 177L)))
stopifnot(grepl("36 total; 36 passed", qc_text[["Final TLF QC"]], fixed = TRUE))
stopifnot(grepl("8/8 PASS", qc_text[["Conformance screening"]], fixed = TRUE))

shiny::testServer(server, {
  session$setInputs(dataset = "AE", subject_id = "ABC-001", data_page = 1,
                    listing_subject = "ABC-001", listing_page = 1,
                    table_name = "Table 2 - TEAEs", qc_report = "Conformance screening")
  stopifnot(all(dataset_rows()$USUBJID == "ABC-001"))
  stopifnot(nrow(dataset_rows()) == sum(source_data$AE$USUBJID == "ABC-001"))
  stopifnot(all(listing_rows()$USUBJID == "ABC-001"))
  stopifnot(nrow(page_rows(source_data$ADLB, 2)) == 25L)
  stopifnot(identical(page_rows(source_data$DM, 9999), source_data$DM[0, , drop = FALSE]))
  stopifnot(grepl("ABC101 synthetic conformance", output$qc_report_text, fixed = TRUE))
  stopifnot(grepl("table_02_teae.csv", output$table_source, fixed = TRUE))
  session$setInputs(dataset = "ADVS", subject_id = "ABC-999", listing_subject = "ABC-999")
  stopifnot(nrow(dataset_rows()) == 0L, nrow(listing_rows()) == 0L)
  session$setInputs(dataset = "ADVS", subject_id = "", data_page = 2)
  stopifnot(nrow(dataset_rows()) == 12500L)
  stopifnot(nrow(page_rows(dataset_rows(), input$data_page)) == 25L)
})

setwd(old_dir)
after <- unname(tools::md5sum(inputs))
stopifnot(identical(before, after))
cat("PASS: app loads; 9 datasets, 4 tables, listing, figure and 4 reports; reactive filters and paging; input files unchanged\n")
