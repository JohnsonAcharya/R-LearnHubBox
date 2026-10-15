# ABC101 synthetic review demonstration - Shiny app v1.0
# Read-only viewer for existing files. Launch from the project root with
# shiny::runApp('app/ABC101_synthetic_review_v1.0', launch.browser = TRUE).

if (!requireNamespace("shiny", quietly = TRUE)) {
  stop("Package 'shiny' is required. Install it with install.packages('shiny').")
}

project_root <- normalizePath(file.path(getwd(), "..", ".."), mustWork = TRUE)
stopifnot(file.exists(file.path(project_root, "data/adam/adsl.csv")))

source_files <- c(
  DM = "data/sdtm/dm.csv", AE = "data/sdtm/ae.csv",
  LB = "data/sdtm/lb.csv", VS = "data/sdtm/vs.csv",
  EX = "data/sdtm/ex.csv", ADSL = "data/adam/adsl.csv",
  ADAE = "data/adam/adae.csv", ADLB = "data/adam/adlb.csv",
  ADVS = "data/adam/advs.csv"
)
table_files <- c(
  "Table 1 - Demographics" = "output/tables/table_01_demographics.csv",
  "Table 2 - TEAEs" = "output/tables/table_02_teae.csv",
  "Table 3 - Laboratory" = "output/tables/table_03_laboratory.csv",
  "Table 4 - Vital signs" = "output/tables/table_04_vital_signs.csv"
)
qc_files <- c(
  "SDTM QC" = "output/qc/sdtm_validation_report.md",
  "ADaM QC" = "output/qc/adam_validation_report.md",
  "Final TLF QC" = "output/qc/tlf_validation_report.md",
  "Conformance screening" = "output/qc/ABC101_synthetic_conformance_findings_v1.0.md"
)
listing_file <- "output/listings/listing_01_subject_ae.csv"
figure_data_file <- "output/figures/figure_01_weight_change.csv"
figure_png_file <- "output/figures/figure_01_weight_change.png"

read_existing_csv <- function(relative) {
  path <- file.path(project_root, relative)
  if (!file.exists(path)) stop("Required existing CSV missing: ", relative)
  read.csv(path, check.names = FALSE, colClasses = "character",
           stringsAsFactors = FALSE, na.strings = "__NO_SYNTHETIC_MISSING_CODE__")
}

read_existing_text <- function(relative) {
  path <- file.path(project_root, relative)
  if (!file.exists(path)) stop("Required existing report missing: ", relative)
  paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

source_data <- lapply(source_files, read_existing_csv)
table_data <- lapply(table_files, read_existing_csv)
listing_data <- read_existing_csv(listing_file)
figure_data <- read_existing_csv(figure_data_file)
qc_text <- lapply(qc_files, read_existing_text)
if (!file.exists(file.path(project_root, figure_png_file))) {
  stop("Required existing Figure 1 PNG missing: ", figure_png_file)
}

stopifnot(!anyDuplicated(source_data$ADSL$USUBJID))
stopifnot(all(source_data$ADSL$ITTFL == "Y"), all(source_data$ADSL$SAFFL == "Y"))

arm_order <- c("Placebo", "Drug 10 mg", "Drug 20 mg")
arm_counts <- data.frame(
  `Actual treatment` = arm_order,
  Subjects = vapply(arm_order, function(arm) sum(source_data$ADSL$TRT01A == arm), integer(1)),
  check.names = FALSE
)
inventory <- data.frame(
  Layer = c(rep("SDTM", 5), rep("ADaM", 4)),
  Dataset = names(source_files),
  Records = vapply(source_data, nrow, integer(1)),
  File = unname(source_files),
  check.names = FALSE
)

extract_qc_line <- function(report, pattern) {
  hit <- grep(pattern, strsplit(report, "\n", fixed = TRUE)[[1]], value = TRUE)
  if (length(hit) != 1L) stop("Expected one QC summary line for: ", pattern)
  gsub("\\*", "", hit)
}

qc_status <- data.frame(
  Evidence = c("SDTM internal QC", "ADaM internal QC", "Final TLF internal QC",
               "Limited standards screening"),
  Result = c(
    paste(extract_qc_line(qc_text[["SDTM QC"]], "^\\*\\*Overall status:"),
          extract_qc_line(qc_text[["SDTM QC"]], "^\\*\\*Checks:")),
    paste(extract_qc_line(qc_text[["ADaM QC"]], "^\\*\\*Overall status:"),
          extract_qc_line(qc_text[["ADaM QC"]], "^\\*\\*Checks:")),
    paste(extract_qc_line(qc_text[["Final TLF QC"]], "^\\*\\*Overall status:"),
          extract_qc_line(qc_text[["Final TLF QC"]], "^\\*\\*Checks:")),
    sub("^\\| Limited executable screening \\| (.*?) \\|$", "\\1",
        grep("^\\| Limited executable screening \\|", strsplit(qc_text[["Conformance screening"]], "\n", fixed = TRUE)[[1]], value = TRUE))
  ),
  check.names = FALSE
)

gap_lines <- grep("^\\| G-[0-9]+ \\|",
                  strsplit(qc_text[["Conformance screening"]], "\n", fixed = TRUE)[[1]],
                  value = TRUE)
parse_gap <- function(line) {
  trimws(strsplit(sub("^\\|", "", sub("\\|$", "", line)), "\\|")[[1]])
}
gap_parts <- lapply(gap_lines, parse_gap)
stopifnot(length(gap_parts) > 0L, all(vapply(gap_parts, length, integer(1)) == 4L))
gaps <- as.data.frame(do.call(rbind, gap_parts), stringsAsFactors = FALSE)
names(gaps) <- c("ID", "Area", "Disposition", "Finding or reason")
gaps$Disposition <- gsub("\\*", "", gaps$Disposition)

page_rows <- function(data, page, size = 25L) {
  page <- suppressWarnings(as.integer(page))
  if (is.na(page) || page < 1L) page <- 1L
  start <- (page - 1L) * size + 1L
  if (start > nrow(data)) return(data[0, , drop = FALSE])
  data[seq.int(start, min(start + size - 1L, nrow(data))), , drop = FALSE]
}

ui <- shiny::fluidPage(
  shiny::tags$head(shiny::tags$style(shiny::HTML("\
    body { background: #f5f7fa; color: #182536; }\
    .container-fluid { max-width: 1500px; margin: auto; }\
    .synthetic-banner { background: #fff0d6; border-left: 5px solid #ae6800; padding: 14px 18px; margin: 16px 0; font-weight: 600; }\
    .review-card { background: white; border: 1px solid #d6e0e9; border-radius: 8px; padding: 18px; margin: 10px 0 18px; }\
    .scroll-table { overflow-x: auto; max-width: 100%; }\
    .scroll-table table { min-width: 760px; background: white; }\
    .figure-panel img { max-width: 100%; height: auto; }\
    .source-note { color: #516477; font-size: 0.95em; }\
    pre { white-space: pre-wrap; overflow-wrap: anywhere; background: white; border: 1px solid #d6e0e9; }\
  "))),
  shiny::titlePanel("ABC101 | Synthetic review demonstration"),
  shiny::div(class = "synthetic-banner",
             "SYNTHETIC DATA ONLY - generated subjects and results. Educational review tool; no clinical conclusions, real protocol/SAP, CDISC conformance claim, or submission-readiness claim."),
  shiny::tabsetPanel(
    id = "review_view",
    shiny::tabPanel("Overview",
      shiny::div(class = "review-card",
        shiny::h3("Study snapshot"),
        shiny::p("Illustrative type 2 diabetes study; 500 generated subjects; Placebo, Drug 10 mg, and Drug 20 mg. The assumed start date, 2025-01-01, is for standards selection only."),
        shiny::p("All generated subjects have ITTFL=Y and SAFFL=Y. Planned and actual treatment match in the current data. One exposure interval per subject runs from 2025-01-01 through 2025-03-26."),
        shiny::tableOutput("arm_counts"),
        shiny::p(class = "source-note", "Source: data/adam/adsl.csv; assumptions: docs/ABC101_synthetic_study_design_and_standards_v1.0.md")
      ),
      shiny::div(class = "review-card",
        shiny::h3("Data flow"),
        shiny::tags$pre("Generated raw CSVs -> SDTM DM/AE/LB/VS/EX -> ADaM ADSL/ADAE/ADLB/ADVS -> Tables 1-4, Listing 1, Figure 1"),
        shiny::p("SDTM, ADaM, display, and limited conformance checks read these existing files. Record-level analysis datasets carry source sequences for traceability."),
        shiny::div(class = "scroll-table", shiny::tableOutput("inventory")),
        shiny::p(class = "source-note", "Source mappings: docs/ABC101_synthetic_cSDRG_v1.0.md and docs/ABC101_synthetic_ADRG_v1.0.md")
      ),
      shiny::div(class = "review-card",
        shiny::h3("Rules and limits"),
        shiny::tags$ul(
          shiny::tags$li("TEAE: AE onset on/after TRTSDT through 30 days after TRTEDT, inclusive; non-TEAEs remain in ADAE."),
          shiny::tags$li("LB/VS baseline is the Baseline visit; predose timing is unverified because collection and dosing times are absent."),
          shiny::tags$li("The outputs are descriptive safety programming examples, with no clinical interpretation or inferential analysis.")
        )
      )
    ),
    shiny::tabPanel("Datasets",
      shiny::div(class = "review-card",
        shiny::h3("Read-only SDTM and ADaM records"),
        shiny::selectInput("dataset", "Dataset", choices = names(source_files), selected = "DM"),
        shiny::textInput("subject_id", "Exact USUBJID (optional)", value = "", placeholder = "ABC-001"),
        shiny::numericInput("data_page", "Page (25 rows)", value = 1, min = 1, step = 1),
        shiny::p(class = "source-note", shiny::textOutput("data_status", inline = TRUE)),
        shiny::div(class = "scroll-table", shiny::tableOutput("dataset_table")),
        shiny::p(class = "source-note", shiny::textOutput("data_source", inline = TRUE))
      )
    ),
    shiny::tabPanel("Tables, listing & figure",
      shiny::tabsetPanel(
        shiny::tabPanel("Tables 1-4",
          shiny::div(class = "review-card",
            shiny::selectInput("table_name", "Generated table", choices = names(table_files)),
            shiny::p(class = "source-note", "The table below reads the generated CSV without recalculation. See the matching Markdown in output/tables/."),
            shiny::div(class = "scroll-table", shiny::tableOutput("tlf_table")),
            shiny::p(class = "source-note", shiny::textOutput("table_source", inline = TRUE))
          )
        ),
        shiny::tabPanel("Listing 1",
          shiny::div(class = "review-card",
            shiny::p("Every ADAE event is present, including non-treatment-emergent events. Sort order is actual treatment, subject, start date, and ASEQ."),
            shiny::textInput("listing_subject", "Exact USUBJID (optional)", value = "", placeholder = "ABC-001"),
            shiny::numericInput("listing_page", "Page (25 rows)", value = 1, min = 1, step = 1),
            shiny::p(class = "source-note", shiny::textOutput("listing_status", inline = TRUE)),
            shiny::div(class = "scroll-table", shiny::tableOutput("listing_table")),
            shiny::p(class = "source-note", paste("Source:", listing_file))
          )
        ),
        shiny::tabPanel("Figure 1",
          shiny::div(class = "review-card figure-panel",
            shiny::p("Mean weight change in kg at Weeks 2, 4, 8, and 12 by actual treatment; safety population. The image and plotted summary are existing TLF outputs."),
            shiny::imageOutput("figure_image", width = "100%"),
            shiny::div(class = "scroll-table", shiny::tableOutput("figure_summary")),
            shiny::p(class = "source-note", paste("Sources:", figure_png_file, "and", figure_data_file))
          )
        )
      )
    ),
    shiny::tabPanel("QC & gaps",
      shiny::div(class = "review-card",
        shiny::h3("Internal QC and limited screening"),
        shiny::p("Internal QC tests project consistency. The separate eight-check standards screening is limited to properties exposed by current CSVs and Define-XML. Neither establishes CDISC or FDA conformance."),
        shiny::div(class = "scroll-table", shiny::tableOutput("qc_status")),
        shiny::h4("Open and untestable conformance areas"),
        shiny::div(class = "scroll-table", shiny::tableOutput("gaps")),
        shiny::p(class = "source-note", "The TLF visual check, when present, refers to the exact PNG hash in the final TLF QC report.")
      ),
      shiny::div(class = "review-card",
        shiny::h3("Full source report"),
        shiny::selectInput("qc_report", "Report", choices = names(qc_files)),
        shiny::p(class = "source-note", shiny::textOutput("qc_source", inline = TRUE)),
        shiny::verbatimTextOutput("qc_report_text")
      )
    )
  ),
  shiny::hr(),
  shiny::p(class = "source-note", "App v1.0. Existing files are loaded at startup; restart the app after regenerating outputs. The app performs no writes or TLF calculations.")
)

server <- function(input, output, session) {
  output$arm_counts <- shiny::renderTable(arm_counts, rownames = FALSE)
  output$inventory <- shiny::renderTable(inventory, rownames = FALSE)

  dataset_rows <- shiny::reactive({
    key <- input$dataset
    shiny::req(key %in% names(source_data))
    data <- source_data[[key]]
    subject <- trimws(input$subject_id)
    if (nzchar(subject)) data <- data[data$USUBJID == subject, , drop = FALSE]
    data
  })
  output$data_status <- shiny::renderText({
    sprintf("%s: %s matching records; page %s of %s.", input$dataset,
            format(nrow(dataset_rows()), big.mark = ","), input$data_page,
            max(1L, ceiling(nrow(dataset_rows()) / 25L)))
  })
  output$dataset_table <- shiny::renderTable(
    page_rows(dataset_rows(), input$data_page), rownames = FALSE)
  output$data_source <- shiny::renderText(paste("Source:", source_files[[input$dataset]]))

  output$tlf_table <- shiny::renderTable({
    shiny::req(input$table_name %in% names(table_data))
    table_data[[input$table_name]]
  }, rownames = FALSE)
  output$table_source <- shiny::renderText(paste("Source:", table_files[[input$table_name]]))

  listing_rows <- shiny::reactive({
    subject <- trimws(input$listing_subject)
    if (nzchar(subject)) listing_data[listing_data$USUBJID == subject, , drop = FALSE]
    else listing_data
  })
  output$listing_status <- shiny::renderText({
    sprintf("%s matching records; page %s of %s. Full listing: %s records.",
            format(nrow(listing_rows()), big.mark = ","), input$listing_page,
            max(1L, ceiling(nrow(listing_rows()) / 25L)),
            format(nrow(listing_data), big.mark = ","))
  })
  output$listing_table <- shiny::renderTable(
    page_rows(listing_rows(), input$listing_page), rownames = FALSE)

  output$figure_image <- shiny::renderImage({
    list(src = file.path(project_root, figure_png_file), contentType = "image/png",
         alt = "Synthetic ABC101 Figure 1: mean weight change and subject counts")
  }, deleteFile = FALSE)
  output$figure_summary <- shiny::renderTable(figure_data, rownames = FALSE)

  output$qc_status <- shiny::renderTable(qc_status, rownames = FALSE)
  output$gaps <- shiny::renderTable(gaps, rownames = FALSE)
  output$qc_report_text <- shiny::renderText({
    shiny::req(input$qc_report %in% names(qc_text))
    qc_text[[input$qc_report]]
  })
  output$qc_source <- shiny::renderText(paste("Source:", qc_files[[input$qc_report]]))
}

shiny::shinyApp(ui, server)
