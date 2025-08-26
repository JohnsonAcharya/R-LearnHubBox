library(shiny)
library(dplyr)
library(tidyr)
library(readxl)
library(openxlsx)

ui <- navbarPage(
  "Survey App",
  
  # ---- Tab 1: Table Report ----
  tabPanel("Table Report",
           fluidPage(
             titlePanel("Survey Frequency & Percentage Tables"),
             
             sidebarLayout(
               sidebarPanel(
                 fileInput("file", "Upload Survey Data (CSV/Excel)", accept = c(".csv", ".xlsx")),
                 tags$details(
                   tags$summary("Select Questions", style = "cursor: pointer; font-weight: bold;"),
                   actionButton("select_all", "Select All"),
                   actionButton("deselect_all", "Deselect All"),
                   br(), br(),
                   uiOutput("question_selector")
                 ),
                 downloadButton("download_excel", "Download Selected Tables (Excel)")
               ),
               mainPanel(
                 uiOutput("tabs_output")
               )
             ),
             tags$style(HTML("
        details[open] > summary { list-style-type: none; }
        details > summary::-webkit-details-marker { display: none; }
        details > summary::before { content: '▶ '; font-size: 14px; }
        details[open] > summary::before { content: '▼ '; font-size: 14px; }
        table { width: auto !important; }
        th, td { text-align: center; padding: 4px 10px; }
      "))
           )
  ),
  
  # ---- Tab 2: About ----
  tabPanel("About",
           fluidPage(
             titlePanel("About This App"),
             h4("Purpose"),
             p("This Shiny app helps analyze survey data by generating frequency, percentage, 
        and summary tables (including Top Box, Top 2 Box, and Mean Scores for rating questions)."),
        
        h4("How to Use"),
        tags$ol(
          tags$li("Upload your survey dataset (CSV or Excel)."),
          tags$li("Select the questions you want to analyze."),
          tags$li("View the generated tables in the Table Report tab."),
          tags$li("Download the results as an Excel file.")
        ),
        
        h4("Notes"),
        p("• Only structured survey data with consistent question naming (e.g., Q2a_1, Q2a_2) is supported."),
        p("• Grand Total rows are bolded in all tables for clarity."),
        
        h4("Developer"),
        p("Built in R and Shiny."),
        p("Contact: your_email@example.com")
           )
  )
)

# ------------------ SERVER -------------------
server <- function(input, output, session) {
  
  survey_data <- reactive({
    req(input$file)
    ext <- tools::file_ext(input$file$name)
    if (ext == "csv") {
      read.csv(input$file$datapath, stringsAsFactors = FALSE)
    } else if (ext == "xlsx") {
      readxl::read_excel(input$file$datapath)
    } else {
      validate("Please upload a CSV or Excel file.")
    }
  })
  
  summarize_question <- function(df, q) {
    sub_cols <- grep(paste0("^", q, "_[0-9]+$"), names(df), value = TRUE)
    
    # Example for Q6 handling
    if (q == "Q6" && length(sub_cols) > 0) {
      freq_list <- list()
      perc_list <- list()
      summary_list <- list()
      valid_cols <- sub_cols[sapply(df[sub_cols], function(x) !all(is.na(x)))]
      if(length(valid_cols) == 0) 
        return(list(freq=data.frame(), perc=data.frame(), summary=data.frame(), perc_numeric=data.frame(), summary_numeric=data.frame()))
      
      for (col in valid_cols) {
        vals <- df[[col]]
        dist <- table(factor(vals, levels = 1:5))
        total <- sum(dist)
        freq_list[[col]] <- c(as.numeric(dist), total)
        perc_list[[col]] <- c(round(100 * dist / total,0), 100)
        top_box <- round(100 * sum(vals == 5, na.rm = TRUE) / total, 0)
        top2_box <- round(100 * sum(vals %in% c(4,5), na.rm = TRUE) / total, 0)
        mean_score <- round(mean(vals, na.rm = TRUE), 2)
        summary_list[[col]] <- c(top_box, top2_box, mean_score)
      }
      
      freq_df <- as.data.frame(do.call(rbind, freq_list))
      names(freq_df) <- c("1","2","3","4","5","Grand Total")
      freq_df <- cbind(RowLabels = names(freq_list), freq_df)
      freq_total <- c("Grand Total", colSums(freq_df[,-1]))
      freq_df <- rbind(freq_df, freq_total)
      
      perc_df <- as.data.frame(do.call(rbind, perc_list))
      names(perc_df) <- c("1","2","3","4","5","Grand Total")
      perc_df <- cbind(RowLabels = names(perc_list), perc_df)
      perc_total_values <- round(colMeans(perc_df[,-1][,1:5]),0)
      perc_total <- c("Grand Total", perc_total_values, 100)
      perc_df <- rbind(perc_df, perc_total)
      perc_display <- perc_df
      for(j in 2:ncol(perc_display)) perc_display[[j]] <- paste0(perc_display[[j]], "%")
      
      summary_df <- as.data.frame(do.call(rbind, summary_list))
      names(summary_df) <- c("Top Box (5)","Top 2 Box (4+5)","Mean Score")
      summary_df <- cbind(RowLabels = names(summary_list), summary_df)
      summary_df$`Top Box (5)` <- paste0(summary_df$`Top Box (5)`, "%")
      summary_df$`Top 2 Box (4+5)` <- paste0(summary_df$`Top 2 Box (4+5)`, "%")
      summary_total <- c(
        "Grand Total",
        paste0(round(mean(as.numeric(sub("%","",summary_df$`Top Box (5)`))),0), "%"),
        paste0(round(mean(as.numeric(sub("%","",summary_df$`Top 2 Box (4+5)`))),0), "%"),
        round(mean(as.numeric(summary_df$`Mean Score`)),2)
      )
      summary_df <- rbind(summary_df, summary_total)
      
      return(list(freq = freq_df, perc = perc_display, summary = summary_df,
                  perc_numeric = perc_df, summary_numeric = summary_df))
    }
    
    # Yes/No example for binary questions
    if(grepl("^Q13a_|^Q15_", q)) {
      if(!q %in% names(df)) 
        return(list(data.frame()))
      
      vals <- df[[q]]
      dist <- table(factor(vals, levels = c(1,2)))
      dist_df <- data.frame(Response = c("Yes","No"),
                            Frequency = as.numeric(dist))
      dist_df$Percentage <- round(100 * dist_df$Frequency / sum(dist_df$Frequency), 1)
      dist_df$Percentage <- paste0(dist_df$Percentage, "%")
      dist_df <- rbind(dist_df,
                       data.frame(Response = "Grand Total",
                                  Frequency = sum(dist_df$Frequency),
                                  Percentage = "100%"))
      return(list(dist_df))
    }
    
    # Multi-column question summary
    if(length(sub_cols) > 0){
      valid_cols <- sub_cols[sapply(df[sub_cols], function(x) !all(is.na(x)))]
      if(length(valid_cols)==0) return(list(data.frame()))
      out <- df %>%
        select(all_of(valid_cols)) %>%
        summarise(across(everything(), ~ sum(!is.na(.)), .names = "{.col}")) %>%
        pivot_longer(everything(), names_to = "Response", values_to = "Frequency") %>%
        mutate(Percentage = round(100 * Frequency / nrow(df), 1))
      out$Percentage <- paste0(out$Percentage, "%")
      out <- rbind(out, data.frame(Response = "Grand Total",
                                   Frequency = sum(out$Frequency),
                                   Percentage = "100%"))
      return(list(out))
    }
    
    # Standard single-column summary
    if(!q %in% names(df)) return(list(data.frame()))
    out <- df %>%
      filter(!is.na(.data[[q]])) %>%
      count(.data[[q]], name = "Frequency") %>%
      mutate(Percentage = round(100 * Frequency / sum(Frequency), 1))
    names(out)[1] <- "Response"
    out$Percentage <- paste0(out$Percentage, "%")
    out <- rbind(out,
                 data.frame(Response = "Grand Total",
                            Frequency = sum(out$Frequency),
                            Percentage = "100%"))
    return(list(out))
  }
  
  output$question_selector <- renderUI({
    req(survey_data())
    df <- survey_data()
    base_names <- unique(gsub("_[0-9]+$", "", names(df)))
    checkboxGroupInput("selected_questions", NULL,
                       choices = base_names,
                       selected = base_names)
  })
  
  observeEvent(input$select_all, {
    df <- survey_data()
    base_names <- unique(gsub("_[0-9]+$", "", names(df)))
    updateCheckboxGroupInput(session, "selected_questions", selected = base_names)
  })
  
  observeEvent(input$deselect_all, {
    updateCheckboxGroupInput(session, "selected_questions", selected = character(0))
  })
  
  # ------ Table rendering with "Grand Total" bolded -------
  output_table_with_bold_gt <- function(df) {
    if (is.null(df) || nrow(df) == 0 || ncol(df) == 0) {
      return(HTML("<p><em>No data available for this question.</em></p>"))
    }
    
    table_rows <- apply(df, 1, function(row) {
      if (tolower(row[1]) %in% c("grand total", "total")) {
        paste0("<tr>",
               paste0(sprintf("<td><strong>%s</strong></td>", row), collapse = ""),
               "</tr>")
      } else {
        paste0("<tr>",
               paste0(sprintf("<td>%s</td>", row), collapse = ""),
               "</tr>")
      }
    })
    
    header <- paste0("<tr>", paste0(sprintf("<th>%s</th>", colnames(df)), collapse = ""), "</tr>")
    HTML(paste0("<table class='table table-striped'>",
                header,
                paste(table_rows, collapse = ""),
                "</table>"))
  }
  
  output$tabs_output <- renderUI({
    req(input$selected_questions)
    tabs <- lapply(input$selected_questions, function(q) {
      tabPanel(q, uiOutput(paste0("ui_", q)))
    })
    if(length(tabs) > 0) do.call(tabsetPanel, tabs)
  })
  
  observe({
    req(survey_data(), input$selected_questions)
    df <- survey_data()
    lapply(input$selected_questions, function(q) {
      res <- summarize_question(df, q)
      output[[paste0("ui_", q)]] <- renderUI({
        if(q=="Q6"){
          tagList(
            h4("Frequency"),
            uiOutput(paste0("custom_table_q6_freq")),
            h4("Percentage"),
            uiOutput(paste0("custom_table_q6_perc")),
            h4("Summary"),
            uiOutput(paste0("custom_table_q6_summary"))
          )
        } else {
          uiOutput(paste0("custom_table_", q))
        }
      })
      if(q=="Q6"){
        output[[paste0("custom_table_q6_freq")]] <- renderUI({ output_table_with_bold_gt(res$freq) })
        output[[paste0("custom_table_q6_perc")]] <- renderUI({ output_table_with_bold_gt(res$perc) })
        output[[paste0("custom_table_q6_summary")]] <- renderUI({ output_table_with_bold_gt(res$summary) })
      } else {
        output[[paste0("custom_table_", q)]] <- renderUI({ output_table_with_bold_gt(res[[1]]) })
      }
    })
  })
  
  # ---- Excel Export with Grand Total Highlight ----
  output$download_excel <- downloadHandler(
    filename = function(){ "survey_summary.xlsx" },
    content = function(file){
      df <- survey_data()
      wb <- createWorkbook()
      highlight_style <- createStyle(textDecoration = "Bold")
      for(q in input$selected_questions){
        res <- summarize_question(df, q)
        addWorksheet(wb, q)
        if(q=="Q6"){
          writeData(wb, q, "Frequency", startRow = 1)
          writeData(wb, q, res$freq, startRow = 2)
          addStyle(wb, q, highlight_style, rows=nrow(res$freq)+1, cols=1:ncol(res$freq), gridExpand=TRUE)
          writeData(wb, q, "Percentage", startRow=nrow(res$freq)+4)
          perc_numeric <- res$perc_numeric
          for(j in 2:ncol(perc_numeric)) perc_numeric[[j]] <- as.numeric(gsub("%","",perc_numeric[[j]]))/100
          writeData(wb, q, perc_numeric, startRow=nrow(res$freq)+5)
          addStyle(wb, q, highlight_style, rows=nrow(res$freq)+5+nrow(perc_numeric)-1, cols=1:ncol(perc_numeric), gridExpand=TRUE)
          addStyle(wb, q, createStyle(numFmt="0%"), 
                   rows=(nrow(res$freq)+5):(nrow(res$freq)+5+nrow(perc_numeric)-1),
                   cols=2:ncol(perc_numeric), gridExpand=TRUE)
          writeData(wb, q, "Summary", startRow=nrow(res$freq)+nrow(perc_numeric)+7)
          summary_numeric <- res$summary_numeric
          summary_numeric$`Top Box (5)` <- as.numeric(gsub("%","",summary_numeric$`Top Box (5)`))/100
          summary_numeric$`Top 2 Box (4+5)` <- as.numeric(gsub("%","",summary_numeric$`Top 2 Box (4+5)`))/100
          summary_numeric$`Mean Score` <- as.numeric(summary_numeric$`Mean Score`)
          writeData(wb, q, summary_numeric, startRow=nrow(res$freq)+nrow(perc_numeric)+8)
          addStyle(wb, q, highlight_style, rows=nrow(res$freq)+nrow(perc_numeric)+8+nrow(summary_numeric)-1, cols=1:ncol(summary_numeric), gridExpand=TRUE)
          addStyle(wb, q, createStyle(numFmt="0%"), 
                   rows=(nrow(res$freq)+nrow(perc_numeric)+8):(nrow(res$freq)+nrow(perc_numeric)+7+nrow(summary_numeric)),
                   cols=2:3, gridExpand=TRUE)
        } else {
          writeData(wb, q, res[[1]])
          if("Percentage" %in% names(res[[1]])){
            perc_col <- which(names(res[[1]])=="Percentage")
            addStyle(wb, q, createStyle(numFmt="0%"), rows=2:(nrow(res[[1]])+1), cols=perc_col, gridExpand=TRUE)
          }
          addStyle(wb, q, highlight_style, rows=nrow(res[[1]])+1, cols=1:ncol(res[[1]]), gridExpand=TRUE)
        }
      }
      saveWorkbook(wb, file, overwrite=TRUE)
    }
  )
}

shinyApp(ui, server)
