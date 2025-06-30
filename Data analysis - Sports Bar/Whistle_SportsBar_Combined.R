# === Load Packages ===
library(tidyverse)
library(readxl)
library(janitor)
library(openxlsx)

# === Load & Clean Data ===
data <- read_excel("PQ - Whistle (Bengaluru) V1.xlsx") %>% clean_names()
wb <- createWorkbook()
sheet_names <- c()

# === Add Sheet Helper ===
add_analysis_sheet <- function(sheet_name, df){
  addWorksheet(wb, sheet_name)
  writeData(wb, sheet_name, df)
  sheet_names <<- c(sheet_names, sheet_name)
}

# === Multi-select Summary Function ===
multi_select_summary_by_age <- function(df, prefix, group = rq2b) {
  vars <- names(df)[startsWith(names(df), prefix)]
  long_df <- df %>%
    select(all_of(vars), {{ group }}) %>%
    pivot_longer(-{{ group }}, names_to = "Option", values_to = "Selected") %>%
    filter(!is.na({{ group }})) %>%
    mutate(selected_flag = ifelse(!is.na(Selected), 1, 0))
  
  by_age <- long_df %>%
    group_by(Option, {{ group }}) %>%
    summarise(
      Count = sum(selected_flag),
      Percent = paste0(round(100 * mean(selected_flag), 1), "%"),
      .groups = "drop"
    ) %>%
    pivot_wider(names_from = {{ group }}, values_from = c(Count, Percent), names_sep = "_Age")
  
  overall <- long_df %>%
    group_by(Option) %>%
    summarise(
      Count_Overall = sum(selected_flag),
      Percent_Overall = paste0(round(100 * mean(selected_flag), 1), "%"),
      .groups = "drop"
    )
  
  left_join(by_age, overall, by = "Option")
}

# === Matrix Summary Function ===
matrix_summary_by_age <- function(df, prefix, group = rq2b) {
  vars <- names(df)[startsWith(names(df), prefix)]
  long_df <- df %>%
    select(all_of(vars), {{ group }}) %>%
    pivot_longer(-{{ group }}, names_to = "Question", values_to = "Response") %>%
    filter(!is.na(Response), !is.na({{ group }}))
  
  by_age <- long_df %>%
    group_by(Question, {{ group }}) %>%
    summarise(
      Count = n(),
      Mean = round(mean(Response, na.rm = TRUE), 2),
      Top2Box = round(mean(Response >= 4, na.rm = TRUE) * 100, 1),
      TopBox = round(mean(Response == 5, na.rm = TRUE) * 100, 1),
      .groups = "drop"
    ) %>%
    pivot_wider(names_from = {{ group }}, values_from = c(Mean, Top2Box, TopBox), names_sep = "_Age")
  
  overall <- long_df %>%
    group_by(Question) %>%
    summarise(
      Mean_Overall = round(mean(Response, na.rm = TRUE), 2),
      Top2Box_Overall = round(mean(Response >= 4, na.rm = TRUE) * 100, 1),
      TopBox_Overall = round(mean(Response == 5, na.rm = TRUE) * 100, 1),
      .groups = "drop"
    )
  
  left_join(by_age, overall, by = "Question")
}

# === Single-select Summary Function ===
freq_percent_overall_and_by_age <- function(df, var, group = rq2b) {
  by_age <- df %>%
    filter(!is.na({{ var }}), !is.na({{ group }})) %>%
    count({{ group }}, {{ var }}) %>%
    group_by({{ group }}) %>%
    mutate(Percent = paste0(round(100 * n / sum(n), 1), "%")) %>%
    ungroup() %>%
    pivot_wider(
      names_from = {{ group }},
      values_from = c(n, Percent),
      names_sep = "_Age"
    ) %>%
    rename(Value = {{ var }})
  
  overall <- df %>%
    filter(!is.na({{ var }})) %>%
    count({{ var }}) %>%
    mutate(
      Percent_Overall = paste0(round(100 * n / sum(n), 1), "%")
    ) %>%
    rename(Value = {{ var }}, Freq_Overall = n)
  
  left_join(by_age, overall, by = "Value") %>%
    select(everything(), Freq_Overall, Percent_Overall)
}

# === Combined Summary Table ===
combined_summary <- tibble(
  Section = character(),
  Question = character(),
  Item = character(),
  Metric = character(),
  `19-25` = character(),
  `26-35` = character(),
  Overall = character()
)

# === Add to Combined Summary ===
add_to_combined <- function(df, section, metric_type = "Percent", is_matrix = FALSE) {
  processed <- if (!is_matrix) {
    df %>%
      mutate(
        Section = section,
        Question = gsub("_.*", "", Option),
        Item = Option,
        Metric = metric_type,
        `19-25` = as.character(if (any(grepl("Percent.*19", names(.)))) pull(., grep("Percent.*19", names(.), value = TRUE)) else NA),
        `26-35` = as.character(if (any(grepl("Percent.*26", names(.)))) pull(., grep("Percent.*26", names(.), value = TRUE)) else NA),
        Overall  = as.character(if ("Percent_Overall" %in% names(.)) Percent_Overall else NA)
      ) %>%
      select(Section, Question, Item, Metric, `19-25`, `26-35`, Overall)
  } else {
    df %>%
      rename(Item = Question) %>%
      mutate(
        Section = section,
        Question = section,
        Metric = "Mean Score",
        `19-25` = as.character(if (any(grepl("Mean.*19", names(.)))) pull(., grep("Mean.*19", names(.), value = TRUE)) else NA),
        `26-35` = as.character(if (any(grepl("Mean.*26", names(.)))) pull(., grep("Mean.*26", names(.), value = TRUE)) else NA),
        Overall  = as.character(Mean_Overall)
      ) %>%
      select(Section, Question, Item, Metric, `19-25`, `26-35`, Overall)
  }
  
  combined_summary <<- bind_rows(combined_summary, processed)
}



# === Generate and export all summaries ===

# Q1a (multi-select)
q1a_tbl <- multi_select_summary_by_age(data, "q1a_")
add_analysis_sheet("Q1a_by__Age", q1a_tbl)
add_to_combined(q1a_tbl, "Q1a")

# Q6 (multi-select)
q6_tbl <- multi_select_summary_by_age(data, "q6_")
add_analysis_sheet("Q6_by_Age", q6_tbl)
add_to_combined(q6_tbl, "Q6")

# Q5b (matrix)
q5b_tbl <- matrix_summary_by_age(data, "q5b_")
add_analysis_sheet("Q5b_by_Age", q5b_tbl)
add_to_combined(q5b_tbl, "Q5b", is_matrix = TRUE)

# Q3a & Q4a: specific to age group
q3a_tbl <- matrix_summary_by_age(data %>% filter(rq2b == "19-25"), "q3a_")
add_analysis_sheet("Q3a_by_19-25", q3a_tbl)

q4a_tbl <- matrix_summary_by_age(data %>% filter(rq2b == "26-35"), "q4a_")
add_analysis_sheet("Q4a_by_26-35", q4a_tbl)

# Q1b (single-select)
q1b_tbl <- freq_percent_overall_and_by_age(data, q1b)
add_analysis_sheet("Q1b_by_Age", q1b_tbl)

# === Export Combined Summary Sheet ===
add_analysis_sheet("Combined_Summary", combined_summary)

# === Save Workbook ===
saveWorkbook(wb, "Whistle_Report_Combined_Agewise1.xlsx", overwrite = TRUE)
