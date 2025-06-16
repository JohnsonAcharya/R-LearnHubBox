### ------PROJECT WHISTLE - Overall Report----

# Load packages
library(tidyverse)
library(readxl)
library(janitor)
library(openxlsx)

# Read and clean data
data <- read_excel("PQ - Whistle (Bengaluru) V1.xlsx") %>% clean_names()
glimpse(data)
wb <- createWorkbook()

# Initialize sheet names to use later
sheet_names <- c()

# Helper function: Frequency + Percent
freq_percent <- function(df, var){
  df %>% count({{ var }}, name = "Freq") %>%
    mutate(Percent = paste0(round(100 * Freq / sum(Freq), 1), "%"))
}

# Define and add data sheets
add_analysis_sheet <- function(sheet_name, df){
  addWorksheet(wb, sheet_name)
  writeData(wb, sheet_name, df)
  sheet_names <<- c(sheet_names, sheet_name)  # track sheet for content index
}

# ====== ANALYSIS SECTIONS ======

# Demographics
add_analysis_sheet("Gender", freq_percent(data, rq2a))
add_analysis_sheet("Age Group", freq_percent(data, rq2b))
add_analysis_sheet("Working Status", freq_percent(data, rq2c))
add_analysis_sheet("Marital Status", freq_percent(data, rq2d))
add_analysis_sheet("Eating Out", freq_percent(data, rq2e_eating_out))
add_analysis_sheet("Alcohol Status", freq_percent(data, rq2e_alcohol))

# Q1a – Multi-select
q1a_vars <- data %>% select(starts_with("q1a_"))
q1a_summary <- colSums(!is.na(q1a_vars))
q1a_df <- data.frame(Occasion = names(q1a_summary), Count = q1a_summary)
q1a_df$Percent <- paste0(round(100 * q1a_df$Count / nrow(data), 1), "%")
add_analysis_sheet("Q1a_Occasions", q1a_df)

# Q1b – Main occasion
add_analysis_sheet("Q1b_Main Occasion", freq_percent(data, q1b))

# Q3a – Age 19–25
q3a_data <- data %>% filter(rq2b == "19-25") %>% select(starts_with("q3a_"))
q3a_result <- q3a_data %>% summarise_all(
  list(
    TopBox = ~mean(. == 5, na.rm = TRUE),
    Top2Box = ~mean(. >= 4, na.rm = TRUE),
    Mean = ~mean(., na.rm = TRUE)
  )
) %>% pivot_longer(everything(), names_to = "Question_Metric", values_to = "Value") %>%
  separate(Question_Metric, into = c("Question", "Metric"), sep = "_(?=[^_]+$)") %>%
  pivot_wider(names_from = Metric, values_from = Value)
add_analysis_sheet("Q3a_Drivers (19-25)", q3a_result)

# Q4a – Age 26–35
q4a_data <- data %>% filter(rq2b == "26-35") %>% select(starts_with("q4a_"))
q4a_result <- q4a_data %>% summarise_all(
  list(
    TopBox = ~mean(. == 5, na.rm = TRUE),
    Top2Box = ~mean(. >= 4, na.rm = TRUE),
    Mean = ~mean(., na.rm = TRUE)
  )
) %>% pivot_longer(everything(), names_to = "Question_Metric", values_to = "Value") %>%
  separate(Question_Metric, into = c("Question", "Metric"), sep = "_(?=[^_]+$)") %>%
  pivot_wider(names_from = Metric, values_from = Value)
add_analysis_sheet("Q4a_Drivers_(26-35)", q4a_result)

# Q5b – Expectations
q5b_data <- data %>% select(starts_with("q5b_"))
q5b_result <- q5b_data %>% summarise_all(
  list(
    TopBox = ~mean(. == 5, na.rm = TRUE),
    Top2Box = ~mean(. >= 4, na.rm = TRUE),
    Mean = ~mean(., na.rm = TRUE)
  )
) %>% pivot_longer(everything(), names_to = "Question_Metric", values_to = "Value") %>%
  separate(Question_Metric, into = c("Question", "Metric"), sep = "_(?=[^_]+$)") %>%
  pivot_wider(names_from = Metric, values_from = Value)
add_analysis_sheet("Q5b Expectations", q5b_result)

# Q6 – Missing Aspects
q6_vars <- data %>% select(starts_with("q6_"))
q6_summary <- colSums(!is.na(q6_vars))
q6_df <- data.frame(Item = names(q6_summary), Count = q6_summary)
q6_df$Percent <- paste0(round(100 * q6_df$Count / nrow(data), 1), "%")
add_analysis_sheet("Q6Missing Aspects", q6_df)

# Q7 – Awareness, Visited, Most Visited
# q7a <- colSums(!is.na(data %>% select(starts_with("q7a_"))))
# q7b <- colSums(!is.na(data %>% select(starts_with("q7b_"))))
# if ("q7c" %in% names(data)) {
#   q7c <- table(data$q7c)
# } else {
#   q7c <- colSums(!is.na(data %>% select(starts_with("q7c_"))))
# }
# 
# bar_labels <- c(
#   "Virat Kohli’s restaurant",
#   "RCB Bar & Café",
#   "Bastian, Shilpa Shetty’s restaurant",
#   "Bob’s",
#   "BLR Brewery",
#   "Sky Garden",
#   "Iron Hill",
#   "Housing Board",
#   "Soshon",
#   "Gilly’s",
#   "Obsidian Sports Bar",
#   "The Stud’s Sports Bar",
#   "Jeff’s",
#   "Dave & Buster’s",
#   "Socials",
#   "Xtreme Sports Bar",
#   "Buffalo Wild Wings",
#   "SkyDeck by Sherlock’s"
# )
# 
# bar_index <- as.integer(gsub("q7a_", "", names(q7a)))
# bar_names <- ifelse(!is.na(bar_index), bar_labels[bar_index], "Other")
# 
# 
# q7_df <- data.frame(
#   Bar_Code = names(q7a),
#   Bar_Name = bar_names,
#   Aware = q7a,
#   Visited = as.numeric(q7b[paste0("q7b_", bar_index)]),
#   Most_Visited = as.numeric(q7c[as.character(bar_index)])
# )
# 
# # Optional: Replace NA with 0
# q7_df[is.na(q7_df)] <- 0
# 
# names(q7_df) <- make.names(names(q7_df), unique = TRUE)
# 
# add_analysis_sheet("Q7 BarsAwareness/Visits", q7_df)
# 

# Q7 — Bar Labels
bar_labels <- c(
  "Virat Kohli’s restaurant", "RCB Bar & Café", "Bastian, Shilpa Shetty’s restaurant", "Bob’s",
  "BLR Brewery", "Sky Garden", "Iron Hill", "Housing Board", "Soshon", "Gilly’s",
  "Obsidian Sports Bar", "The Stud’s Sports Bar", "Jeff’s", "Dave & Buster’s",
  "Socials", "Xtreme Sports Bar", "Buffalo Wild Wings", "SkyDeck by Sherlock’s"
)

# Awareness and visitation
q7a <- colSums(!is.na(data %>% select(starts_with("q7a_"))))
q7b <- colSums(!is.na(data %>% select(starts_with("q7b_"))))

# For q7c (most visited): usually a single column, coded 1-18
q7c_table <- table(data$q7c)
bar_index <- as.integer(gsub("q7a_", "", names(q7a)))
bar_names <- ifelse(!is.na(bar_index), bar_labels[bar_index], "Other")

# Construct clean Q7 summary table
q7_df <- data.frame(
  Bar_Code = names(q7a),
  Bar_Name = bar_names,
  Aware = as.integer(q7a),
  Visited = as.integer(q7b[paste0("q7b_", bar_index)]),
  Most_Visited = as.integer(q7c_table[as.character(bar_index)])
)

# Replace NAs with 0
q7_df[is.na(q7_df)] <- 0

# Write to Excel sheet
addWorksheet(wb, "Q7 Bars Summary")
writeData(wb, "Q7 Bars Summary", q7_df)


# Q8 – Concept Ranking
# q8a <- data %>% select(starts_with("q8a_")) %>% summarise_all(mean, na.rm = TRUE)
# q8b <- data %>% select(starts_with("q8b_")) %>% summarise_all(mean, na.rm = TRUE)
# q8c <- data %>% select(starts_with("q8c_")) %>% summarise_all(mean, na.rm = TRUE)
# q8_summary <- bind_rows(
#   Liked_Most = q8a,
#   Most_Relevant = q8b,
#   Most_Unique = q8c,
#   .id = "Metric"
# )
# add_analysis_sheet("Q8 Concept Ranking", q8_summary)

# STEP 1: Compute Means for Q8a, Q8b, Q8c
q8a <- data %>% select(starts_with("q8a_")) %>% summarise_all(mean, na.rm = TRUE)
q8b <- data %>% select(starts_with("q8b_")) %>% summarise_all(mean, na.rm = TRUE)
q8c <- data %>% select(starts_with("q8c_")) %>% summarise_all(mean, na.rm = TRUE)

# STEP 2: Clean and Standardize Column Names
q8a_clean <- q8a %>% rename_with(~str_remove(., "^q8a_"))
q8b_clean <- q8b %>% rename_with(~str_remove(., "^q8b_"))
q8c_clean <- q8c %>% rename_with(~str_remove(., "^q8c_"))

# STEP 3: Add Metric Column
q8a_clean$Metric <- "Liked_Most"
q8b_clean$Metric <- "Most_Relevant"
q8c_clean$Metric <- "Most_Unique"

# STEP 4: Combine into long format and calculate ranks
q8_summary <- bind_rows(q8a_clean, q8b_clean, q8c_clean) %>% relocate(Metric)

# STEP 5: Compute Rank across each row (lower score = more preferred)
q8_summary_with_ranks <- q8_summary %>%
  pivot_longer(-Metric, names_to = "Concept", values_to = "Mean_Score") %>%
  group_by(Metric) %>%
  mutate(Rank = rank(Mean_Score)) %>%
  pivot_wider(names_from = Metric, values_from = c(Mean_Score, Rank)) %>%
  relocate(Concept)

# STEP 6: Optional - Round scores for presentation
q8_summary_with_ranks <- q8_summary_with_ranks %>%
  mutate(across(starts_with("Mean_Score_"), ~round(., 2)))

# STEP 7: Export to Excel
addWorksheet(wb, "Q8 Concept_Ranking")
writeData(wb, "Q8 Concept_Ranking", q8_summary_with_ranks)


# ====== ADD CONTENTS SHEET WITH LINKS ======
addWorksheet(wb, "Contents", tabColour = "#DDDDDD")

# Create hyperlink table
contents <- tibble(
  `Section Title` = sheet_names,
  `Click to Open` = paste0("'", sheet_names, "'!A1")
)

# Add clickable hyperlinks
writeData(wb, "Contents", contents, startCol = 1, startRow = 1)
for (i in seq_along(sheet_names)) {
  writeFormula(
    wb, "Contents", 
    x = makeHyperlinkString(sheet = sheet_names[i], row = 1, col = 1, text = "Go to Sheet"),
    startCol = 3, startRow = i + 1
  )
}

# ====== SAVE EXCEL ======
saveWorkbook(wb, "Whistle_Analysis_Report_withContents1.5.xlsx", overwrite = TRUE)
