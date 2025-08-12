############################################################
# Project: Supplement Data Analysis
# Author: Johnson  A
# Date: 2025-08-13
# Description:
#   This script explores and analyzes supplement usage data.
#   It loads necessary R libraries, imports the dataset,
#   and prepares it for analysis and visualization.
#
# Input:
#   Supplements.csv  - Contains supplement usage survey data
#
# Output:
#   Summary statistics, visualizations, and cleaned dataset
############################################################



### Explore and Analyze Supplement Data in R

##   🔹Step 1: Load Required Libraries

library(tidyverse)   # For data manipulation and visualization
library(readr)       # For reading CSV or text files
library(ggplot2)     # For plotting
library(dplyr)       # For data wrangling
library(skimr)       # For data overview
library(janitor)     # For cleaning column names

## Load Dataset

sf <- read.csv("Supplements.csv")

str(sf)
dim(sf)
glimpse(sf)
summary(sf$SU_Calcium)

boxplot(sf$SU_Calcium)

# Created freq table of Multiple choice question SU1 columns
# Select only AgeGroup + SU1_* columns
sf %>%
  select(AgeGroup, starts_with("SU1_")) %>%
  group_by(AgeGroup) %>%
  summarise(across(starts_with("SU1_"), sum, na.rm = TRUE)) %>%
  arrange(AgeGroup)


## -----------------------------------------------------------------------------


library(dplyr)
library(tidyr)

table(sf$Importance_Quality)

# Created freq table of Multiple choice question SU1 columns
sf %>%
  # Fix column names so spaces don't cause issues
  rename_with(~ gsub(" ", "_", .x)) %>%
  # Keep AgeGroup and SU1_ columns
  select(AgeGroup, starts_with("Importance_")) %>%
  # Convert to long format
  pivot_longer(cols = starts_with("Importance_"),
               names_to = "Supplement",
               values_to = "Value") %>%
  # Count entries where Value > 0
  group_by(Supplement, AgeGroup) %>%
  summarise(Count = sum(Value > 0, na.rm = TRUE), .groups = "drop") %>%
  # Reshape to wide format
  pivot_wider(names_from = AgeGroup, values_from = Count, values_fill = 0) %>%
  # Add total per supplement
  mutate(Total = rowSums(across(where(is.numeric)))) %>%
  # Add grand total row
  bind_rows(summarise(., across(where(is.numeric), sum)) %>%
              mutate(Supplement = "Grand Total")) %>%
  select(Supplement, `18-24`, `25-34`, Total)



## -----------------------------------------------------------------------------

#  count of how many respondents chose each scale value (e.g., 1–5) for each item.

sf %>%
  # Select only the Importance_* columns
  select(starts_with("Importance_")) %>%
  # Convert to long format
  pivot_longer(cols = everything(),
               names_to = "Attribute",
               values_to = "Score") %>%
  # Count how many responses for each score
  group_by(Attribute, Score) %>%
  summarise(Count = n(), .groups = "drop") %>%
  # Spread so each score is its own column
  pivot_wider(names_from = Score, values_from = Count, values_fill = 0) %>%
  arrange(Attribute)



## -----------------------------------------------------------------------------

# both counts and percentages for each scale value (1–5) in your Importance_ columns:


sf %>%
  # Select only Importance_* columns
  select(starts_with("Importance_")) %>%
  # Convert to long format
  pivot_longer(cols = everything(),
               names_to = "Attribute",
               values_to = "Score") %>%
  group_by(Attribute, Score) %>%
  summarise(Count = n(), .groups = "drop") %>%
  # Calculate total per attribute
  group_by(Attribute) %>%
  mutate(Percent = round(Count / sum(Count) * 100, 1)) %>%
  ungroup() %>%
  # Reshape so we have both Count and Percent columns for each score
  pivot_wider(names_from = Score,
              values_from = c(Count, Percent),
              values_fill = 0) %>%
  arrange(Attribute)



## -----------------------------------------------------------------------------

# mean score for each Importance_ attribute

sf %>%
  select(starts_with("Importance_")) %>%
  pivot_longer(cols = everything(),
               names_to = "Attribute",
               values_to = "Score") %>%
  group_by(Attribute) %>%
  summarise(
    Mean_Score = round(mean(Score, na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  arrange(Attribute)



## -----------------------------------------------------------------------------

## Calculate Top Box, Top 2 Box, and Mean Score for all your Importance_ columns

#   Definitions (based on the usual 1–5 Likert scale):
#   
#   Top Box (TB) = % of respondents giving the highest score (5)
# 
#   Top 2 Box (T2B) = % giving scores 4 or 5
# 
#   Mean Score = average score


sf %>%
  select(starts_with("Importance_")) %>%
  pivot_longer(cols = everything(),
               names_to = "Attribute",
               values_to = "Score") %>%
  group_by(Attribute) %>%
  summarise(
    TopBox = round(sum(Score == 5, na.rm = TRUE) / n() * 100, 1),
    Top2Box = round(sum(Score >= 4, na.rm = TRUE) / n() * 100, 1),
    Mean_Score = round(mean(Score, na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  arrange(Attribute)

# Cross Check with table function for each importance count
table(sf$Importance_Ingredients)
summary(sf$Importance_Price)


## -----------------------------------------------------------------------------

library(dplyr)
library(tidyr)


# Assuming 'supplement_survey_expanded' exists

# Step 1: Summarize counts by AgeGroup
supplement_freq_by_age <- sf %>%
  select(AgeGroup, starts_with("SU_")) %>%
  group_by(AgeGroup) %>%
  summarise(across(starts_with("SU_"), sum), .groups = "drop")

# Step 2: Convert to long format
supplement_freq_long <- supplement_freq_by_age %>%
  pivot_longer(
    cols = starts_with("SU_"),
    names_to = "Supplements",
    values_to = "Count"
  )

# Step 3: Pivot wider for final table
supplement_freq_table <- supplement_freq_long %>%
  pivot_wider(
    names_from = AgeGroup,
    values_from = Count,
    values_fill = 0
  ) %>%
  mutate(Supplements = sub("^SU_", "", Supplements))  # clean names

# Step 4: Add Grand Total column (row sums)
supplement_freq_table <- supplement_freq_table %>%
  mutate(Total = rowSums(across(where(is.numeric))))

# Step 5: Add Grand Total row (column sums)
grand_total_row <- supplement_freq_table %>%
  summarise(across(where(is.numeric), sum)) %>%
  mutate(Supplements = "Grand Total") %>%
  select(names(supplement_freq_table))  # ensure column order matches

# Step 6: Bind Grand Total row to bottom
supplement_freq_table_with_totals <- bind_rows(supplement_freq_table, grand_total_row)

# View the table with totals
print(supplement_freq_table_with_totals)


#Percentage within each AgeGroup (column-wise percentages)
percentage_table_by_age <- supplement_freq_table_with_totals %>%
  mutate(across(-Supplements, ~ . / sum(.) * 100))

print(percentage_table_by_age)


total_samples <- nrow(sf)  # total number of respondents, e.g. 20

percentage_table_total <- supplement_freq_table_with_totals %>%
  mutate(across(where(is.numeric), ~ . / total_samples * 100))

print(percentage_table_total)


## -----------------------------------------------------------------------------



library(dplyr)
library(tidyr)

# Summarize counts of supplements by AgeGroup (if not already done)
supplement_freq_by_age <- sf %>%
  select(AgeGroup, starts_with("SU_")) %>%
  group_by(AgeGroup) %>%
  summarise(across(starts_with("SU_"), sum), .groups = "drop")

# Calculate total respondents per AgeGroup
agegroup_counts <- sf %>%
  count(AgeGroup) %>%
  rename(Total = n)

# Join totals to supplement counts
supplement_freq_pct <- supplement_freq_by_age %>%
  left_join(agegroup_counts, by = "AgeGroup") %>%
  # Calculate percentage: count / total respondents per age group * 100
  mutate(across(starts_with("SU_"), ~ .x / Total * 100)) %>%
  select(-Total)  # remove Total if not needed in result

# Reshape to final table format: Supplements as rows, AgeGroups as columns
supplement_freq_pct_long <- supplement_freq_pct %>%
  pivot_longer(
    cols = starts_with("SU_"),
    names_to = "Supplements",
    values_to = "Percentage"
  )

supplement_pct_table <- supplement_freq_pct_long %>%
  pivot_wider(
    names_from = AgeGroup,
    values_from = Percentage,
    values_fill = 0
  ) %>%
  mutate(Supplements = sub("^SU_", "", Supplements))  # clean names

# View the percentage table
print(supplement_pct_table)


## -----------------------------------------------------------------------------

library(dplyr)
library(tidyr)

# Step 1: Summarize counts of supplements by Gender
supplement_freq_by_gender <- sf %>%
  select(Gender, starts_with("SU_")) %>%
  group_by(Gender) %>%
  summarise(across(starts_with("SU_"), sum), .groups = "drop")

# Step 2: Calculate total respondents per Gender
gender_counts <- sf %>%
  count(Gender) %>%
  rename(Total = n)

# Step 3: Calculate percentage of users of each supplement within each Gender
supplement_gender_pct <- supplement_freq_by_gender %>%
  left_join(gender_counts, by = "Gender") %>%
  mutate(across(starts_with("SU_"), ~ .x / Total * 100)) %>%
  select(-Total)

# Step 4: Reshape to long format
supplement_gender_long <- supplement_gender_pct %>%
  pivot_longer(
    cols = starts_with("SU_"),
    names_to = "Supplements",
    values_to = "Percentage"
  )

# Step 5: Pivot wider to have Supplements as rows and Gender as columns
supplement_gender_table <- supplement_gender_long %>%
  pivot_wider(
    names_from = Gender,
    values_from = Percentage,
    values_fill = 0
  ) %>%
  mutate(Supplements = sub("^SU_", "", Supplements))  # clean supplement names

# View the Gender percentage table
print(supplement_gender_table)


# (Optional) To combine with the AgeGroup percentage table side-by-side,
# you can join by 'Supplements' or just display separately for clarity.

# Assuming 'supplement_pct_table' is your AgeGroup percentage table from before:
combined_table <- supplement_pct_table %>%
  left_join(supplement_gender_table, by = "Supplements")

print(combined_table)


## -----------------------------------------------------------------------------
