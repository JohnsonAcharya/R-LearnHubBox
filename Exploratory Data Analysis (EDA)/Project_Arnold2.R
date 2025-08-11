




df_arnold <- read.csv("Arnold2_data.csv")

View(df_arnold)
glimpse(df_arnold)
str(df_arnold)


dim(df_arnold)
summary(df_arnold)



Q2a <- df_arnold %>% 
  group_by(Q1a_Rank, GymExperience) %>% 
  summarise(n = n(), .groups = "drop") %>% 
  pivot_wider(names_from = GymExperience, values_from = n, values_fill = 0)


Q2a_Age <- df_arnold %>% 
  group_by(Q2a_, Age_Group) %>% 
  summarise(n = n(), .groups = "drop") %>% 
  pivot_wider(names_from = Age_Group, values_from = n, values_fill = 0)


banner_table <- full_join(Q2a, Q2a_Age, by = "GymFreq_3M")






# Required libraries
library(dplyr)
library(tidyr)

create_banner_table_smart <- function(df_arnold, mr_prefix) {
  # Step 1: Match all multi-response columns with smart regex (e.g., "Q2a_1_01", "Q2a_3_01", etc.)
  mr_cols <- grep(paste0("^", mr_prefix, "[0-9]+_01$"), names(df_arnold), value = TRUE)
  
  if (length(mr_cols) == 0) {
    stop("❌ No matching columns found. Please check your prefix.")
  }
  
  # Step 2: Convert to long
  df_long <- df_arnold %>%
    pivot_longer(cols = all_of(mr_cols),
                 names_to = "Question_Option",
                 values_to = "Selected") %>%
    filter(Selected == 1 | Selected == TRUE)
  
  # Step 3A: Banner by Gym Experience
  gym_table <- df_long %>%
    group_by(Question_Option, GymExperience) %>%
    summarise(n = n(), .groups = "drop") %>%
    pivot_wider(names_from = GymExperience, values_from = n, values_fill = 0)
  
  # Step 3B: Banner by Age Group
  age_table <- df_long %>%
    group_by(Question_Option, Age_Group) %>%
    summarise(n = n(), .groups = "drop") %>%
    pivot_wider(names_from = Age_Group, values_from = n, values_fill = 0)
  
  # Step 4: Merge both banners
  banner_table <- left_join(gym_table, age_table, by = "Question_Option")
  
  return(banner_table)
}


q2a_banner <- create_banner_table_smart(df_arnold, "Q18d")

names(df_arnold)

-------------------------------------------------------------------------------------
# Generate the banner-style table
q2a_banner <- create_banner_table(df_arnold, mr_prefix = "Q22a_")

q2a_banner <- create_banner_table_separate(df_arnold, "Q2a_1")
print(q2a_banner)


q2a_cols <- c("Q2a_1_01", "Q2a_01_02", "Q2a_1_03", "Q2a_1_04", "Q2a_1_05", "Q2a_1_06")
q2a_banner <- create_banner_table_manual(df_arnold, Q4c_cols)







library(dplyr)
library(tidyr)

create_banner_table_smart1 <- function(df_arnold, prefix) {
  # Identify columns that match the prefix
  matched_cols <- names(df_arnold)[grepl(paste0("^", prefix), names(df_arnold))]
  
  # Check for Gym Experience and Age Group
  if (!all(c("GymExperience", "Age_Group") %in% names(df_arnold))) {
    stop("Required columns 'GymExperience' and 'Age_Group' not found.")
  }
  
  df_arnold <- df_arnold %>%
    mutate(
      banner_group = paste(GymExperience, Age_Group, sep = "_")
    )
  
  if (length(matched_cols) > 1) {
    # Multiple-response case
    df_long <- df_arnold %>%
      select(all_of(matched_cols), banner_group) %>%
      pivot_longer(
        cols = all_of(matched_cols),
        names_to = "Question_Option",
        values_to = "Response"
      ) %>%
      filter(Response == 1) %>%
      group_by(Question_Option, banner_group) %>%
      summarise(Frequency = n(), .groups = "drop") %>%
      pivot_wider(names_from = banner_group, values_from = Frequency, values_fill = 0)
    
    return(df_long)
    
  } else if (length(matched_cols) == 1) {
    # Single-choice case
    df_single <- df_arnold %>%
      select(all_of(matched_cols), banner_group) %>%
      filter(!is.na(.data[[matched_cols]])) %>%
      group_by(!!sym(matched_cols), banner_group) %>%
      summarise(Frequency = n(), .groups = "drop") %>%
      pivot_wider(names_from = banner_group, values_from = Frequency, values_fill = 0) %>%
      rename(Question_Option = !!sym(matched_cols))
    
    return(df_single)
    
  } else {
    stop("❌ No matching columns found. Please check your prefix.")
  }
}


# For Multiple Choice
Q2a <- create_banner_table_smart1(df_arnold, "Q2a1_")

# For Single Choice
Q18d <-  create_banner_table_smart1(df_arnold, "Q18d")


# q2a_cols <- c("Q2a_1_01", "Q2a_2_01", "Q2a_3_01", "Q2a_4_01", "Q2a_5_01", "Q2a_6_01")
# q2a_banner <- create_banner_table_manual(df, q2a_cols)

names(df_arnold)



create_banner_table_smart1.1 <- function(df_arnold, prefix, banner_vars = c("Age_Group", "GymExperience")) {
  # Select all columns starting with prefix
  matching_cols <- grep(paste0("^", prefix), names(df_arnold), value = TRUE)
  
  if (length(matching_cols) == 0) {
    stop("❌ No matching columns found. Please check your prefix.")
  }
  
  # Pivot longer
  df_long <- df_arnold %>%
    select(all_of(c(banner_vars, matching_cols))) %>%
    pivot_longer(
      cols = all_of(matching_cols),
      names_to = "Question_Option",
      values_to = "Selected"
    ) %>%
    filter(Selected == 1)  # Filter only selected options
  
  # Combine banner columns into one
  df_long <- df_long %>%
    mutate(Banner = paste(!!sym(banner_vars[2]), !!sym(banner_vars[1]), sep = "_"))
  
  # Count
  df_banner <- df_long %>%
    group_by(Question_Option, Banner) %>%
    summarise(n = n(), .groups = "drop") %>%
    pivot_wider(
      names_from = Banner,
      values_from = n,
      values_fill = 0
    )
  
  return(df_banner)
}

Q2a1_ <- create_banner_table_smart1.1(df_arnold, "Q2a1_")






create_banner_table_smart_v2 <- function(df_arnold, prefix, banner_vars = c("Age_Group", "GymExperience")) {
  # Match columns based on prefix
  matching_cols <- grep(paste0("Q2a1_", prefix), names(df_arnold), value = TRUE)
  
  if (length(matching_cols) == 0) {
    stop("❌ No matching columns found. Please check your prefix.")
  }
  
  # Combine banner vars into one banner column
  df_arnold <- df_arnold %>%
    mutate(Banner = paste(!!sym(banner_vars[2]), !!sym(banner_vars[1]), sep = "_"))
  
  if (length(matching_cols) == 1) {
    # Single-choice logic
    df_single <- df_arnold %>%
      filter(!is.na(.data[[matching_cols[1]]])) %>%
      group_by(!!sym(matching_cols[1]), Banner) %>%
      summarise(n = n(), .groups = "drop") %>%
      pivot_wider(names_from = Banner, values_from = n, values_fill = 0) %>%
      rename(Question_Option = 1)
    
    return(df_single)
    
  } else {
    # Multi-choice logic
    df_long <- df_arnold %>%
      select(all_of(c("Banner", matching_cols))) %>%
      pivot_longer(
        cols = all_of(matching_cols),
        names_to = "Question_Option",
        values_to = "Selected"
      ) %>%
      filter(Selected == 1) %>%
      group_by(Question_Option, Banner) %>%
      summarise(n = n(), .groups = "drop") %>%
      pivot_wider(names_from = Banner, values_from = n, values_fill = 0)
    
    return(df_long)
  }
}


# For a multi-choice prefix (like Q2a_)
Q2a_V2 <- create_banner_table_smart_v2(df_arnold, "Q2a1_")

# For a single-choice prefix (like Q18d)
Q18d_v2 <- create_banner_table_smart_v2(df_arnold, "Q18d")




create_banner_table_smart_v2.1 <- function(df_arnold, prefix, banner_vars = c("Age_Group", "GymExperience")) {
  # Match columns based on prefix
  matching_cols <- grep(prefix, names(df_arnold), value = TRUE)
  
  if (length(matching_cols) == 0) {
    stop("❌ No matching columns found. Please check your prefix.")
  }
  
  # Combine banner vars into one banner column
  df_arnold <- df_arnold %>%
    mutate(Banner = paste(!!sym(banner_vars[2]), !!sym(banner_vars[1]), sep = "_"))
  
  if (length(matching_cols) == 1) {
    # Single-choice logic
    df_single <- df_arnold %>%
      filter(!is.na(.data[[matching_cols[1]]])) %>%
      group_by(!!sym(matching_cols[1]), Banner) %>%
      summarise(n = n(), .groups = "drop") %>%
      pivot_wider(names_from = Banner, values_from = n, values_fill = 0) %>%
      rename(Question_Option = 1)
    
    return(df_single)
    
  } else {
    # Multi-choice logic
    df_long <- df_arnold %>%
      select(all_of(c("Banner", matching_cols))) %>%
      pivot_longer(
        cols = all_of(matching_cols),
        names_to = "Question_Option",
        values_to = "Selected"
      ) %>%
      filter(Selected == 1) %>%
      group_by(Question_Option, Banner) %>%
      summarise(n = n(), .groups = "drop") %>%
      pivot_wider(names_from = Banner, values_from = n, values_fill = 0)
    
    return(df_long)
  }
}

Q2a1_V2.1 <- create_banner_table_smart_v2.1(df_arnold, "Q1a_")


table(df_arnold$GymExperience)
table(df_arnold$Age_Group)
table(df_arnold$Q18b)
table(df_arnold$Q2a_)

prop.table(table(df_arnold$GymExperience)) * 100
prop.table(table(df_arnold$Q2a_)) * 100

prop.table(table(df_arnold$GymExperience, df_arnold$Age_Group,df_arnold$Q18b), margin = 2) * 100

#For multiple-response, sum binary columns:

colSums(df_arnold[, grep("Q1a_", names(df_arnold))])

# Cross Tabulations
#E.g., cross tab brand by supplement

install.packages("gtsummary")
library(gtsummary)

ftable(xtabs(~ Q2a1_01 + GymExperience + Age_Group, data = df_arnold))
ftable(xtabs(~ GymExperience + Age_Group + q2a_nutrition + q2a_exercise, data = df_arnold))


table(df_arnold$Q18b, df_arnold$GymExperience)


library(dplyr)
df_arnold %>%
  select(df_arnold$Q1a_Rank1) %>%
  gather(key = "rank", value = "item") %>%
  group_by(item) %>%
  summarise(count = n())

#--------------------------------------------------------------------------------

sf <- read.csv("Supplements.csv")

str(sf)


# Select only AgeGroup + SU1_* columns
sf %>%
  select(AgeGroup, starts_with("SU1_")) %>%
  group_by(AgeGroup) %>%
  summarise(across(starts_with("SU1_"), sum, na.rm = TRUE)) %>%
  arrange(AgeGroup)

#--------------------------------------------------------------------------------

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


#--------------------------------------------------------------------------------
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


#--------------------------------------------------------------------------------
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



#--------------------------------------------------------------------------------
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


#--------------------------------------------------------------------------------

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
#--------------------------------------------------------------------------------

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
