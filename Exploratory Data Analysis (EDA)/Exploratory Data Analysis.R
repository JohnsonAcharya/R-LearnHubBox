### Step-by-Step Guide to Analyze Data in R

##   🔹Step 1: Load Required Libraries

library(tidyverse)   # For data manipulation and visualization
library(readr)       # For reading CSV or text files
library(ggplot2)     # For plotting
library(dplyr)       # For data wrangling
library(skimr)       # For data overview
library(janitor)     # For cleaning column names



##   🔹 Step 2: Import the Dataset

df <- read_csv("F:/R PROGRAMMIG/Github-Projects/R-LearnHubBox/Data/Pixar_films/box_office.csv", show_col_types = FALSE)     # For CSV files
# OR
df <- read_excel("your_file.xlsx")  # For Excel files (requires readxl package)

##  Check structure:
str(df)
glimpse(df)
view(df)


##  🔹 Step 3: Clean Column Names


df <- clean_names(df)  # Converts column names to snake_case


## 🔹 Step 4: Understand the Data

head(df)             # First few rows
summary(df)          # Summary statistics
skim(df)             # Detailed summary
names(df)            # Column names
dim(df)              # Dimensions (rows, columns)



## 🔹 Step 5: Check for Missing Values

colSums(is.na(df))     # Total NA in each column
anyNA(df)              # Is there any missing data?

