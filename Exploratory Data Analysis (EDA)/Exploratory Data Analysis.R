### Step-by-Step Guide to Analyze Data in R

##   🔹Step 1: Load Required Libraries

library(tidyverse)   # For data manipulation and visualization
library(readr)       # For reading CSV or text files
library(ggplot2)     # For plotting
library(dplyr)       # For data wrangling
library(skimr)       # For data overview
library(janitor)     # For cleaning column names



##   🔹 Step 2: Import the Dataset

df <- read_csv("your_file.csv")     # For CSV files
# OR
df <- read_excel("your_file.xlsx")  # For Excel files (requires readxl package)
