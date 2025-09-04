### Step-by-Step Guide to Analyze Data in R

##   🔹Step 1: Load Required Libraries

library(tidyverse)   # For data manipulation and visualization
library(readr)       # For reading CSV or text files
library(ggplot2)     # For plotting
library(dplyr)       # For data wrangling
library(skimr)       # For data overview
library(janitor)     # For cleaning column names



##   🔹 Step 2: Import the Dataset

# Method 1

df <- read_csv("F:/R PROGRAMMIG/Github-Projects/R-LearnHubBox/Data/Pixar_films/box_office.csv", show_col_types = FALSE)     # For CSV files
# OR
df <- read_excel("your_file.xlsx")  # For Excel files (requires readxl package)

# And

df <- read.csv(text = url("Data Web Link"))


# Method 2

data("iris")

df <- datasets::iris


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



## 🔹 Step 6: Data Type Checks & Conversion

sapply(df, class)       # Check column types
df$gender <- as.factor(df$budget)  # Convert to factor
df$date <- as.Date(df$date, format = "%Y-%m-%d")  # Convert to date



##🔹 Step 7: Univariate Analysis
##   Univariate analysis is a type of statistical analysis that examines a single variable at a time.

##   For numerical variables:

summary(df$gender)
hist(df$budget)
boxplot(df$budget)


##  For categorical variables:

table(df$film)
table(df$gender)

barplot(table(df$film))



##  🔹 Step 8: Bivariate/Multivariate Analysis
##   Compare two variables:

# Numerical vs Numerical

plot(df$budget, df$boxofficeuscanada)


## Categorical vs Categorical

table(df$film, df$boxofficeworldwide)

# Categorical vs Numerical

plot(budget ~ film, data = df) # notworking recheck


# -----------------------------------------------------------------------------

# Method 2

data("iris")

df <- datasets::iris


####################################
# Display summary Statistics
####################################


str(iris)
class(iris)
# head() & tail()

head(iris,4)
tail(iris,4)


summary(iris)
summary(iris$Sepal.Length)
view(iris$Sepal.Length)


# Check to see if there is missing data?
sum(is.na(iris))    # 0 or missing numbers
anyNA(iris)    # True or False

library(skimr)
# Perform skim for to display summary statistic
skim(iris)

# Group data by species and perform skim

iris %>% 
  group_by(Species) %>% 
  skim()

iris |> 
  group_by(Species) |> 
  summarise(Sepal.Length)

####################################
# Quick Data Visualization

# R base plot
####################################


# Panel plots
plot(iris)

plot(iris, col = "blue") # Added Col argument to set color


# Scatter plot

plot(iris$Sepal.Width, iris$Sepal.Length)


# Task:
# Write a function that prints integers 1 to 50, replacing any integers:
#   - integers divisible by 3 with "Fizz",
#   - integers divisible by 5 with "Buzz", and
#   - integers divisible by both 3 and 5 as "FizzBuzz".


v <- c("a","b","c","d","e","f")
v[-3:-5]

##  method 1

fizzbuzz <- function(n) {
  sapply(1:50, function(x) {
    if (x %% 3 == 0 & x %% 5 == 0) "FizzBuzz"
    else if (x %% 3 == 0 & x %% 5 != 0) "Fizz"
    else if (x %% 5 == 0 & x %% 3 != 0) "Buzz"
    else as.character(x)
  })
}


fizzbuzz(25)


## different method 2
fizzb <- function(n) {
  if (n < 0) stop("Input must be a non-negative integer")
  
  # Create a vector 1:n
  x <- 1:n
  
  # Convert to character (so we can replace with "Fizz"/"Buzz")
  result <- as.character(x)
  
  # Apply replacements
  result[x %% 3 == 0 & x %% 5 == 0] <- "FizzBuzz"
  result[x %% 3 == 0 & x %% 5 != 0] <- "Fizz"
  result[x %% 5 == 0 & x %% 3 != 0] <- "Buzz"
  
  return(result)
}

fizzb(5)

## different method 3
fb <- print(ifelse(1:50 %% 3 == 0 | 1:50 %% 5 == 0,
             paste0(ifelse(1:50 %% 3 == 0, "Fizz", ""),
                    ifelse(1:50 %% 5 == 0, "Buzz", "")),
             1:50))
