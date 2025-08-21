## R + Shiny Recovery Plan – Daily Focus with Code
############################################################
# Script: R + Shiny Recovery Plan – Daily Focus with Code
# Author: Johnson A
# Date: 15-08-2025
#
# Description:
#   This script outlines daily tasks, learning goals, 
#   and example code for rebuilding R and Shiny skills.
#
# Purpose:
#   Step-by-step recovery plan to strengthen fundamentals,
#   practice coding daily, and prepare for interviews.
#
# Notes:
#   - Update <Insert Date> each day
#   - Add progress notes at the bottom
############################################################



## 1️⃣ Basic Data Structures in R
# (a) Vector
# 
# Definition: A sequence of elements of the same type.
# 
# Types: numeric, character, logical, integer, complex.

# Numeric vector

num_vec <- c(1,2,3,4,5)


# Character vector

char_vec <- c("Apple","Banana","Mango")


# Logical vector
Logi_vec <- c(TRUE, FALSE, TRUE)


## Summary

summary(num_vec)   # Check Min, Max, Mean, Median, Quartiles
length(num_vec)    # check length/count of elements
class(num_vec)     # Check Type of objects



# (b) Factor
# 
# Definition: Stores categorical data with levels.

colors <- factor(c("red", "blue", "red", "green"))
colors
summary(colors)    # Frequency table
levels(colors)      # Unique categories - R sorts these levels alphabetically.


# (c) Matrix
# 
# Definition: 2D array with elements of the same type.

mat <- matrix(1:12, nrow = 3, byrow = T)
mat

summary(mat)   # Summary of all elements
dim(mat)      # Check Dimensions


# (d) List
#
#Definition: Collection of elements of different types.


mylist <- list(name = "Johnosn", age = 2, score = c(90,85,88))
mylist
summary(mylist) # Structure summary


# (e) Data Frame
# 
# Definition: Tabular data structure with columns of possibly different types.


df <- data.frame(Name = c("A", "B", "C"),
                 Age = c(25,30,34),
                 Score = c(90,88,98)
                 )
    

summary(df)    # Column-wise summary
str(df)        # Structure of data frame


# 2️⃣ Quick Summary Functions

# | Function            | Purpose                                   |
#   | ------------------- | ----------------------------------------- |
#   | `summary()`         | Descriptive stats for each element/column |
#   | `str()`             | Structure of the object                   |
#   | `length()`          | Number of elements                        |
#   | `dim()`             | Dimensions (rows, cols)                   |
#   | `head()` / `tail()` | First/last rows                           |
#   | `table()`           | Frequency table                           |
#   | `class()`           | Object type                               |
#   | `typeof()`          | Internal storage type                     |
  



#  3️⃣ Example: Putting It All Together

# Create a data frame

df <- data.frame(
  Gender = factor(c("Male", "Female", "Female", "Male")),
  Age = c(25,30,28,22),
  Score = c(90,88,85,95)
)

# Basic summaries
summary(df)         # Summary of all columns
str(df)             # Data structure
table(df$Gender)    # Count by gender
mean(df$Score)     # Mean of Score
df[df$Age > 22,]    # Filter data by age > 22


###  Task

# Practice task with mtcars

# Step 1: Load the dataset
# 
# mtcars is built into R, so no file loading needed.

data("mtcars")    # loads the dataset

summary(mtcars$mpg)
str(mtcars)
head(mtcars)
view(mtcars)
table(mtcars$mpg)


# Create breaks of size 5 of numeric obs data
# Bin mpg in steps of 5
mtcars$mpg_bin <- cut(mtcars$mpg, breaks = seq(10, 40, by = 5), right = FALSE)

# Create summary table - to check the count of mpg by range to match with below histogram
mpg_table <- as.data.frame(table(mtcars$mpg_bin))


# 👍 let’s visualize it step by step.

# Histogram of mpg


hist(mtcars$mpg,
     main = "Distribution of miles per gallon(mpg)",
     xlab = "MPG",
     col = "lightblue",
     border = "blue")

#Add abline of mean and median to cross verify mean vs median
abline(v = mean(mtcars$mpg), col = "red", lwd = 2) # added mean line and lwd is thickness of abline
abline(v = median(mtcars$mpg), col = "green", lwd = 2) # added median line
# add Legend
legend("topright",
       legend = c("Mean", "Median"), # Red line = mean (~20.1) and Green line = median (~19.2)
       col = c("red", "green"),
       lwd = 2)



# Histogram of hp (horse power)
summary(mtcars$hp)

hist(mtcars$hp)
abline(v = mean(mtcars$hp), col = "red", lwd = 3, lty = 2)
text(x = mean(mtcars$hp),
  y = max(hist(mtcars$hp, plot = FALSE)$counts), 
     labels = paste("Mean =", round(mean(mtcars$hp), 1)), 
     pos = 4, col = "blue")


# Boxplot of hp

table(mtcars$hp)   # check the outlier count

boxplot(mtcars$hp,
        main = "Boxplot of Horse power",
      col = "lightgreen")


## ==============================================================================

## 1️⃣ Functions in R

# Functions are reusable blocks of code

my_function <- function(arg1, arg2) {   # function define it
  result <- arg1 + arg2                 # argument go inside () we can add x,y also
  return(result)                        # return gives back the result
}

my_function(39,8)

# 2️⃣ Apply Family

# Instead of writing loops, R has the apply family of functions:

# (a) apply()
# Works on matrices/data frames → apply a function row-wise or column-wise.

apply(mtcars[,1:4], 2, mean) # it gives you each entire column total average (2 = columns)
apply(mtcars[,1:11], 1, sum)  # sum of each row (1 = rows)


# (b) lapply()

# Works on lists or vectors → always returns a list.
# mean of each column (list output)
lapply(mtcars[,1:4], mean)   # mean of each column (list output)

# lapply gives list output of select column from data


# mean of each column (vector output)
sapply(mtcars[,1:4], mean)


# mean mpg by number of cylinders
tapply(mtcars$mpg, mtcars$cyl, mean)  # same result will get in using dplyr belwo

# max horsepower (hp) by cylinder group, how would you write that using tapply()
tapply(mtcars$hp, mtcars$cyl, max)

# both gives same result below used dplyr for cyl  group_by mpg
mtcars %>% 
  group_by(mtcars$cyl) %>% 
  summarise(mpg_mean = mean(mpg))


# (e) mapply()
# Multivariate” apply → applies a function to multiple vectors in parallel.

mapply(sum,1:5, 6:10)  # sums (1+6, 2+7, …)


# Task - on apply function

# Mini practice challenge to use all 5 of the apply-family functions.

# Challenge: Apply Family with mtcars
# 1. apply()
# 👉 Find the sum of each row for the first 5 cars in mtcars.

str(mtcars)

# Keep only numeric columns
num_data <- mtcars[ , sapply(mtcars, is.numeric)]

# Now apply over the first 5 rows
row_sums <- apply(num_data[1:5, ], 1, sum)
row_sums


# 2. lapply()
# 👉 Use lapply() to get the mean of mpg, hp, and wt separately (expect a list).

lapply(mtcars[c("mpg", "hp", "wt")], mean)  # use char when specific column output want

# 3. sapply()
# 👉 Use sapply() to do the same as above, but check how the output differs.

sapply(mtcars[c("mpg", "hp", "wt")], mean)
sapply(mtcars, class) # to simply check each column class

# 4. tapply()
# 👉 Find the average horsepower (hp) by number of gears (gear column).

tapply(mtcars$hp, mtcars$gear, mean)

# 5. mapply()
# 👉 Add two sequences together: (1:5) and (6:10) using mapply().
mapply(sum, 1:5, 6:10)

# add mpg and hp element-wise (row by row)
mapply(sum, mtcars$mpg, mtcars$hp)


#👉 Task:
# Using tapply(), calculate the mean mpg for each number of cylinders (cyl).
# Then, wrap it in sapply() so the result is a numeric vector instead of a list.

tapply(mtcars$mpg, mtcars$cyl, mean)

sapply(split(mtcars$mpg, mtcars$cyl), mean)

# ------------------------------------------------------------------------------
 
## Task 

  # At its simplest, an R function looks like this:

my_function_new <- function(arg1, arg2) {
  output <- arg1 + arg2
  return(output)
}
my_function_new(5,8)

# ?: Why do you think we need to use return() inside the function, instead of just
#    writing result on the last line?

# A: return() tells R what value to give back when the function finishes.
# It’s like saying: “Okay R, here’s the final answer I want you to keep

# in R, if you just put an expression on the last line (without return()), 
# R will still return it. 
# For example:

my_sum <- function(a,b){
  a+b
}

my_sum(2,3) # works fine, gives 5

#  So return() isn’t always required — but many people use it to make the function’s purpose clearer.

# practice:
#   Can you write a function called mpg_per_hp that takes two arguments (mpg, hp) 
#   and returns their ratio (mpg divided by hp)?

mpg_per_hp <- function(mpg, hp) {
  result <- mpg/hp
  return(result)
}

mpg_per_hp(mtcars$hp, mtcars$mpg)


#👉 Next step: what if you wanted your function to return the average mpg/hp 
# ratio instead of all the individual ones?

mpg_per_hp_mean <- function(mpg, hp) {
  result <- mean(mpg/hp)
  return(result)
}

mpg_per_hp_mean(mtcars$mpg, mtcars$hp)


# Let me toss this back to you:
#   If you wanted your function to be a little more polished, how could you 
# make it return the result rounded to 2 decimal places instead of that long decimal?

mpg_per_hp_round <- function(mpg, hp) {
  result <- round(mean(mpg/hp),2)
  return(result)
}

mpg_per_hp_round(mtcars$mpg, mtcars$hp)



# Quick challenge for you:
#   Suppose you want the user to decide how many decimal places to round to (instead of always 2).
# 
# ? 👉 How would you add a third argument to your function so the rounding precision is flexible?


# in R, you can add more arguments to your function just by putting them inside the parentheses in the definition.

# If we want to let the user choose how many digits to round to, we can add a new argument, say digits:

mpg_per_hp_digit <- function(mpg, hp, digits) {  # add digit in function
  result <- round(mean(mpg/hp), digits)
  return(result)
}

mpg_per_hp_digit(mtcars$mpg, mtcars$hp, 3) # rounds to 3 decimals or add any number as round


# Next step: Sometimes we also want to give our function a default value (so if the user doesn’t type digits, it will automatically use 2).

mpg_per_hp_digit_dflt <- function(mpg, hp, digits = 2) {
  result <- round(mean(mpg/hp), digits)
  return(result)
}
mpg_per_hp_digit_dflt(mtcars$mpg, mtcars$hp)      # defaults to 2 digits
mpg_per_hp_digit_dflt(mtcars$mpg, mtcars$hp, 4)   # override with 4 digits


## Task 

# 👉 Write a function called cyl_mean_mpg() that takes two arguments:
#   
#   the dataset (data)
# 
# the number of cylinders (cylinders)
# 
# and it should return the mean mpg for that cylinder group.

cyl_mean_mpg <- function(mtcars, cylinders) {
  result <- mean(mtcars$mpg[mtcars$cyl == cylinders])
  return(result)
}

cyl_mean_mpg(mtcars, 8) #a custom function that works like tapply, but only for the group

# cross check with tapply
tapply(mtcars$mpg, mtcars$cyl, mean)


## Task 

# We could make the function even more flexible by letting the user pick which column to average (not just mpg).
# 
# For example, they might want average hp or wt by cylinders.
# 
#👉 Want to try modifying your function so the user can pass a column name as an argument instead of being locked to mpg?


cyl_mean <- function(mtcars, cylinder, col) {
  result <- mean(mtcars[mtcars$cyl == cylinder, col])
  return(result)
}

cyl_mean(mtcars, 4, "mpg") # mean mpg for 4 cylinders
cyl_mean(mtcars, 6, "mpg")    # mean wt for 6 cylinders
cyl_mean(mtcars, 8, "mpg")    # mean wt for 8 cylinders
cyl_mean(mtcars, 8, "wt")    # mean wt for 8 cylinders
cyl_mean(mtcars, 4, "hp")    # mean horsepower for 4 cylinders
cyl_mean(mtcars, 6, "hp")    # mean horsepower for 6 cylinders
cyl_mean(mtcars, 8, "hp")    # mean horsepower for 8 cylinders

# cross check with tapply
tapply(mtcars$hp, mtcars$cyl, mean)


# build on your cyl_mean() idea. Right now it takes one variable (like "mpg" or "hp") and one cylinder group.
# 
# But we can make a function that:
#   
#   Loops through several variables (say mpg, hp, wt, etc.)
# 
# Returns the means for each variable within a given cylinder group.


cyl_summary <- function(mtcars, cylinder, var) {
  result <- sapply(var, function(v){
    mean(mtcars[[v]][mtcars$cyl == cylinder])
  })
  return(result)
}

cyl_summary(mtcars, 6, c("mpg", "hp"))

#a simple function that adds 1 to an input value:
add_one <- function(x) x+1

sapply(mtcars[1:4], min)
