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

mat <- matrix(1:9, nrow = 3, byrow = T)
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




