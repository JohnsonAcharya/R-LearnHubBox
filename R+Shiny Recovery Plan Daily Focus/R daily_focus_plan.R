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

