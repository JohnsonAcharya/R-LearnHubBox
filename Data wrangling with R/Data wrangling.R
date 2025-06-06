###Data wrangling with R

library(tidyverse)


?diamonds
view(diamonds)


#  Subset by row with filter()----------

?filter

#filer row from specific column by value or char
diamond_sm <- filter(diamonds, price > 1000)
diamond_sm

diamond_sm <- filter(diamonds, cut == "Ideal")
diamond_sm


diamond_sm <- filter(diamonds,
                     cut == "Ideal",
                     price >10000)
diamond_sm

#Checks missing value
diamond_sm <- is.na(diamonds)

# --------------------------------------------------------------------------------
# Subset by column with select()---------------

?select

# Select specific column from datasets
diamond_sm <- select(diamonds, color,cut) 
diamond_sm

# Select column from start 1 to 4 from dataset
diamond_sm <- select(diamonds, 1:4)
diamond_sm


# Select column which column names starts with C
diamond_sm <- select(diamonds, starts_with("c"))
diamond_sm

# Select column which column names contain any specific latter  "C"
diamond_sm <- select(diamonds, contains("t"))
diamond_sm


# Select a column you want as first column with all columns from datasets
diamond_sm <- select(diamonds, price, everything())
diamond_sm

#Remove specific column from dataset using - before coumn name
diamond_sm <- select(diamonds, -price)
diamond_sm
#--------------------------------------------------------------------------------