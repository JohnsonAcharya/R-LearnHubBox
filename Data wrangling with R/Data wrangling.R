###Data wrangling with R

library(tidyverse)


?diamonds
view(diamonds)


#  Subset by row with filter()----------

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

