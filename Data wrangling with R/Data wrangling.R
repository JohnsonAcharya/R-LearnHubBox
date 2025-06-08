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

## reorder row with arrange() function

diamond_arr <- diamonds %>%
           arrange(color)
diamond_arr

#arrange by two criteria
diamond_arr <- diamonds %>%
  arrange(color, carat)
diamond_arr

#arrange by descending order
diamond_arr <- diamonds %>% 
            arrange(desc(carat))
diamond_arr
#--------------------------------------------------------------------------------

## Add or modify columns by mutate function

diamond_new <- diamonds %>% 
           mutate(mass.g = 0.20 * carat)
diamond_new
glimpse(diamond_new)


#create two columns with added price per carat column
diamond_new <- diamonds %>% 
         mutate(maas.g  = 0.20 * carat,
                price_per_carat = price / carat)
diamond_new


#create 3 columns with chamge cut column obs as lower case
diamond_new <- diamonds %>% 
  mutate(maas.g  = 0.20 * carat,
         price_per_carat = price / carat,
         cut = tolower(cut))
diamond_new
glimpse(diamond_new)


#added additional column expensive_tf to check price is greater than 1000
diamond_new <- diamonds %>% 
  mutate(maas.g  = 0.20 * carat,
         price_per_carat = price / carat,
         cut = tolower(cut),
         expensive_tf = price > 1000)
diamond_new
glimpse(diamond_new)
#--------------------------------------------------------------------------------

?slice_max
?bind_row
?left_join
?rename
?case_when


##  Grouped summarise by guroup() and summarize()

diamond_grp <- diamonds %>% 
          group_by(cut) %>% 
          summarise(mean(price))
diamond_grp



diamond_grp <- diamonds %>% 
            group_by(cut) %>% 
            summarise(avg_price =  mean(price),
                      sd_price = sd(price))
diamond_grp

## group by more than 1 thing 
diamond_grp <- diamonds %>% 
  group_by(cut, color) %>% 
  summarise(avg_price =  mean(price),
            sd_price = sd(price))
diamond_grp




## count by cut and color
diamond_cnt <- diamonds %>% 
            count(cut, color)
diamond_cnt


## aggregate function is used to calculate the mean of the value column


# Sample data frame
data <- data.frame(
  group = c("A", "A", "B", "B", "A", "B"),
  value = c(1, 2, 3, 4, 5, 6)
)

# Calculate the mean of 'value' for each group 'group'
result <- aggregate(value ~ group, data = data, FUN = mean)

# Print the result
print(result)

## ggplot bar chart with theme

ggplot(diamonds,
       aes(x = cut))+
  geom_bar() +
  theme_minimal()
