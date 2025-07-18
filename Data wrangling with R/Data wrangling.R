###Data wrangling with R

library(tidyverse)


?diamonds
view(diamonds)
head(diamonds)

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
       aes(x = carat, y = cut, fill = cut))+
  geom_bar(stat="identity") +
  theme_minimal()



##  Practice - Grouped summarise by guroup() and summarize() with diamond dataset?

# check with reframe function instead summarize function as  
# Returning more (or less) than 1 row per `summarise()` group was deprecated
# in dplyr 

glimpse(diamonds)

diamond_pract<- diamonds %>% 
  group_by(cut) %>% 
  reframe(avg_cost = mean(price),
            sd_cost = sd(price),
            avg_carat = mean(carat),
            per_carat = price/carat)
diamond_pract


diamond_test <- diamonds %>% 
  group_by(clarity) %>% 
  summarise(per_car = price/carat)
diamond_test

--------------------------------------------------------------------------------
str(diamonds)
sum(is.na(diamonds))


head(diamonds)
summarise(diamonds)
colSums(is.na(diamonds))


mean(diamonds$price)




diamond_newCol <- diamonds %>% 
  mutate(price_per_carat = price/carat)
diamond_newCol
glimpse(diamond_newCol)



### Flag for Expensive Diamonds (e.g., price > $10,000)
diamonds_flag <- diamonds %>%
  mutate(expensive_flag = ifelse(price > 10000, "Expensive", "Affordable"))

table(diamonds_flag$expensive_flag)

glimpse(diamonds_flag)


##  Log Transformation to Handle Skewness

diamond_log <- diamonds %>% 
          mutate(Carat_log = log(carat),
                 Price_log = log(price))
diamond_log
summary(diamond_log$Price_log)


diamondInt <- diamonds %>% 
  mutate(carat_cut_interaction  = carat * x)
diamondInt


##  Create a new feature value_category:?

##     # Ensure price_per_carat exists
Diamond_Per <-  diamonds %>% 
          mutate(price_per_carat = price/carat)

Diamond_Per
glimpse(Diamond_Per)

###   # Create value_category
diamond_val <-  Diamond_Per %>% 
      mutate(value_category = case_when(
        price_per_carat > 6000 ~ "High Value",
        price_per_carat >= 3000 ~ "Mid Value",
        TRUE ~ "Low Value"
      ))
diamond_val
glimpse(diamond_val)
view(diamond_val)


#### Count each category

table(diamond_val$value_category)


### --- Bar Plot: value_category Distribution
ggplot(diamond_val, aes(x = value_category, fill = value_category)) +
  geom_bar() + 
  theme_minimal() +
  labs(title = "Distribution of Diamonds by Value Category", x = "Value Category", y = "Count") + 
  scale_fill_manual(values = c("Low Value" = "red", "Mid Value" = "orange", "High Value" = "green"))
 


## 2. Create is_square Based on Dimensions

Diamond_sqr <- diamonds %>% 
  mutate(is_square = abs(x - y) < 0.1)

# Calculate percentage of square diamonds

square_pct <- mean(Diamond_sqr$is_square, na.rm = T) * 100
square_pct


##-----Learn R inside RStudio-----
install.packages("swirl")
library(swirl)


### --- Practice day 1 - 2506


glimpse(diamonds)


diamond_dy1_prac <- diamonds %>% 
  mutate(diamond_clarity = clarity,
         diamond_price =  carat * price)
diamond_dy1_prac
view(diamond_dy1_prac)


diamond_dy1_prac <- diamonds %>% 
  group_by(cut) %>% 
  reframe(diamond_price =  carat * price)
diamond_dy1_prac


### --- Practice day 2 - 2606

## - How many rows and columns are there in the diamonds dataset?

dim(diamonds)


## - What is the average price of a diamond in the dataset?

mean(diamonds$price)

## - Filter the data to find diamonds with cut "Ideal" and carat > 1

diamond_filtr <- diamonds %>% 
  filter(cut == "Ideal",
         carat > 1)
diamond_filtr
head(diamond_filtr)


## - Create a summary table showing the average price for each cut type.

diam_summary <-  diamonds %>% 
  group_by(cut) %>% 
  reframe(Avg_price = mean(price))
diam_summary


## - Plot a histogram of the price variable

Dia_plot <- ggplot(diamonds, aes(x = price)) +
  geom_histogram(binwidth = 600, fill =  "green", color = "red")+
  labs(title = "Histogram of Diamond Price")

Dia_plot


### --- Practice day 3 - 3006


## Q. Find the most expensive diamond for each cut type.

Diamond_exp1 <- diamonds %>% 
  group_by(cut) %>% 
  slice_max(price,n = 1) %>% 
  select(cut, price, carat, color, clarity)
Diamond_exp1


### --- Practice day 4 - 090725


summary(Diamond_exp)
diamonds
data(diamond)

summary(diamonds$price)


###Bivariate Analysis
#Price vs Carat (Scatter)

ggplot(diamonds, aes(x = carat, y = price))+
  geom_point(alpha = 0.3, color = "blue")+
  labs(title = "Price vs Carat (Scatter)")

# Boxplot of Price by Cut

ggplot(diamonds, aes(x = cut, y = price))+
  geom_boxplot(fill = "blue")+
  labs(title = "Price Distribution by Cut")


#  Average Price by Clarity

diamonds %>% 
  group_by(clarity) %>% 
  summarise(avg_price = mean(price)) %>% 
  ggplot(aes(x = clarity, y = avg_price))+
  geom_col(fill = 'blue')+
  labs(title = "Average Price by Clarity")
