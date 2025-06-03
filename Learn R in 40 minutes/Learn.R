
#Learn R in 40 minutes
#Link: https://www.youtube.com/watch?v=yZ0bV2Afkjc
#Limk; https://github.com/equitable-equations/youtube


install.packages("tidyverse")
library(tidyverse)


mean(Scooby$imdb, na.rm = T)
mean(Scooby$run_time)

#Check total miising value in IMDB
sum(is.na(Scooby$imdb))


data()
view(mpg)
?mpg
?mean

glimpse(mpg)
?filter
filter(mpg, cty >= 20)

#filter city which is > 20
mpg_efficient <- filter(mpg, cty >=20)
view(mpg_efficient)

#filter from Manufacturer with Ford and City > 11
mpg_ford <- filter(mpg, manufacturer == "ford", cty > 11)

#Create new column with city_matrix with multiple by 0.425144
mpg_matrix <- mutate(mpg, cty_matrix = 0.425144 * cty)

view(mpg_matrix)

##Create new column with city_matric with multiple by 0.425144 using pipe operator
mpg_matric <- mpg %>%
  mutate(cty_matric = 0.425144 * cty)
view(mpg_matric)

?round


#Summarise means of City group by with Class using pipe operator
mpg %>% 
  group_by(class) %>% 
  summarise(mean(cty))


#Data Viz

ggplot(mpg, aes(x = cty)) + 
  geom_bar() + 
  labs(x = "City Mileage")

#checked
?ggplot

Viz1 <- ggplot(mpg, aes(x = cty, y = hwy)) + 
  geom_point() + 
  geom_smooth(method = "lm")


ggplot(mpg, aes(x = cty, y = hwy, color = class)) + 
  geom_point()


Viz3 <- ggplot(mpg, aes(x = cty, y = hwy, color = class)) + 
  geom_point() +
  scale_color_brewer(palette = "Dark2")



View(mpg)
str(mpg)

mpg %>% 
  group_by(class, displ) %>% 
  summarise(mean_response = mean(cyl, na.rm = TRUE))
