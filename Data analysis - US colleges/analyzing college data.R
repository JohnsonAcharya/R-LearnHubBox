library(tidyverse)
install.packages("ISLR2")
library(ISLR2)

# Do colleges with larger full-time enrollments
# have lower graduation rates?
# Is this different for public/private institutions?
# If it different for more selective schools?


?College
glimpse(College)

view(College)

#Exploratory graphics

ggplot(College, aes(x = Grad.Rate)) +
  geom_histogram()  # Note wierd outlier found

Suspecious <- filter(College, Grad.Rate >= 100)
Suspecious


ggplot(College, aes(x = F.Undergrad, y =  Grad.Rate )) +
  geom_point()

##Check With log10
ggplot(College, aes(x = log10(F.Undergrad), y =  Grad.Rate )) +
  geom_point()


college_sm <- College %>%
  mutate(log_full = log10(F.Undergrad)) %>%
  select(Grad.Rate,
         log_full,
         Private,
         Top25perc)
view(college_sm)

####-- Modeling---
##Check With log10 and added smooth line
ggplot(College, aes(x = log10(F.Undergrad), 
                    y =  Grad.Rate )) +
  geom_point()+
  geom_smooth(method = "lm")

