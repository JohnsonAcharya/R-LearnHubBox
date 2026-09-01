
library(gtsummary)
library(gt)
library(dplyr)


data("trial")


head(trial)


glimpse(trial)


tbl_summary(trial)


trial |> 
  select(age, marker, stage, grade, response) |> 
  tbl_summary()


summary(trial)

trial %>%
  tbl_summary(
    statistic = list(
      age ~ "{median} ({p25}, {p75})",
      marker ~ "{mean} ({sd})"
    )
  )


## This is extremely useful because the labels become part of your table rather 
## than requiring manual editing afterward.

trial %>%
  tbl_summary(
    label = list(
      age ~ "Age (years)",
      marker ~ "Biomarker level",
      stage ~ "Disease stage"
    )
  )



## Suppose trt represents treatment.

trial |> 
  tbl_summary(
    by = trt
  )


## Add an overall column

trial |> 
  tbl_summary(by = trt) |> 
  add_overall()



## the table publication-friendly

table1 <-
  trial %>%
  tbl_summary(
    by = trt,
    label = list(
      age ~ "Age (years)",
      marker ~ "Biomarker level",
      stage ~ "Disease stage",
      grade ~ "Grade Level"
    )
  ) %>%
  add_overall() %>%
  add_p()

table1


## Add a title

table1 |> 
  modify_header(label ~ "**Baseline characteristics**")


## Bold important rows

table1 |> 
  bold_labels()


## Also bold significant p-values:

table1 |> 
  bold_p()


# combine them:

table1 |> 
  bold_labels() |> 
  bold_p()
  

## workflow 

table1 <-
  trial %>%
  tbl_summary(by = trt) %>%
  add_overall() %>%
  add_p() %>%
  bold_labels() %>%
  bold_p()
