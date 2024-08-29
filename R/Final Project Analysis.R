#Loading COVID-19 death count data by week ending date and state, downloaded from CDC
install.packages("here")
install.packages("dplyr")
install.packages("gtsummary")

library(here)
library(tidyverse)
library(dplyr)
library(gtsummary)
library(janitor)

data <- read_csv(here::here("data/raw/COVID_Data.csv"))

#Table of descriptive statistics with categorical variables
Table_1 <- tbl_summary(
  data, 
  by = race_or_hispanic_origin, 
  include= c(ends_with("Deaths"), sex, education_level, age_group), 
  label = list (
    education_level ~ "Education Level",
    age_group ~ "Age Group",
    covid_19_deaths ~ "COVID-19 Deaths",
    total_deaths ~ "Total Deaths",
    sex ~ "Sex"
  ),
  missing_text = "Missing")
print(Table_1)

# Check levels of each factor variable
levels(as.factor(data$race_or_hispanic_origin))
levels(data$sex)
levels(as.factor(data[[education_level]]))
levels(as.factor(data[[age_group]]))

data$race_or_hispanic_origin <- as.factor(data$race_or_hispanic_origin)
data$sex <- as.factor(data$sex)
data$education_level <- as.factor(data$education_level)
data$age_group <- as.factor(data$age_group)


#Linear regression table since using COVID mortality counts
linear_model <- lm(covid_19_deaths ~ race_or_hispanic_origin +
                   sex + education_level + age_group, 
                   data = data)

new_table_function <- function(model){
  tbl_regression(
    linear_model,
    intercept = FALSE,
    label = list(
      race_or_hispanic_origin ~ "Race or Hispanic Origin",
      sex ~ "Sex",
      education_level ~ "Education Level",
      age_group ~ "Age Group"
    )
  )
}

new_table_function(linear_model)



tbl_uvregression(
  data, 
  y = covid_19_deaths,
  include = c(Race, Sex, education_level, age_group),
  method = glm,
  method.args = list(family = binomial()),
  exponentiate = TRUE)



tbl_regression(
  poission_model, 
  intercept = TRUE,
)