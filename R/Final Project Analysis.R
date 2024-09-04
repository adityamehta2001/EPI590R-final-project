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

#Cleaning variable names to reflect R formatting with underscores
data <- janitor::clean_names(data)

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
  missing_text = "Missing") |>
  bold_labels() |>
  modify_header(label="**Variable**")

print(Table_1)
COVID_Deaths_Hispanic <- inline_text(Table_1, variable = covid_19_deaths , column = "stat_1")
Total_Deaths_Hispanic <- inline_text(Table_1, variable = total_deaths , column = "stat_1")

#Defining variables as factors for use in linear regression model
data$race_or_hispanic_origin <- as.factor(data$race_or_hispanic_origin)
data$sex <- as.factor(data$sex)
data$education_level <- as.factor(data$education_level)
data$age_group <- as.factor(data$age_group)

#Poisson regression table since using discrete COVID mortality counts
poisson_model <- glm(covid_19_deaths ~ race_or_hispanic_origin +
                   sex + education_level + age_group, 
                   data = data, family=poisson())

poisson_model_2 <- glm(total_deaths ~ race_or_hispanic_origin +
                         sex + education_level + age_group, 
                       data = data, family=poisson())

#Created new function for Poisson model creation 
new_table_function <- function(model){
  reg_tbl <- tbl_regression(
    model,
    intercept = FALSE,
    label = list(
      race_or_hispanic_origin ~ "Race or Hispanic Origin",
      sex ~ "Sex",
      education_level ~ "Education Level",
      age_group ~ "Age Group"
    )
  )|>
    bold_labels() |>
    modify_header(label="**Variable**")
  return(reg_tbl)
}

new_table_function(model = poisson_model)

#Testing function on different Poisson model that uses total death data
new_table_function(model = poisson_model_2)

#Creating a histogram of residuals as my figure using hist function.

residuals <- residuals(linear_model)
hist(residuals, main = "Histogram of Residuals", xlab = "Residuals", col = "turquoise",
     border = "black")

#second use of {here} package to save the figure as PNG.
png(filename = here::here("documents", "residual_histogram.png"))

hist(residuals, main = "Histogram of Residuals", xlab = "Residuals", col = "turquoise",
     border = "black")

invisible(dev.off())
