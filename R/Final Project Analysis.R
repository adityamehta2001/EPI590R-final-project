#Loading COVID-19 death count data by week ending date and state, downloaded from CDC
install.packages("here")
install.packages("dplyr")
install.packages("gtsummary")

library(here)
library(tidyverse)
library(dplyr)
library(gtsummary)

dat <- read_csv(here::here("data/raw/data.csv"))

label = list (
  Total_Deaths <- "Total Deaths",
  COVID_19_Deaths <- "COVID-19 Deaths",
  Influenza_Deaths <- "Influenza Deaths",
  Pneumonia_Deaths <- "Pneumonia Deaths",
  Pneumonia_and_COVID_19_Deaths <- "Pneumonia and COVID-19 Deaths",
  Pneumonia_Influenza_or_COVID_19_Deaths <- "Pneumonia, Influenza, or COVID-19 Deaths"
)

#Table of descriptive statistics
tbl_summary(dat, by = State, include= c(Total_Deaths,
                                                   ends_with ("Deaths")), 
                       missing_text = "Missing")
#54 states total included, since United States, District of Columbia, Puerto Rico, and NYC all counted as well
var(dat)

#Poisson regression table since using discrete mortality counts
poisson_model <- glm(Total_Deaths ~ COVID_19_Deaths + Pneumonia_Deaths + 
                       Influenza_Deaths + Pneumonia_and_COVID_19_Deaths +
                       Pneumonia_Influenza_or_COVID_19_Deaths, 
                      data = dat, family = poisson()
                     )

tbl_regression(
  poission_model, 
  intercept = TRUE,
  )