install.packages("tidyverse")
install.packages("gtsummary")
install.packages("here")
install.packages("broom")


library(tidyverse)
library(gtsummary)
library(broom)


load("C:/Users/Adaiah Soibi-Harry/Desktop/Fall 2023/Epi590R/Adaiah_Final_Project/Data/covid_testing.rda")
load(here::here("Data/covid_testing.rda"))

glimpse(covid_testing)
summary(covid_testing)

#table of descriptive statistics

tbl_summary(
  covid_testing,
  by = result,
  include = c(result, clinic_name, demo_group,
              patient_class, rec_ver_tat, col_rec_tat, age, gender),
  label = list(
    result ~ "Test result",
    clinic_name ~ "Clinic",
    demo_group ~ "Demographic group",
    patient_class ~ "Patient class",
    rec_ver_tat ~ "Result time",
    col_rec_tat ~ "Lab time",
    age ~ "Age", 
    gender ~ "Gender"
  ),
  missing_text = "Missing")

#Adding a new variable

covid_testing <- covid_testing |> mutate(gender_binary = ifelse(gender == "female", 1, 0))

#Fitting a logistic regression model and present well-formatted results from the regression 


logistic_model <- glm(gender_binary ~ result + rec_ver_tat + age,
                      data = covid_testing, family = binomial())


tbl_regression(
  logistic_model,
  exponentiate = TRUE,
  label = list(
    result ~ "Test result",
    rec_ver_tat ~ "Result time",
    age ~ "Age"
  ))


#Creating a histogram

hist(covid_testing$age)


#Write and use a function that does something with the data 

sd <- function(age) {
  sd_value <- sqrt(var(age))
  return (sd_value)}

sd(covid_testing$age)











