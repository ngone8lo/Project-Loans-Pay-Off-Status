## @knitr cleaning_data

##Ngoné Lo
## March 2020

#### Set up workspace ###
#Importing libraries
library(janitor) # Helps with initial data cleaning and pretty tables
library(skimr) # Helps with initial data visualisation
library(tidyverse)

#Loading dataset
loan_data <- read_csv("inputs/credit_train.csv")

#Cleaning the variables' names using Janitor
loan_data <- clean_names(loan_data) 

#Overview
skim(loan_data)


#Drop NA rows for annual_income
loan_data<- loan_data %>% drop_na(annual_income)

#Calculate annual debt_to_income
loan_data$debt_to_income=
  (loan_data$monthly_debt*12)/loan_data$annual_income

loan_data<- loan_data %>% 
  #Filter to only have population of interest
  #Low risk population with debt consolidation as purpose for loan
  filter(debt_to_income <= 0.35 &
           purpose == "Debt Consolidation" &
           number_of_credit_problems == 0 &
           bankruptcies == 0 &
           tax_liens == 0)

#Select columns/variables of interest
loan_data <- loan_data %>%
  select(loan_status, credit_score, years_in_current_job, 
         term, annual_income)

#Overview of data with selected variables
skim(loan_data)

#Filter out n/a rows in years_in_current_job
loan_data<- loan_data %>% 
  filter(years_in_current_job!="n/a")


#There are credit score values bigger than the maximum of 850.
#Let's get an overview of the unique values to get an idea of the situation 
unique(loan_data$credit_score[loan_data$credit_score>850])

#Looks like there was an additional zero added for the credit score values
#greater than 850. We divide the credit score values greater than
#850 by 10 o get them back to normal range
loan_data <- loan_data %>%
  mutate(credit_score = case_when(
    credit_score>850 ~ credit_score/10,
    credit_score<=850 ~ credit_score))

#Overview of the variable credit score
skim(loan_data$credit_score)


#The final cleaned dataset has 51903 observations for 5 variables


#### Save cleaned dataset ####
write_csv(loan_data, "outputs/datasets/loan_data_cleaned.csv")

