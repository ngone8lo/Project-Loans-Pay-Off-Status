## @knitr eda

##Ngoné Lo
## March 2020

#### Set up workspace ###
#Importing libraries
library(tidyverse)
library(janitor) # Helps with initial data cleaning and pretty tables

#Loading dataset
loan_data <- read_csv("outputs/datasets/loan_data_cleaned.csv")

#First we take a look at the prediction variable: loan status
#Overview of the variable term
status_prop <- loan_data %>% 
  tabyl(loan_status) %>% 
  adorn_pct_formatting()

#### Save term_proportion table####
write_csv(status_prop, "outputs/tables/loan_status.csv")

#The data is highly biased toward Fully Paid Loan Status



#Second, we take a look at credit score

#Summary statistics of the variable credit score
summary(loan_data$credit_score)

#Histogram of credit score
ggplot(data = loan_data, mapping = aes(x=credit_score)) + 
  geom_histogram(binwidth=10, color="black", fill="ivory2" )+ #plot histogram
  #annotate summary statistics to plot
  annotate("text", label = "min=585", x = 590, y = 11000, color = "gray35",
           hjust = 0, size=4)+ 
  annotate("text", label = "1st Quartile=706", x = 590, y = 9800, color = "gray35",
           hjust = 0, size=4)+
  annotate("text", label = "mean=724", x = 590, y = 8600, color = "gray35",
           hjust = 0, size=4)+ 
  annotate("text", label = "median=718", x = 590, y = 7400, color = "gray35",
           hjust = 0, size=4)+
  annotate("text", label = "3rd Quartile=739", x = 590, y = 6200, color = "gray35",
           hjust = 0, size=4)+
  annotate("text", label = "max=751", x = 590, y = 5000, color = "gray35",
           hjust = 0, size=4)+
  annotate("text", label = "sd=27", x = 590, y = 3800, color = "gray35",
           hjust = 0, size=4)+
  theme_minimal() + # Make the theme neater
  #Define title, subtile, and axis size
  theme(plot.title =element_text(size = 16),
        plot.subtitle =element_text(size = 16),
        axis.title = element_text(size=16))+ 
  #Define title, subtitle, and axis labels
  labs(title= "Figure 1: Distribution of Credit Score",
       subtitle="With Summary Statistics",
       x="Credit Score",
       y="Count")

#### Save the graph ####
ggsave("outputs/figures/credit_score_distribution.png",
       width = 15, height = 10, units = "cm")


#Third, we take a look at annual income

#Summary statistics of the variable annual_income
summary(loan_data$annual_income)

#Looks like we are dealing with millionaires here

#Histogram of annual income
ggplot(loan_data) + 
  #plot histogram
  geom_histogram(aes(x=annual_income), binwidth=500000, 
                 color="black", fill="ivory2")+
  #annotate summary statistics to plot
  annotate("text", label = "min=76,627", x = 4000000, y = 18000, 
           color = "gray35", hjust = 0, size=4)+ 
  annotate("text", label = "1st Quartile=871,976", x = 4000000, y = 16000, 
           color = "gray35", hjust = 0, size=4)+
  annotate("text", label = "median=1,213,264", x = 4000000, y = 14000, 
           color = "gray35", hjust = 0, size=4)+ 
  annotate("text", label = "mean=1,401,084", x = 4000000, y = 12000, 
           color = "gray35", hjust = 0, size=4)+
  annotate("text", label = "3rd Quartile=1,685,053", x = 4000000, y = 10000, 
           color = "gray35", hjust = 0, size=4)+
  annotate("text", label = "max=165,557,393", x = 4000000, y = 8000, 
           color = "gray35", hjust = 0, size=4)+
  annotate("text", label = "sd=1,145,882", x = 4000000, y = 6000, 
           color = "gray35", hjust = 0, size=4)+
  theme_minimal() + # Make the theme neater
  #Define title, subtitle, and axis size
  theme(plot.title =element_text(size = 16), 
        plot.subtitle =element_text(size = 16),
        axis.title = element_text(size=16))+ 
  #Define title, subtitle, and axis labels
  labs(title= "Figure 2: Distribution of Annual Income",
       subtitle= "With Summary Statistics",
       x="Annual Income",
       y="Count") + 
  xlim(c(0, 7500000)) #set x axis limit

#### Save the graph ####
ggsave("outputs/figures/annual_income_distribution.png",
       width = 15, height = 10, units = "cm")


#The other variables are categorical and are:
# 1 Term of Loan (Long term, Short Term) and
# 2. Years in Current Job (> 1 year, 1 Year, 2 years,..., 10+ years )

#Overview of the variable term
term_prop <- loan_data %>% 
  tabyl(term) %>% 
  adorn_pct_formatting()

#### Save term_proportion table####
write_csv(term_prop, "outputs/tables/term_proportion.csv")

#Re-leveling years in current job
loan_data$years_in_current_job<-factor(loan_data$years_in_current_job,
                                 levels=c("< 1 year", "1 year", "2 years", "3 years",
                                          "4 years", "5 years", "6 years", "7 years",
                                          "8 years", "9 years", "10+ years",))
#Overview of the variable years in current in job
current_job_years_prop <- loan_data %>% 
  tabyl(years_in_current_job) %>% 
  adorn_pct_formatting()

#### Save term_proportion table####
write_csv(current_job_years_prop, "outputs/tables/current_job_years_proportion.csv")

#Because of skewdness reasons and to align more with real world practices
#where people are often assigned to a income bracket, we decided to categorize
#credit score and annual income.

#Categorizing credit score
loan_data <- loan_data %>%
  mutate(credit_score_category = case_when(
    credit_score < 620 ~ "Poor",
    credit_score >= 620 & credit_score < 690  ~ "Average",
    credit_score >= 690 & credit_score < 720  ~ "Good",
    credit_score >= 720  ~ "Very Good"), credit_score = NULL)

#Overview of the grouped credit_score
table(loan_data$credit_score_category)


#Categorizing annual_income
loan_data <- loan_data %>%
  mutate(income_bracket = case_when(
    annual_income <500000 ~ "<500K",
    annual_income >= 500000 & annual_income < 1000000  ~ "500K-1M",
    annual_income >= 1000000 & annual_income < 1500000  ~ "1M-1.5M",
    annual_income >= 1500000 & annual_income < 2000000  ~ "1.5M-2M",
    annual_income >= 2000000 ~ "2M+"), annual_income = NULL)

#Overview of the grouped income
table(loan_data$income_bracket)


#For simplicity reasons, we re-grouped years in current job
#in 3 groups instead of 11 groups

#Categorizing job stability based on years_in_current_job
#Store possible categories in vectors
low <- c("< 1 year", "1 year", "2 years", "3 years")
medium <- c("4 years", "5 years", "6 years", "7 years")
high <- c("8 years", "9 years", "10+ years")

#Create job_stability column by grouping years_in_current_job:
loan_data <- loan_data %>%
  mutate(job_stability = case_when(
    years_in_current_job %in% low ~ "Low", #Assign Low job stability
    years_in_current_job %in% medium ~ "Medium",  #Assign Medium job stability
    years_in_current_job %in% high ~ "High"  #Assign High job stability
  ), years_in_current_job=NULL)

#Overview of the grouped job_stability
table(loan_data$job_stability)


#We suspect credit score and term of loan to be
#the best predictors of loan status
#Re-leveling credit score
loan_data$credit_score_category<-factor(
  loan_data$credit_score_category,levels=
    c("Poor", "Average", "Good", "Very Good"))

loan_summary1 <- loan_data %>%
  group_by(credit_score_category, term, loan_status) %>%
  summarise(n = n()) %>% # Count the number in each group and response
  group_by(credit_score_category, term) %>%
  mutate(prop = n/sum(n)) # Calculate proportions within each group

ggplot(loan_summary1) +
  #Specify a barplot of loan status
  geom_col(aes(x = loan_status, y = prop, fill=loan_status)) + 
  #Facet by credit category score and term
  facet_grid(credit_score_category~term) + 
  theme_minimal()+  #Make the theme neater
  #Define size of title, axis and legend
  theme(plot.title =element_text(size = 16),
        axis.title.y = element_text(size = 16),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14),
        strip.text = element_text(size = 9))+ 
  #Define title, y_axis, and legend labels
  labs(title= "Fgure 3: Loan Status by Credit Score Category
               and Loan Term",
       y="Proportion", fill="Loan Status")+ 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank() #Delete x_labels
        )


#### Save the graph ####
ggsave("outputs/figures/loan_status_by_credit_score_term.png",
       width = 15, height = 10, units = "cm")

#Loan status vary with both credit score category and term loan 


#Next we look at job stability and income bracket
#Re-leveling job stability
loan_data$job_stability<-factor(loan_data$job_stability,
                                levels=c("Low", "Medium", "High"))
#Re-leveling income bracket
loan_data$income_bracket<-factor(loan_data$income_bracket,
                                 levels=c("<500K", "500K-1M", "1M-1.5M",
                                          "1.5M-2M", "2M+"))

#Group by job stability  and income bracket. Calculate proportions
loan_summary2 <- loan_data %>%
  group_by(job_stability, income_bracket, loan_status) %>%
  summarise(n = n()) %>% # Count the number in each group and response
  group_by(job_stability, income_bracket) %>%
  mutate(prop = n/sum(n)) # Calculate proportions within each group

ggplot(loan_summary2) +
  #Specify a barplot of loan status
  geom_col(aes(x = loan_status, y = prop, fill=loan_status)) + 
  #Facet by job stability and income bracket
  facet_grid(job_stability~income_bracket) + 
  theme_minimal()+  #Make the theme neater
  #Define size of title, axix and legend
  theme(plot.title =element_text(size = 16),
        axis.title.y = element_text(size = 16),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14),
        strip.text = element_text(size = 10))+ 
  #Define title, y_axis, and legend labels
  labs(title= "Figure 4: Loan Status by Job Stability
               and Income Bracket",
       y="Proportion", fill="Loan Status")+ 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank() #Delete x_labels
  )

#### Save the graph ####
ggsave("outputs/figures/loan_status_by_job_stability_income.png",
       width = 15, height = 10, units = "cm")

#While income bracket seems to cause significant difference, job stability
#does not. Hence credit score category, term loan, and income bracket 
#will be used as our predictors in our logistic model


#Select columns/variables of interest
loan_data <- loan_data %>%
  select(loan_status, credit_score_category, term, income_bracket)

#### Save selected data for model####
write_csv(loan_data, "outputs/datasets/loan_data_selected.csv")

