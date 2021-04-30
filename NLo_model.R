## @knitr model

##Ngoné Lo
## March 2020

#### Set up workspace ###
#Importing libraries
library(broom) # Helps with model outputs
library(tidymodels) # Help with modelling
library(caret) #Helps with classification regression models and cross validation
library(tidyverse) 

#Loading dataset
loan_data <- read_csv("outputs/datasets/loan_data_selected.csv")

#Re-leveling income bracket
loan_data$income_bracket<-factor(loan_data$income_bracket,
                                 levels=c("<500K", "500K-1M", "1M-1.5M",
                                          "1.5M-2M", "2M+"))

loan_data <- loan_data %>%
  mutate(credit_score_category_encoded = case_when(
    credit_score_category == "Poor" ~ 0,
    credit_score_category == "Average" ~ 1,
    credit_score_category == "Good" ~ 2,
    credit_score_category == "Very Good" ~ 3
  ))

#convert loan term to boolean
loan_data <- loan_data %>%
  mutate(long_term_loan = case_when(
    term == "Long Term" ~ TRUE,
    term == "Short Term" ~ FALSE
  ))

#encode income bracket to numeric
loan_data <- loan_data %>%
  mutate(income_bracket_encoded = case_when(
    income_bracket == "<500K" ~ 0,
    income_bracket == "500K-1M" ~ 1,
    income_bracket == "1M-1.5M" ~ 2,
    income_bracket == "1.5M-2M" ~ 3,
    income_bracket == "2M+" ~ 4
  ))

#convert loan status to binary
loan_data <- loan_data %>%
  mutate(loan_paid = case_when(
    loan_status == "Fully Paid" ~ 1,
    loan_status == "Charged Off" ~ 0
  ))


#Set seed for reproducibility
set.seed(1203)

# Split the data into test/training sets
loan_data_split <- 
  loan_data %>%
  initial_split(prop = 3/4)  

loan_train <- training(loan_data_split)
loan_test <- testing(loan_data_split)

rm(loan_data_split)


#### Model ####
#predict loan_paid using credit score, income_bracket, long_term
model <- glm(loan_paid ~ 
               credit_score_category_encoded + income_bracket_encoded +
               long_term_loan, 
             data = loan_train, family="binomial")


# model output: coeffcicients
coeff_output<-tidy(model)
coeff_output
#### Save coeff_output table####
write_csv(coeff_output, "outputs/tables/coeff_output.csv")


#Look at what the model predicts, compared with the actual
#Cut-off probability = 0.7
loan_model1_fit_train <- 
  augment(model, 
          data = loan_train,
          type.predict = "response") %>% # 
  select(-.hat, -.sigma, -.cooksd, -.std.resid) %>% 
  mutate(predict_loan_paid = if_else(.fitted > 0.7, 1, 0))


#How many loans were predicted as charged off (not paid) "0" and
#how many were predicted as Fully paid "1" in the training set
table(loan_model1_fit_train$predict_loan_paid)


#Look at the distribution of how far off the model is
loan_model1_fit_train %>% 
  ggplot(aes(x = .fitted, fill = loan_status)) +
  #Specify a histogram of loan status
  geom_histogram(binwidth = 0.02, position = "dodge") +
  theme_minimal()+  #Make the theme neater
  #Define size of title, axix and legend
  theme(plot.title =element_text(size = 14),
        axis.title = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14))+
  #Define title, y_axis, and legend labels
  labs(title = "Estimated Probability of Loan being Repaid vs.
               True Status of Loans",
       x = "Estimated probability that loan is repaid",
       y = "Count",
       fill = "Loan Status") +
  #Choose color palette
  scale_fill_brewer(palette = "Set1")



#### Save the graph ####
ggsave("outputs/figures/model_distribution.png",
       width = 15, height = 10, units = "cm")



#How the model probabilities change based on credit score, income bracket,
#and term of loan
ggplot(loan_model1_fit_train,
       aes(x = credit_score_category_encoded,
           y = .fitted, 
           color = income_bracket)) +
  geom_line() +
  geom_point() +
  facet_wrap(term~.)+ #Facet by loan term
  theme_minimal()+  #Make the theme neater
  #Define size of title, axix and legend
  theme(plot.title =element_text(size = 14),
        axis.title = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        strip.text = element_text(size = 12))+ 
  #Define title, y_axis, and legend labels
  labs(title = "Figure 6: Probability of Loan Being Repaid based on Credit
               Score Group, Term of Loan, and Income Bracket",
       x = "Credit Score Group",
       y = "Predicted probability that loan is repaid",
       color = "Income Bracket") +
  #choose color palette
  scale_color_brewer(palette = "Set1")


#### Save the graph ####
ggsave("outputs/figures/predictors_model_change.png",
       width = 15, height = 10, units = "cm")



#confusion matrix of training set to compare prediction to actual values
confusionMatrix(data = as.factor(loan_model1_fit_train$predict_loan_paid),
                reference = as.factor(loan_model1_fit_train$loan_paid),
                mode="prec_recall")



# adding the test to analysis: fit test set
loan_model2_fit_test <- 
  augment(model,
          newdata = loan_test,
          type.predict = "response") %>%  
  mutate(predict_loan_paid = if_else(.fitted > 0.7, 1, 0))


#confusion matrix of test set to compare predictions to actual values
confusion_matrix_test <- confusionMatrix(data = as.factor(
  loan_model2_fit_test$predict_loan_paid),
  reference = as.factor(loan_model2_fit_test$loan_paid), mode="prec_recall")

confusion_matrix_test

#charged off class performance metrics
performance_test<-tidy(confusion_matrix_test, mode="prec_recall")
#### Save performance_test####
write.csv(performance_test, "outputs/tables/performance_class0.csv")

#accuracy metrics
accuracy_test <- as.data.frame(as.matrix(confusion_matrix_test, what="overall"))
#### Save coeff_accuracy_metrics####
write.csv(accuracy_test, "outputs/tables/accuracy_metrics.csv")

#confusion matrix
confusion_matrix<-as.table(confusion_matrix_test)
#### Save confusion_matrix####
write.csv(confusion_matrix, "outputs/tables/confusion_matrix.csv", row.names = TRUE)



#compare the test with the training sets in terms of forecasts.
#select required columns for the training and test graphs
training <- loan_model1_fit_train %>% 
  select(loan_status, .fitted) %>% 
  mutate(type = "Training set")

test <- loan_model2_fit_test %>% 
  select(loan_status, .fitted) %>% 
  mutate(type = "Test set")

#combine training and test in one set and remove them afaterwards
both <- rbind(training, test)
rm(training, test)

#Look at the distribution of how far off the model is for 
#both training and test sets
both %>% 
  ggplot(aes(x = .fitted, fill = loan_status)) +
  #Specify a histogram of loan status
  geom_histogram(binwidth = 0.02, position = "dodge") +
  theme_minimal()+  #Make the theme neater
  #Define size of title, axix and legend
  theme(plot.title =element_text(size = 14),
        axis.title = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        strip.text = element_text(size = 12))+ 
  #Define title, y_axis, and legend labels
  labs(title = "Figure 5: Estimated Probability of Loan being Repaid vs.
               True Status of Loan",
       x = "Estimated probability that loan is repaid",
       y = "Count",
       fill = "Loan Status") +
  #Choose color palette
  scale_fill_brewer(palette = "Set1") +
  #facet by training and test type and free/independent y_axis
  facet_wrap(type~.,
             nrow = 2,
             scales = "free_y")

#### Save the graph ####
ggsave("outputs/figures/model_distribution_training_test.png",
       width = 15, height = 10, units = "cm")



#Cross Validation
# Define train control for k fold cross validation
train_control <- trainControl(method="cv", number=10)
# Fit logistic regression model on training set
model_cross <- train(loan_status ~ 
                       credit_score_category_encoded + income_bracket_encoded +
                       long_term_loan, 
                     data = loan_train, 
                     trControl=train_control,
                     method= "glm",
                     family="binomial")

cross_val<-model_cross$results

#cross validation accuracy metrics
cross_val_accuracy <- as.data.frame(as.matrix(cross_val, what="overall"))
#### Save coeff_accuracy_metrics####
write.csv(cross_val_accuracy, "outputs/tables/cross_val_metrics.csv")
