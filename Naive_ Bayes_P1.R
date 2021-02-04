

###### Prepare a classification model using Naive Bayes for salary data ########

## Read the salary train & test data frame 
# Load "SalaryData_Train" dataset  
salary_train <- read.csv("File Path",stringsAsFactors = FALSE)

# Load "SalaryData_Test" dataset 
salary_test <- read.csv("File Path",stringsAsFactors = FALSE)

# Checking the class imbalance 
table(salary_train$Salary)
table(salary_test$Salary)

## Convert all the character variables to factor
# For training data 
salary_train$workclass <- as.factor(salary_train$workclass)
salary_train$education <- as.factor(salary_train$education)
salary_train$maritalstatus <- as.factor(salary_train$maritalstatus)
salary_train$occupation <- as.factor(salary_train$occupation)
salary_train$relationship <- as.factor(salary_train$relationship)
salary_train$race <- as.factor(salary_train$race)
salary_train$sex <- as.factor(salary_train$sex)
salary_train$native <- as.factor(salary_train$native)
salary_train$Salary <- as.factor(salary_train$Salary)


# For testing data
salary_test$workclass <- as.factor(salary_test$workclass)
salary_test$education <- as.factor(salary_test$education)
salary_test$maritalstatus <- as.factor(salary_test$maritalstatus)
salary_test$occupation <- as.factor(salary_test$occupation)
salary_test$relationship <- as.factor(salary_test$relationship )
salary_test$race <- as.factor(salary_test$race)
salary_test$sex <- as.factor(salary_test$sex)
salary_test$native <- as.factor(salary_test$native)
salary_test$Salary <- as.factor(salary_test$Salary)

## EDA 
# Combine both data 
salary <- rbind(salary_train,salary_test)
# Convert all the factor varoab;e into numeric 
salary$workclass <- as.numeric(factor(salary$workclass))
salary$education <- as.numeric(factor(salary$education))
salary$maritalstatus <- as.numeric(factor(salary$maritalstatus))
salary$occupation <- as.numeric(factor(salary$occupation))
salary$relationship <- as.numeric(factor(salary$relationship ))
salary$race <- as.numeric(factor(salary$race))
salary$sex <- as.numeric(factor(salary$sex))
salary$native <- as.numeric(factor(salary$native))
salary$Salary <- as.numeric(factor(salary$Salary))
# Cheking the correlation
cor(salary)

## Summary of all continuous features 
summary(salary$age)
summary(salary$workclass)
summary(salary$education)
summary(salary$educationno)
summary(salary$maritalstatus)
summary(salary$occupation)
summary(salary$relationship)
summary(salary$race)
summary(salary$sex)
summary(salary$capitalgain)
summary(salary$capitalloss)
summary(salary$hoursperweek)
summary(salary$native)
summary(salary$Salary)


# Set the environment for automated EDA
install.packages("DataExplorer")
library(DataExplorer)
install.packages("tidyverse")
library(tidyverse)


## Visualiation of all features & report generation of them 
plot_intro(salary)
plot_missing(salary)

# Histogram
DataExplorer::plot_histogram(salary)
# Histogram for transformed variable featuers 
DataExplorer::plot_histogram(log(salary))

# Density plot
plot_density(salary)

# Correlation between features 
plot_correlation(salary, cor_args = list( 'use' = 'complete.obs'))

# Box plots by indivisual feature based against all features 
plot_boxplot(salary, by= 'age',            ncol=2)
plot_boxplot(salary, by= 'workclass',      ncol=2)
plot_boxplot(salary, by=  'education',     ncol=2)
plot_boxplot(salary, by=  'educationno',   ncol=2)
plot_boxplot(salary, by=  'maritalstatus', ncol=2)
plot_boxplot(salary, by=  'occupation',    ncol=2)
plot_boxplot(salary, by=  'relationship',  ncol=2)
plot_boxplot(salary, by=  'race',          ncol=2)
plot_boxplot(salary, by=  'sex',           ncol=2)
plot_boxplot(salary, by=  'capitalgain',   ncol=2)
plot_boxplot(salary, by=  'capitalloss',   ncol=2)
plot_boxplot(salary, by=  'hoursperweek',  ncol=2)
plot_boxplot(salary, by=  'native',        ncol=2)
plot_boxplot(salary, by=  'Salary',        ncol=2)


# Scatter plots by indivisual feature based against all features 
plot_scatterplot(salary, by=  'age',           ncol=2)
plot_scatterplot(salary, by=  'workclass',     ncol=2)
plot_scatterplot(salary, by=  'education',     ncol=2)
plot_scatterplot(salary, by=  'educationno',   ncol=2)
plot_scatterplot(salary, by=  'maritalstatus', ncol=2)
plot_scatterplot(salary, by=  'occupation',    ncol=2)
plot_scatterplot(salary, by=  'relationship',  ncol=2)
plot_scatterplot(salary, by=  'race',          ncol=2)
plot_scatterplot(salary, by=  'sex',           ncol=2)
plot_scatterplot(salary, by=  'capitalgain',   ncol=2)
plot_scatterplot(salary, by=  'capitalloss',   ncol=2)
plot_scatterplot(salary, by=  'hoursperweek',  ncol=2)
plot_scatterplot(salary, by=  'native',        ncol=2)
plot_scatterplot(salary, by=  'Salary',        ncol=2)


# Visualization HTML report generation in webbrowser of all features 
create_report(salary)

## Preprocessing of data 
# Check the duplicates in data 
library(dplyr)
sum(duplicated(salary_train))
sum(duplicated(salary_test))
salary_train1 <-  distinct(salary_train)
salary_test1  <-  distinct(salary_test)

# Check the NA values in data 
sum(is.na(salary_train1))
sum(is.na(salary_test1))


## Training a model on the data
install.packages("e1071")
library(e1071)
install.packages("naivebayes")
library(naivebayes)
library(caret)
# Prepare the naive base model 
salary_classifier <- naiveBayes(salary_train1$Salary~.,
                         data = salary_train1)


## Evaluating model performance
salary_test_pred <- predict(salary_classifier, salary_test1)
table(salary_test_pred, salary_test1$Salary)
confusionMatrix(salary_test_pred, salary_test1$Salary)




