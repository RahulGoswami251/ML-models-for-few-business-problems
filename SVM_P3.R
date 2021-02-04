
########### Prepare a classification model using SVM for salary data ##############

## Load "SalaryData_Train" dataset  
salary_train <- read.csv("File Path")
str(salary_train)

## Load "SalaryData_Test" dataset 
salary_test <- read.csv("File Path")
str(salary_test)

## Convert all the character variables to factor
## For training data 
salary_train$workclass <- as.numeric(factor(salary_train$workclass))
salary_train$education <- as.numeric(factor(salary_train$education))
salary_train$maritalstatus <- as.numeric(factor(salary_train$maritalstatus))
salary_train$occupation <- as.numeric(factor(salary_train$occupation))
salary_train$relationship <- as.numeric(factor(salary_train$relationship))
salary_train$race <- as.numeric(factor(salary_train$race))
salary_train$sex <- as.numeric(factor(salary_train$sex))
salary_train$native <- as.numeric(factor(salary_train$native))
salary_train$Salary <- as.factor(salary_train$Salary)




# For testing data
salary_test$workclass <- as.numeric(factor(salary_test$workclass))
salary_test$education <- as.numeric(factor(salary_test$education))
salary_test$maritalstatus <- as.numeric(factor(salary_test$maritalstatus))
salary_test$occupation <- as.numeric(factor(salary_test$occupation))
salary_test$relationship <- as.numeric(factor(salary_test$relationship))
salary_test$race <- as.numeric(factor(salary_test$race))
salary_test$sex <- as.numeric(factor(salary_test$sex))
salary_test$native <- as.numeric(factor(salary_test$native))
salary_test$Salary <- as.factor(salary_test$Salary)


# Checking the class imbalance 
table(salary_train$Salary)
table(salary_test$Salary)

## EDA 
#  Combine both data 
salary <- rbind(salary_train,salary_test)
# Convert all the factor variable into numeric 
salary$workclass <- as.numeric(factor(salary$workclass))
salary$education <- as.numeric(factor(salary$education))
salary$maritalstatus <- as.numeric(factor(salary$maritalstatus))
salary$occupation <- as.numeric(factor(salary$occupation))
salary$relationship <- as.numeric(factor(salary$relationship ))
salary$race <- as.numeric(factor(salary$race))
salary$sex <- as.numeric(factor(salary$sex))
salary$native <- as.numeric(factor(salary$native))
salary$Salary <- as.numeric(factor(salary$Salary))
# Checking the correlation
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


## Visualization of all features & report generation of them 
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
#  Check the duplicates in data 
library(dplyr)
sum(duplicated(salary_train))
sum(duplicated(salary_test))

# Check the NA values in data 
sum(is.na(salary_train))
sum(is.na(salary_test))


## Normalization of data 
norm_data1 <- scale(salary_train[,c(1:13)])
salary_train1 <- cbind(norm_data1,salary_train[14])

norm_data2 <- scale(salary_test[,c(1:13)])
salary_test1 <- cbind(norm_data2, salary_test[14])


# Begin by training a simple linear SVM
set.seed(380)
install.packages("kernlab")
library(kernlab)
salary_classifier <- ksvm(salary_train1$Salary ~ ., data = salary_train1,
                          kernel = "vanilladot")


## Evaluating model performance
#  Predictions on testing dataset
salary_predictions <- predict(salary_classifier, salary_test1[-14])
head(salary_predictions)
# table(salary_predictions, salary_test$Salary )
agreement <- salary_predictions == salary_test1$Salary
prop.table(table(agreement))
## Improving model performance 
salary_classifier_rdfdot <- ksvm(salary_train1$Salary ~ ., 
                                 data = salary_train1, 
                                 kernel = "rbfdot")
salary_predictions_rbf <- predict(salary_classifier_rdfdot, salary_test1[-14])
head(salary_predictions_rbf)
# table(burn_area_predictions_rbf, forest_fire_test$size_catagory )
agreement_rbf <- salary_predictions_rbf == salary_test1$Salary
table(agreement_rbf)
prop.table(table(agreement_rbf))



