


#### Build a Random forest for the 'iris' data with function 'ctree()' in package "party"###

# Load "iris" Data 
data("iris")
# To make the results consistent across the runs
attach(iris)
set.seed(7)

## EDA Process
#  Summary of data 
summary(iris$Sepal.Length)
summary(iris$Sepal.Width)
summary(iris$Petal.Length)
summary(iris$Petal.Width)
summary(iris$Species)

# Converting character into numerical feature 
iris$Species <- as.factor(iris$Species)

# Set the environment for automated EDA
install.packages("DataExplorer")
library(DataExplorer)
install.packages("tidyverse")
library(tidyverse)


## Visualiation of all features & report generation of them 
plot_intro(iris)
plot_missing(iris)

# Histogram
DataExplorer::plot_histogram(iris)

# Density plot
plot_density(iris)

# Correlation between features 
plot_correlation(iris, cor_args = list( 'use' = 'complete.obs'))

# Box plots by individual feature based against all features 
plot_boxplot(iris, by= 'Sepal.Length',  ncol = 2)
plot_boxplot(iris, by= 'Sepal.Width',   ncol = 2)
plot_boxplot(iris, by= 'Petal.Length',  ncol = 2)
plot_boxplot(iris, by= 'Petal.Width',   ncol = 2)
plot_boxplot(iris, by= 'Species',       ncol = 2)


# Scatter plots by individual feature based against all features 
plot_scatterplot(iris, by= 'Sepal.Length')
plot_scatterplot(iris, by= 'Sepal.Width')
plot_scatterplot(iris, by= 'Petal.Length')
plot_scatterplot(iris, by= 'Petal.Width')
plot_scatterplot(iris, by= 'Species')


# Visualization HTML report generation in webbrowser of all features 
create_report(iris)


# Checking the duplicates in data 
install.packages("dplyr")
library(dplyr)
sum(duplicated(iris))

# Remove duplicates from data & create new dataset  
iris1 <- distinct(iris)

# Data normalization 
norm_data <- scale(iris1[,c(1:4)])
iris2 <- cbind(norm_data,iris1[5])
#Data Partition
inTraininglocal<-createDataPartition(iris2$Species,p=.80,list = F)
training<-iris2[inTraininglocal,]
testing<-iris2[-inTraininglocal,]


## Set the environment 
install.packages("caret", dependencies = TRUE)
install.packages("randomForest")
library(randomForest)
library(caret)
## Prepare random forest model 
Spec._model<-randomForest(training$Species~.,data=training,ntree=300)
## View the forest results.
print(Spec._model)
# Importance of the variable - Lower Gini
print(importance(Spec._model))
# Prediction
pred<- predict(Spec._model,testing[,-5])
table(pred,testing$Species)
## Visualization of model
plot(Spec._model)
## Checking the testing accuracy 
confusionMatrix(pred,testing$Species)

## Checking the testing accuracy 
pred1 <- predict(Spec._model,training[-5])
table(pred1,training$Species)
confusionMatrix(pred1,training$Species)








