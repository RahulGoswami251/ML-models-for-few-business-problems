

#### Build a decision tree for the 'iris' data with function 'ctree()' in package "party" ###

# Load "iris" dataset 
data("iris")
# Install the required packages
install.packages("caret")
install.packages("C50")
install.packages("tree")
install.packages("party")
# Library invoke
library(caret)
library(C50)
library(tree)
library(party)
# To make the results consistent across the runs
attach(iris)
set.seed(7)

# Converted character type class into factor 
iris$Species <- as.factor(iris$Species)


## EDA Process
# Summary of data 
summary(iris$Sepal.Length)
summary(iris$Sepal.Width)
summary(iris$Petal.Length)
summary(iris$Petal.Width)
summary(iris$Species)


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

# Box plots by indivisual feature based against all features 
plot_boxplot(iris, by= 'Sepal.Length',  ncol = 2)
plot_boxplot(iris, by= 'Sepal.Width',   ncol = 2)
plot_boxplot(iris, by= 'Petal.Length',  ncol = 2)
plot_boxplot(iris, by= 'Petal.Width',   ncol = 2)
plot_boxplot(iris, by= 'Species',       ncol = 2)


# Scatter plots by indivisual feature based against all features 
plot_scatterplot(iris, by= 'Sepal.Length')
plot_scatterplot(iris, by= 'Sepal.Width')
plot_scatterplot(iris, by= 'Petal.Length')
plot_scatterplot(iris, by= 'Petal.Width')
plot_scatterplot(iris, by= 'Species')


# Visualization HTML report generation in webbrowser of all features 
create_report(iris)


# Cheking the duplicay in data 
install.packages("dplyr")
library(dplyr)
sum(duplicated(iris))

# Remove duplicacy from data & create new dataset  
iris1 <- distinct(iris)


# Data Partition
inTraininglocal<-createDataPartition(iris1$Species,p=.70,list = F)
training<-iris1[inTraininglocal,]
testing<-iris1[-inTraininglocal,]

# Model Building
model<-ctree(Species~.,data = training) 
# Generate the model summary
summary(model)
# Predict for test data set
pred<-predict(model,testing[,-5]) #type ="prob"
# Accuracy of the algorithm
a<-table(testing$Species,pred)
sum(diag(a))/sum(a)
# Visualize the decision tree
plot(model)
