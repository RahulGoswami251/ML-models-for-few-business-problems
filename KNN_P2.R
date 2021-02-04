

######### Implement a KNN model to classify the animals into category ###############

# Load "Zoo" dataset 
Zoo <- read.csv("File Path")

# Table of glass
Zoo <- Zoo[,-1]

## EDA
# Checking the correlation 
cor(Zoo)

## Summary of all continuous features 
summary(Zoo$hair)
summary(Zoo$feathers)
summary(Zoo$eggs)
summary(Zoo$milk)
summary(Zoo$airborne)
summary(Zoo$aquatic)
summary(Zoo$predator)
summary(Zoo$toothed)
summary(Zoo$backbone)
summary(Zoo$breathes)
summary(Zoo$venomous)
summary(Zoo$fins)
summary(Zoo$legs)
summary(Zoo$tail)
summary(Zoo$domestic)
summary(Zoo$catsize)


# Set the environment for automated EDA
install.packages("DataExplorer")
library(DataExplorer)
install.packages("tidyverse")
library(tidyverse)

## Visualization of all features & report generation of them 
plot_intro(Zoo)
plot_missing(Zoo)

# Histogram
DataExplorer::plot_histogram(Zoo)

# Density plot
plot_density(Zoo)

# Correlation between features 
plot_correlation(Zoo, cor_args = list( 'use' = 'complete.obs'))

# Box plots by individual feature based against all features 
plot_boxplot(Zoo, by= 'hair',  ncol = 2)
plot_boxplot(Zoo, by= 'feathers',  ncol = 2)
plot_boxplot(Zoo, by= 'eggs',  ncol = 2)
plot_boxplot(Zoo, by= 'milk',  ncol = 2)
plot_boxplot(Zoo, by= 'airborn',  ncol = 2)
plot_boxplot(Zoo, by= 'quaatic',  ncol = 2)
plot_boxplot(Zoo, by= 'predator',  ncol = 2)
plot_boxplot(Zoo, by= 'toothed',  ncol = 2)
plot_boxplot(Zoo, by= 'backbone',  ncol = 2)
plot_boxplot(Zoo, by= 'breathes',  ncol = 2)
plot_boxplot(Zoo, by= 'hair',  ncol = 2)
plot_boxplot(Zoo, by= 'hair',  ncol = 2)
plot_boxplot(Zoo, by= 'hair',  ncol = 2)
plot_boxplot(Zoo, by= 'hair',  ncol = 2)
plot_boxplot(Zoo, by= 'hair',  ncol = 2)
plot_boxplot(Zoo, by= 'hair',  ncol = 2)
plot_boxplot(Zoo, by= 'hair',  ncol = 2)



# Scatter plots by indivisual feature based against all features 
plot_scatterplot(Glass, by= 'RI')
plot_scatterplot(Glass, by= 'Na')
plot_scatterplot(Glass, by= 'Mg')
plot_scatterplot(Glass, by= 'Al')
plot_scatterplot(Glass, by= 'Si')
plot_scatterplot(Glass, by= 'K')
plot_scatterplot(Glass, by= 'Ca')
plot_scatterplot(Glass, by= 'Ba')
plot_scatterplot(Glass, by= 'Fe')
plot_scatterplot(Glass, by= 'Type')


# Visualization HTML report generation in webbrowser of all features 
create_report(Glass)


# Checking the data balance condition  
prop.table(table(Zoo$type))*100
# Standardize the feature of data 
install.packages('dplyr')    
library(dplyr)
standard.features <- scale(Zoo[,1:16])
# Join the standardized data with the target column
Zoo_Std <- cbind(standard.features,Zoo[17])
# Check if there are any missing values to impute. 
anyNA(Zoo_Std)
# Looks like the data is free from NA's
head(Zoo_Std)
# Data visualization
install.packages('ggplot2')
install.packages('corrplot')
library(corrplot)
library(ggplot2)
corrplot(cor(Zoo_Std))
# Split the train & test data 
install.packages('caTools')  
library(caTools)
set.seed(101)
sample <- sample.split(Zoo_Std$type,SplitRatio = 0.70)
train <- subset(Zoo_Std,sample==TRUE)
test <- subset(Zoo_Std,sample==FALSE)
# Prepare the KNN model for predict classification of data  
install.packages('class')   
library(class)
predicted.type <- knn(train[1:16],test[1:16],train$type,k=1)
# Error in prediction of model 
error <- mean(predicted.type!=test$type)
# Confusion Matrix
install.packages('caret')
library(caret)
confusionMatrix(predicted.type,as.factor(test$type))

## Findout the different k values 
predicted.type <- NULL
error.rate <- NULL

for (i in 1:17) {
  predicted.type <- knn(train[1:16],test[1:16],train$type,k=i)
  error.rate[i] <- mean(predicted.type!=test$type)
  
}

knn.error <- as.data.frame(cbind(k=1:17,error.type =error.rate))


## Choosing K Value by Visualization
#  Lets plot error.type vs k using ggplot
ggplot(knn.error,aes(k,error.type))+ 
  geom_point()+ 
  geom_line() + 
  scale_x_continuous(breaks=1:17)+ 
  theme_bw() +
  xlab("Value of K") +
  ylab('Error')

# Prepare the new KNN model for predict classification of data  
install.packages('class')   
library(class)
predicted.type1 <- knn(train[1:16],test[1:16],train$type,k=3)
# Error in prediction of model 
error <- mean(predicted.type1!=test$type)
# Confusion Matrix
install.packages('caret')
library(caret)
confusionMatrix(predicted.type1,as.factor(test$type))



