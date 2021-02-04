
######### Prepare a model for glass classification using KNN ###############

# Load "glass" dataset or import it in global environment 
Glass <- read.csv("File path")

## EDA
#  Checking the correlation 
cor(Glass)

## Summary of all continuous features 
summary(Glass$RI)
summary(Glass$Na)
summary(Glass$Mg)
summary(Glass$Al)
summary(Glass$Si)
summary(Glass$K)
summary(Glass$Ca)
summary(Glass$Ba)
summary(Glass$Fe)
summary(Glass$Type)

# Set the environment for automated EDA
install.packages("DataExplorer")
library(DataExplorer)
install.packages("tidyverse")
library(tidyverse)

## Visualiation of all features & report generation of them 
plot_intro(Glass)
plot_missing(Glass)

# Histogram
DataExplorer::plot_histogram(Glass)
# Histogram for transformed variable featuers 
DataExplorer::plot_histogram(log(Glass))

# Density plot
plot_density(Glass)

# Correlation between features 
plot_correlation(Glass, cor_args = list( 'use' = 'complete.obs'))

# Box plots by individual feature based against all features 
plot_boxplot(Glass, by= 'RI',  ncol = 2)
plot_boxplot(Glass, by= 'Na',  ncol = 2)
plot_boxplot(Glass, by= 'Mg',  ncol = 2)
plot_boxplot(Glass, by= 'Al',  ncol = 2)
plot_boxplot(Glass, by= 'Si',  ncol = 2)
plot_boxplot(Glass, by= 'K',   ncol = 2)
plot_boxplot(Glass, by= 'Ca',  ncol = 2)
plot_boxplot(Glass, by= 'Ba',  ncol = 2)
plot_boxplot(Glass, by= 'Fe',  ncol = 2)
plot_boxplot(Glass, by= 'Type',ncol = 2)


# Box plots by transofmed indivisual feature based against all features 
plot_boxplot(log(Glass), by= 'RI',  ncol = 2)
plot_boxplot(log(Glass), by= 'Na',  ncol = 2)
plot_boxplot(log(Glass), by= 'Mg',  ncol = 2)
plot_boxplot(log(Glass), by= 'Al',  ncol = 2)
plot_boxplot(log(Glass), by= 'Si',  ncol = 2)
plot_boxplot(log(Glass), by= 'K',  ncol = 2)
plot_boxplot(log(Glass), by= 'Ca',  ncol = 2)
plot_boxplot(log(Glass), by= 'Ba',  ncol = 2)
plot_boxplot(log(Glass), by= 'Fe',  ncol = 2)
plot_boxplot(log(Glass), by= 'Type',  ncol = 2)



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


# Checking & removing the duplicates from data 
library(dplyr)
sum(duplicated(Glass))
Glass1 <- distinct(Glass)

# Checking the NA values 
sum(is.na(Glass))


# Table of glass
prop.table(table(Glass1$Type))*100


# Standardize the feature of data 
install.packages('dplyr')    
library(dplyr)
standard.features <- scale(Glass1[,1:9])
# Join the standardized data with the target column
Glass_Std <- cbind(standard.features,Glass1[10])
# Check if there are any missing values to impute. 
anyNA(Glass_Std)
# Looks like the data is free from NA's
head(Glass_Std)
# Data visualization
install.packages('ggplot2')
install.packages('corrplot')
library(corrplot)
library(ggplot2)

corrplot(cor(Glass_Std))
# Split the train & test data 
install.packages('caTools')  
library(caTools)
set.seed(101)
sample <- sample.split(Glass_Std$Type,SplitRatio = 0.70)
train <- subset(Glass_Std,sample==TRUE)
test <- subset(Glass_Std,sample==FALSE)
# Prepare the KNN model for predict classification of data  
install.packages('class')   
library(class)
predicted.type <- knn(train[1:9],test[1:9],train$Type,k=1)
# Error in prediction of model 
error <- mean(predicted.type!=test$Type)
# Confusion Matrix
install.packages('caret')
library(caret)
confusionMatrix(predicted.type,as.factor(test$Type))

## Findout the different k values 
predicted.type <- NULL
error.rate <- NULL

for (i in 1:10) {
  predicted.type <- knn(train[1:9],test[1:9],train$Type,k=i)
  error.rate[i] <- mean(predicted.type!=test$Type)
  
}

knn.error <- as.data.frame(cbind(k=1:10,error.type =error.rate))


## Choosing K Value by Visualization
## Lets plot error.type vs k using ggplot
ggplot(knn.error,aes(k,error.type))+ 
  geom_point()+ 
  geom_line() + 
  scale_x_continuous(breaks=1:10)+ 
  theme_bw() +
  xlab("Value of K") +
  ylab('Error')








