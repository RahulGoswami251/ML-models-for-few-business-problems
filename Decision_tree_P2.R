

###### Use Decision tree to prepare a model on fraud data treating ########

## Load "Fraud_check" dataset
fraud_check <- read.csv("File path")

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
set.seed(42)

## Convert character varaible into factor 
fraud_check$Undergrad <- factor(fraud_check$Undergrad)
fraud_check$Marital.Status <- factor(fraud_check$Marital.Status) 
fraud_check$Urban <- factor(fraud_check$Urban)

## Create the column for given condition for fraud detection 
Risky_Good <- ifelse(fraud_check$Taxable.Income<=30000,"Risky","Good")
Fr_Ch <- data.frame(fraud_check,Risky_Good) 
Fr_Ch$Risky_Good <- factor(Fr_Ch$Risky_Good)

# Summary of data 
summary(Fr_Ch$Undergrad)
summary(Fr_Ch$Marital.Status)
summary(Fr_Ch$Taxable.Income)
summary(Fr_Ch$City.Population)
summary(Fr_Ch$Work.Experience)
summary(Fr_Ch$Urban)
summary(Fr_Ch$Risky_Good)


# Set the environment for automated EDA
install.packages("DataExplorer")
library(DataExplorer)
install.packages("tidyverse")
library(tidyverse)


## Visualiation of all features & report generation of them 
plot_intro(Fr_Ch)
plot_missing(Fr_Ch)

# Histogram
DataExplorer::plot_histogram(Fr_Ch)

#Density plot
plot_density(Fr_Ch)

# Correlation between features 
plot_correlation(Fr_Ch, cor_args = list( 'use' = 'complete.obs'))

# Box plots by individual feature based against all features 
plot_boxplot(Fr_Ch, by= 'Undergrad',  ncol = 2)
plot_boxplot(Fr_Ch, by= 'Marital.Status',  ncol = 2)
plot_boxplot(Fr_Ch, by= 'Taxable.Income',  ncol = 2)
plot_boxplot(Fr_Ch, by= 'City.Population',  ncol = 2)
plot_boxplot(Fr_Ch, by= 'Work.Experience',  ncol = 2)
plot_boxplot(Fr_Ch, by= 'Urban',  ncol = 2)
plot_boxplot(Fr_Ch, by= 'Risky_Good',  ncol = 2)


# Scatter plots by individual feature based against all features 
plot_scatterplot(Fr_Ch, by= 'Undergrad')
plot_scatterplot(Fr_Ch, by= 'Marital.Status')
plot_scatterplot(Fr_Ch, by= 'Taxable.Income')
plot_scatterplot(Fr_Ch, by= 'City.Population')
plot_scatterplot(Fr_Ch, by= 'Work.Experience')
plot_scatterplot(Fr_Ch, by= 'Urban')
plot_scatterplot(Fr_Ch, by= 'Risky_Good')


# Visualization HTML report generation in webbrowser of all features 
create_report(Fr_Ch)


## Create the training & testing dataset 
inTraininglocal<-createDataPartition(Fr_Ch$Risky_Good,p=.70,list = F)
training<-Fr_Ch[inTraininglocal,]
testing<-Fr_Ch[-inTraininglocal,]

# Model Building
FC_model<-C5.0(Risky_Good~.,data = training)

# Generate the model summary
summary(FC_model)
# Predict for test data set
pred_FC<-predict.C5.0(FC_model,testing[,-7]) #type ="prob"
# Accuracy of the algorithm
accu<-table(testing$Risky_Good,pred_FC)
sum(diag(accu))/sum(accu)
# Visualize the decision tree
plot(FC_model)




################# Boosting Method
## Load "Fraud_check" dataset
fraud_check <- read.csv("File path")

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
set.seed(52)

## Convert character varaible into factor 
fraud_check$Undergrad <- factor(fraud_check$Undergrad)
fraud_check$Marital.Status <- factor(fraud_check$Marital.Status) 
fraud_check$Urban <- factor(fraud_check$Urban)

## Create the column for given condition for fraud detection 
Risky_Good <- ifelse(fraud_check$Taxable.Income<=30000,"Risky","Good")
Fr_Ch. <- data.frame(fraud_check,Risky_Good) 
Fr_Ch$Risky_Good <- factor(Fr_Ch$Risky_Good)
inTraininglocal<-createDataPartition(Fr_Ch$Risky_Good,p=.70,list = F)
training<-Fr_Ch.[inTraininglocal,]
testing<-Fr_Ch.[-inTraininglocal,]

# Model Building
model<-C5.0(Risky_Good~.,data = training,trails=10)

# Generate the model summary
summary(model)
# Predict for test data set
pred<-predict.C5.0(model,testing[,-7]) #type ="prob"
# Accuracy of the algorithm
a<-table(testing$Risky_Good,pred)
sum(diag(a))/sum(a)
# Visualize the decision tree
plot(model)









