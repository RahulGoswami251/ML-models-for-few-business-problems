


###### Use Random forest to prepare a model on fraud data treating ########

## Load "Fraud_check" dataset 
fraud_check <- read.csv("File Path")


## Create the class of given condition for fraud detection 
Risky_Good <- ifelse(fraud_check$Taxable.Income<=30000,"Risky","Good")
Fr_Ch <- data.frame(fraud_check,Risky_Good) 
Fr_Ch$Risky_Good <- factor(Fr_Ch$Risky_Good)

## Convert character variable into numerical 
Fr_Ch$Undergrad <-      as.numeric(factor(Fr_Ch$Undergrad))
Fr_Ch$Marital.Status <- as.numeric(factor(Fr_Ch$Marital.Status)) 
Fr_Ch$Urban <-          as.numeric(factor(Fr_Ch$Urban))



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


## Visualization of all features & report generation of them 
plot_intro(Fr_Ch)
plot_missing(Fr_Ch)

# Histogram
DataExplorer::plot_histogram(Fr_Ch)

# Density plot
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

## Preprocessing of data 
#  Data Normalization 
#  Apply normalization 
norm_frch_data <- scale(Fr_Ch[,c(1:6)])
final_frch_data <- cbind(norm_frch_data,Fr_Ch[7])

## Create the training & testing dataset 
library(caret)
inTraininglocal<-createDataPartition(final_frch_data$Risky_Good,p=.80,list = F)
training<-final_frch_data[inTraininglocal,]
testing<-final_frch_data[-inTraininglocal,]


# To make the results consistent across the runs
set.seed(160)
# Prepare the Random forest model 
Control_parameters <- trainControl(method = "cv",
                                   number = 10,
                                   savePredictions = TRUE,
                                   classProbs = TRUE)


# Prepare the random forest model 
FC_model <- train(Risky_Good~.,
                         data = training,
                         method= "rf",
                         trControl= Control_parameters,tuneGrid = grid_tuning)


## View the forest results.
print(FC_model)
# Prediction
pred<- predict(FC_model,testing[-7])
table(pred,testing$Risky_Good)
## Visualization of model
plot(FC_model)
confusionMatrix(pred,testing$Risky_Good)

# Checking the training accuracy 
pred1 <- predict(FC_model,training[,-7])
table(pred1,training$Risky_Good)
confusionMatrix(pred1,training$Risky_Good)



