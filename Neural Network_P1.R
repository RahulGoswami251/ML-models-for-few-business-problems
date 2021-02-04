

####### Build a Neural Network model for 50_startups data to predict profit ########### 

# Load "50_Startups" dataset or Import it in global environment  
fifty_start_up <- read.csv("File Path")

## EDA 
## Convert categorical column state of data set into numerical 
fifty_start_up$State <- as.factor(fifty_start_up$State)
fifty_start_up$State

fifty_start_up$State <- as.numeric(factor(fifty_start_up$State))
fifty_start_up$State

## Summary of all continuous features 
summary(fifty_start_up$R.D.Spend)
summary(fifty_start_up$Administration)
summary(fifty_start_up$Marketing.Spend)
summary(fifty_start_up$State)
summary(fifty_start_up$Profit)

# Set the environment for automated EDA
install.packages("DataExplorer")
library(DataExplorer)
install.packages("tidyverse")
library(tidyverse)

# Cheking the correlation
cor(fifty_start_up)

## Visualiation of all features & report generation of them 
plot_intro(fifty_start_up)
plot_missing(fifty_start_up)

# Histogram
DataExplorer::plot_histogram(fifty_start_up)
# Histogram for transformed variable featuers 
DataExplorer::plot_histogram(log(fifty_start_up))

# Density plot
plot_density(fifty_start_up)

# Correlation between features 
plot_correlation(fifty_start_up, cor_args = list( 'use' = 'complete.obs'))

# Box plots by individual feature based against all features 
plot_boxplot(fifty_start_up, by= 'R.D.Spend',  ncol = 2)
plot_boxplot(fifty_start_up, by= 'Administration',  ncol = 2)
plot_boxplot(fifty_start_up, by=  'Marketing.Spend', ncol=2)
plot_boxplot(fifty_start_up, by=  'State', ncol=2)


# Box plots by transformed individual feature based against all features 
plot_boxplot(log(fifty_start_up), by= 'R.D.Spend',  ncol = 2)
plot_boxplot(log(fifty_start_up), by= 'Administration',  ncol = 2)
plot_boxplot(log(fifty_start_up), by=  'Marketing.Spend', ncol=2)


# Scatter plots by individual feature based against all features 
plot_scatterplot(fifty_start_up, by= 'R.D.Spend')
plot_scatterplot(fifty_start_up, by= 'Administration')
plot_scatterplot(fifty_start_up, by=  'Marketing.Spend')
plot_scatterplot(fifty_start_up, by=  'State')



# Visualization HTML report generation in webbrowser of all features 
create_report(fifty_start_up)

# Checking the duplicates in data 
sum(duplicated(fifty_start_up))
# Checking the NA values 
sum(is.na(fifty_start_up))


## Preprocessing of data 
#  Custom normalization function
normalize <- function(x) { 
                 return((x - min(x)) / (max(x) - min(x)))
             }

# Apply normalization to entire data frame
norm_fifty_stup <- as.data.frame(lapply(fifty_start_up, normalize))
# Create training and test data
set.seed(123)
library(caTools)
sample <- sample.int(n = nrow(norm_fifty_stup), size = floor(.75*nrow(norm_fifty_stup)), replace = F)
fifty_stup_train <- norm_fifty_stup[sample, ]
fifty_stup_test  <- norm_fifty_stup[-sample, ]


## Training a model on the data ----
#Train the neuralnet model
install.packages("neuralnet")
library(neuralnet)

# Simple ANN with only a single hidden neuron
fifty_startup_model <- neuralnet(Profit ~R.D.Spend+Administration+Marketing.Spend+State,
                            data = fifty_stup_train,act.fct ="logistic")

# Visualize the network topology
plot(fifty_startup_model)

## Evaluating model performance ----
#  Obtain model results
model_results <- compute(fifty_startup_model, fifty_stup_test[1:4])
# Obtain predicted profit values
predicted_profit <- model_results$net.result
# Examine the correlation between predicted and actual profit values
cor(predicted_profit,fifty_stup_test$Profit)

# Checking the accuracy of model 
rmse=(sum((fifty_stup_test$Profit-model_results$net.result)^2)/nrow(fifty_stup_test))^0.5
Rsquare <- c(1-rmse)
Rsquare


## Improving model performance ----
#  A more complex neural network topology with 4 hidden neurons
fifty_startup_model2 <- neuralnet(Profit ~R.D.Spend+Administration+Marketing.Spend+State,
                             data = fifty_stup_train, hidden =c(7,3))

# Plot the network
windows();
plot(fifty_startup_model2)
# Evaluate the results as we did before
model_results2 <- compute(fifty_startup_model2, fifty_stup_test[1:4])
predicted_profit2 <- model_results2$net.result
cor(predicted_profit2, fifty_stup_test$Profit)

# Checking the accuracy of model 
rmse1=(sum((fifty_stup_test$Profit-model_results2$net.result)^2)/nrow(fifty_stup_test))^0.5
Rsquare1 <- c(1-rmse1)
Rsquare1


