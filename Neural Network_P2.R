

########## Prepare a model for strength of concrete data using Neural Networks ##########

# Load "concrete" dataset 
install.packages("readr")
library(readr)
concrete <- read.csv("File Path")

### EDA 
# Checking the correlation
cor(concrete)

## Summary of all continuous features 
summary(concrete$cement)
summary(concrete$slag)
summary(concrete$ash)
summary(concrete$water)
summary(concrete$superplastic)
summary(concrete$coarseagg)
summary(concrete$fineagg)
summary(concrete$age)
summary(concrete$strength)

# Set the environment for automated EDA
install.packages("DataExplorer")
library(DataExplorer)
install.packages("tidyverse")
library(tidyverse)


## Visualization of all features & report generation of them 
plot_intro(concrete)
plot_missing(concrete)

# Histogram
DataExplorer::plot_histogram(concrete)
# Histogram for transformed variable features 
DataExplorer::plot_histogram(log(concrete))

# Density plot
plot_density(concrete)

# Correlation between features 
plot_correlation(concrete, cor_args = list( 'use' = 'complete.obs'))

# Box plots by individual feature based against all features 
plot_boxplot(concrete, by= 'cement',  ncol = 2)
plot_boxplot(concrete, by= 'slag',  ncol = 2)
plot_boxplot(concrete, by=  'ash', ncol=2)
plot_boxplot(concrete, by=  'water', ncol=2)
plot_boxplot(concrete, by=  'superplastic', ncol=2)
plot_boxplot(concrete, by=  'coarseagg', ncol=2)
plot_boxplot(concrete, by=  'fineagg', ncol=2)
plot_boxplot(concrete, by=  'age', ncol=2)
plot_boxplot(concrete, by=  'strength', ncol=2)


# Box plots by transofmed indivisual feature based against all features 
plot_boxplot(log(concrete), by= 'cement',  ncol = 2)
plot_boxplot(log(concrete), by= 'slag',  ncol = 2)
plot_boxplot(log(concrete), by=  'ash', ncol=2)
plot_boxplot(log(concrete), by=  'water', ncol=2)
plot_boxplot(log(concrete), by=  'superplastic', ncol=2)
plot_boxplot(log(concrete), by=  'coarseagg', ncol=2)
plot_boxplot(log(concrete), by=  'fineagg', ncol=2)
plot_boxplot(log(concrete), by=  'age', ncol=2)
plot_boxplot(log(concrete), by=  'strength', ncol=2)

# Scatter plots by indivisual feature based against all features 
plot_scatterplot(concrete, by= 'cement')
plot_scatterplot(concrete, by= 'slag')
plot_scatterplot(concrete, by=  'ash')
plot_scatterplot(concrete, by=  'water')
plot_scatterplot(concrete, by=  'superplastic')
plot_scatterplot(concrete, by=  'coarseagg')
plot_scatterplot(concrete, by=  'fineagg')
plot_scatterplot(concrete, by=  'age')
plot_scatterplot(concrete, by=  'strength')


# Visualization HTML report generation in webbrowser of all features 
create_report(concrete)

# Checking the duplicates in data 
install.packages("dplyr")
library(dplyr)
sum(duplicated(concrete))

# Remove the duplicated va;ue from data 
concrete1 <- distinct(concrete)

# Checking the NA values 
sum(is.na(concrete1))

# Custom normalization function
normalize <- function(x) { 
  return((x - min(x)) / (max(x) - min(x)))
}
# Apply normalization to entire data frame
concrete_norm <- as.data.frame(lapply(concrete1, normalize))
# Create training and test data
library(caTools)
sample <- sample.int(n = nrow(concrete_norm), size = floor(.75*nrow(concrete_norm)), replace = F)
train <- concrete_norm[sample, ]
test  <- concrete_norm[-sample, ]

OR

concrete_train <- concrete_norm[1:773, ]
concrete_test <- concrete_norm[774:1030, ]
                 
## Training a model on the data 
# Train the neuralnet model
install.packages("neuralnet")
library(neuralnet)

# Simple ANN with only a single hidden neuron
concrete_model <- neuralnet(strength ~ cement + slag +
                              ash + water + superplastic + 
                              coarseagg + fineagg + age,
                            data = train,act.fct ="logistic")

# Visualize the network topology
plot(concrete_model)

## Evaluating model performance ----
#  Obtain model results
model_results <- compute(concrete_model, test[1:8])
# Obtain predicted strength values
predicted_strength <- model_results$net.result
# Examine the correlation between predicted and actual values
cor(predicted_strength, test$strength)
# Checking the accuracy of model 
rmse=(sum((test$strength-model_results$net.result)^2)/nrow(test))^0.5
Rsquare <- c(1-rmse)
Rsquare

## Improving model performance ----
#  A more complex neural network topology with 5 hidden neurons
concrete_model2 <- neuralnet(strength ~ cement + slag +
                               ash + water + superplastic + 
                               coarseagg + fineagg + age,
                             data = train, hidden =c(13,8))

# Plot the network
windows();
plot(concrete_model2)
# Evaluate the results as we did before
model_results2 <- compute(concrete_model2, test[1:8])
predicted_strength2 <- model_results2$net.result
cor(predicted_strength2, test$strength)

# Checking the accuracy of model 
rmse1=(sum((test$strength-model_results2$net.result)^2)/nrow(test))^0.5
Rsquare1 <- c(1-rmse1)
Rsquare1



