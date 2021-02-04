


### PREDICT THE BURNED AREA OF FOREST FIRES WITH NEURAL NETWORKS ####

# Load "forestfires" dataset
install.packages("readr")
library(readr)
forest_fires <- read.csv("File Path")

## Convert categorical column state of data set into numerical
forest_fires$month <- as.numeric(factor(forest_fires$month))
forest_fires$day <- as.numeric(factor(forest_fires$day))
forest_fires$size_category <- as.numeric(factor(forest_fires$size_category))

### EDA
# Checking the correlation
cor(forest_fires)

## Summary of all continuous features 
summary(forest_fires$month)
summary(forest_fires$day)
summary(forest_fires$FFMC)
summary(forest_fires$DMC)
summary(forest_fires$DC)
summary(forest_fires$ISI)
summary(forest_fires$temp)
summary(forest_fires$RH)
summary(forest_fires$wind)
summary(forest_fires$rain)
summary(forest_fires$area)
summary(forest_fires$dayfri)
summary(forest_fires$daymon)
summary(forest_fires$daysat)
summary(forest_fires$daysun)
summary(forest_fires$daythu)
summary(forest_fires$daytue)
summary(forest_fires$daywed)
summary(forest_fires$monthapr)
summary(forest_fires$monthaug)
summary(forest_fires$monthdec)
summary(forest_fires$monthfeb)
summary(forest_fires$monthjan)
summary(forest_fires$monthjul)
summary(forest_fires$monthjun)
summary(forest_fires$monthmar)
summary(forest_fires$monthmay)
summary(forest_fires$monthnov)
summary(forest_fires$monthoct)
summary(forest_fires$monthsep)
summary(forest_fires$size_category)



# Set the environment for automated EDA
install.packages("DataExplorer")
library(DataExplorer)
install.packages("tidyverse")
library(tidyverse)


## Visualization of all features & report generation of them 
plot_intro(forest_fires)
plot_missing(forest_fires)

# Histogram
DataExplorer::plot_histogram(forest_fires)
# Histogram for transformed variable features 
DataExplorer::plot_histogram(log(forest_fires))

# Density plot
plot_density(forest_fires)

# Correlation between features 
plot_correlation(forest_fires, cor_args = list( 'use' = 'complete.obs'))

# Box plots by individual feature based against all features 
plot_boxplot(forest_fires, by= 'month',  ncol = 2)
plot_boxplot(forest_fires, by= 'day',  ncol = 2)
plot_boxplot(forest_fires, by=  'FFMC', ncol=2)
plot_boxplot(forest_fires, by=  'DMC', ncol=2)
plot_boxplot(forest_fires, by=  'DC', ncol=2)
plot_boxplot(forest_fires, by=  'ISI', ncol=2)
plot_boxplot(forest_fires, by=  'temp', ncol=2)
plot_boxplot(forest_fires, by=  'RH', ncol=2)
plot_boxplot(forest_fires, by=  'wind', ncol=2)
plot_boxplot(forest_fires, by=  'rain', ncol=2)
plot_boxplot(forest_fires, by=  'area', ncol=2)



# Box plots by transofmed indivisual feature based against all features 
plot_boxplot(log(forest_fires), by= 'month',  ncol = 2)
plot_boxplot(log(forest_fires), by= 'day',  ncol = 2)
plot_boxplot(log(forest_fires), by=  'FFMC', ncol=2)
plot_boxplot(log(forest_fires), by=  'DMC', ncol=2)
plot_boxplot(log(forest_fires), by=  'DC', ncol=2)
plot_boxplot(log(forest_fires), by=  'ISI', ncol=2)
plot_boxplot(log(forest_fires), by=  'temp', ncol=2)
plot_boxplot(log(forest_fires), by=  'RH', ncol=2)
plot_boxplot(log(forest_fires), by=  'wind', ncol=2)
plot_boxplot(log(forest_fires), by=  'rain', ncol=2)
plot_boxplot(log(forest_fires), by=  'area', ncol=2)

# Scatter plots by indivisual feature based against all features 
plot_scatterplot(forest_fires, by= 'month')
plot_scatterplot(forest_fires, by= 'day')
plot_scatterplot(forest_fires, by=  'FFMC')
plot_scatterplot(forest_fires, by=  'DMC')
plot_scatterplot(forest_fires, by=  'DC')
plot_scatterplot(forest_fires, by=  'ISI')
plot_scatterplot(forest_fires, by=  'temp')
plot_scatterplot(forest_fires, by=  'RH')
plot_scatterplot(forest_fires, by=  'wind')
plot_scatterplot(forest_fires, by=  'rain')
plot_scatterplot(forest_fires, by=  'area')


# Visualization HTML report generation in webbrowser of all features 
create_report(forest_fires)

# Checking the duplicates in data 
install.packages("dplyr")
library(dplyr)
sum(duplicated(forest_fires))

# Remove the duplicated va;ue from data 
forest_fires1 <- distinct(forest_fires)

# Checking the NA values 
sum(is.na(forest_fires1))


# Custom normalization function
normalize <- function(x) { 
  return((x - min(x)) / (max(x) - min(x)))
}

# Apply normalization to entire data frame
norm_forest_fires <- as.data.frame(lapply(forest_fires1[-31], normalize))
summary(norm_forest_fires)
norm_forest_fires1 <- cbind(norm_forest_fires,forest_fires1[31])

# Create training and test data
set.seed(26)
library(caTools)
sample <- sample.int(n = nrow(norm_forest_fires1), size = floor(.75*nrow(norm_forest_fires1)), replace = F)
forest_fire_train <- norm_forest_fires1[sample, ]
forest_fire_test  <- norm_forest_fires1[-sample, ]


## Training a model on the data ----
#  Train the neuralnet model
install.packages("neuralnet")
library(neuralnet)

# Simple ANN with only a single hidden neuron
forest_fire_model <- neuralnet(size_category~.,data=forest_fire_train,
                               act.fct="logistic")


# Visualize the network topology
plot(forest_fire_model)

## Evaluating model performance ----
#  Obtain model results
model_results <- compute(forest_fire_model, forest_fire_test[,-31])
#  Obtain predicted forest burn area
predicted_burn_area <- model_results$net.result
# Examine the correlation between predicted and actual forest burn area
cor(predicted_burn_area,forest_fire_test$size_category)
# Checking the accuracy of model 
rmse=(sum((forest_fire_test$size_category-model_results$net.result)^2)/nrow(forest_fire_test))^0.5
Rsquare <- c(1-rmse)
Rsquare











