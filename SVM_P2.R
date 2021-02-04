

###### Classify the Size_Categorie using SVM ########

## Load "forestfires" dataset  
forest_fires <- read.csv("File Path")

## Convert categorical column state of data set into numerical
forest_fires$month <- as.numeric(factor(forest_fires$month))
forest_fires$day <- as.numeric(factor(forest_fires$day))
forest_fires$size_category <- as.factor(forest_fires$size_category)

# Checking the class imbalance 
table(forest_fires$size_category)


# EDA 
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
summary(forest_fires$size_category)


# Set the environment for automated EDA
install.packages("DataExplorer")
library(DataExplorer)
install.packages("tidyverse")
library(tidyverse)


## Visualiation of all features & report generation of them 
plot_intro(forest_fires)
plot_missing(forest_fires)

# Histogram
DataExplorer::plot_histogram(forest_fires)
# Histogram for transformed variable features 
DataExplorer::plot_histogram(log(forest_fires[1:30]))

# Density plot
plot_density(forest_fires)

# Correlation between features 
plot_correlation(forest_fires, cor_args = list( 'use' = 'complete.obs'))

# Box plots by individual feature based against all features 
plot_boxplot(forest_fires, by=  'month',   ncol=2)
plot_boxplot(forest_fires, by=  'day',     ncol=2)
plot_boxplot(forest_fires, by=  'FFMC',    ncol=2)
plot_boxplot(forest_fires, by=  'DMC',     ncol=2)
plot_boxplot(forest_fires, by=  'DC',      ncol=2)
plot_boxplot(forest_fires, by=  'ISI',     ncol=2)
plot_boxplot(forest_fires, by=  'temp',    ncol=2)
plot_boxplot(forest_fires, by=  'RH',      ncol=2)
plot_boxplot(forest_fires, by=  'wind',    ncol=2)
plot_boxplot(forest_fires, by=  'rain',    ncol=2)
plot_boxplot(forest_fires, by=  'area',    ncol=2)
plot_boxplot(forest_fires, by=  'size_category',  ncol=2)



# Scatter plots by indivisual feature based against all features 
plot_scatterplot(forest_fires, by=  'month',   ncol=2)
plot_scatterplot(forest_fires, by=  'day',     ncol=2)
plot_scatterplot(forest_fires, by=  'FFMC',    ncol=2)
plot_scatterplot(forest_fires, by=  'DMC',     ncol=2)
plot_scatterplot(forest_fires, by=  'DC',      ncol=2)
plot_scatterplot(forest_fires, by=  'ISI',     ncol=2)
plot_scatterplot(forest_fires, by=  'temp',    ncol=2)
plot_scatterplot(forest_fires, by=  'RH',      ncol=2)
plot_scatterplot(forest_fires, by=  'wind',    ncol=2)
plot_scatterplot(forest_fires, by=  'rain',    ncol=2)
plot_scatterplot(forest_fires, by=  'area',    ncol=2)
plot_scatterplot(forest_fires, by=  'size_category',  ncol=2)


# Visualization HTML report generation in webbrowser of all features 
create_report(forest_fires)

## Preprocessing of data 
# Normalize the features 
norm_data <- scale(forest_fires[,c(1:30)])
forest_fires1 <- cbind(norm_data,forest_fires[31])


# Divide into training and test data
set.seed(86)
library(caTools)
sample <- sample.int(n = nrow(forest_fires1),
                     size = floor(.80*nrow(forest_fires1)),
                     replace = F)
forest_fire_train <- forest_fires1[sample, ]
forest_fire_test  <- forest_fires1[-sample, ]


# Begin by training a simple linear SVM
install.packages("kernlab")
library(kernlab)
burn_area_classifier <- ksvm(size_category ~ ., data = forest_fire_train,
                          kernel = "vanilladot")


## Evaluating model performance ----
# Predictions on testing dataset
burn_area_predictions <- predict(burn_area_classifier, forest_fire_test)
head(burn_area_predictions)
# table(burn_area_predictions, forest_fire_test$size_catagory )
agreement <- burn_area_predictions == forest_fire_test$size_category
prop.table(table(agreement))

