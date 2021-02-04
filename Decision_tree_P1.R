


### Building decision tree model for a cloth manufacturing company who  is interested to know about the segment or attributes causes high sale###### 

## Load "Company_Data" dataset 
company_data <- read.csv("File Path")

## EDA process
## Categorized the class variable 
range(company_data$Sales)
summary(company_data$Sales)

sales_cat. <- with(company_data,ifelse(company_data$Sales<=5.390,"Very Poor",
                                       ifelse(company_data$Sales>5.390 & company_data$Sales<7.490,"Poor",
                                              ifelse(company_data$Sales>7.490 & company_data$Sales<9.320,"Average",
                                                     ifelse(company_data$Sales>9.320 & company_data$Sales<=16.270,"Good","Very Good")))))



company_data1 <- data.frame(company_data,sales_cat.)

## Convert class variables into character to factor 
company_data1$Urban <- as.numeric(factor(company_data1$Urban))
company_data1$US <-    as.numeric(factor(company_data1$US))
company_data1$ShelveLoc <- as.numeric(factor(company_data1$ShelveLoc))
company_data1$sales_cat. <- factor(company_data1$sales_cat.)


# Summary of data 
summary(company_data1$Sales)
summary(company_data1$CompPrice)
summary(company_data1$Income)
summary(company_data1$Advertising)
summary(company_data1$Population)
summary(company_data1$Price)
summary(company_data1$ShelveLoc)
summary(company_data1$Age)
summary(company_data1$Education)
summary(company_data1$Urban)
summary(company_data1$US)
summary(company_data1$sales_cat.)

# Set the environment for automated EDA
install.packages("DataExplorer")
library(DataExplorer)
install.packages("tidyverse")
library(tidyverse)


## Visualiation of all features & report generation of them 
plot_intro(company_data1)
plot_missing(company_data1)

# Histogram
DataExplorer::plot_histogram(company_data1)

# Density plot
plot_density(company_data1)

# Correlation between features 
plot_correlation(company_data1, cor_args = list( 'use' = 'complete.obs'))

# Box plots by indivisual feature based against all features 
plot_boxplot(company_data1, by= 'Sales',  ncol = 2)
plot_boxplot(company_data1, by= 'CompPrice',  ncol = 2)
plot_boxplot(company_data1, by= 'Income',  ncol = 2)
plot_boxplot(company_data1, by= 'Advertising',  ncol = 2)
plot_boxplot(company_data1, by= 'Population',  ncol = 2)
plot_boxplot(company_data1, by= 'Price',  ncol = 2)
plot_boxplot(company_data1, by= 'ShelveLoc',  ncol = 2)
plot_boxplot(company_data1, by= 'Age',  ncol = 2)
plot_boxplot(company_data1, by= 'Education',  ncol = 2)
plot_boxplot(company_data1, by= 'Urban',  ncol = 2)
plot_boxplot(company_data1, by= 'US',  ncol = 2)
plot_boxplot(company_data1, by= 'sales_cat.',  ncol = 2)



# Scatter plots by indivisual feature based against all features 
plot_scatterplot(company_data1, by= 'Sales')
plot_scatterplot(company_data1, by= 'CompPrice')
plot_scatterplot(company_data1, by= 'Income')
plot_scatterplot(company_data1, by= 'Advertising')
plot_scatterplot(company_data1, by= 'Population')
plot_scatterplot(company_data1, by= 'Price')
plot_scatterplot(company_data1, by= 'ShelveLoc')
plot_scatterplot(company_data1, by= 'Age')
plot_scatterplot(company_data1, by= 'Education')
plot_scatterplot(company_data1, by= 'Urban')
plot_scatterplot(company_data1, by= 'US')
plot_scatterplot(company_data1, by= 'sales_cat.')


# Visualization HTML report generation in webbrowser of all features 
create_report(company_data1)

## Preprocessing of data 
# Check the NA values in data 
sum(is.na(company_data1))
sum(duplicated(company_data1))
# To make the results consistent across the runs
set.seed(150)
# Data Normalization 
# Custom normalization function
normalize <- function(x) { 
  return((x - min(x)) / (max(x) - min(x)))
}

# Apply normalization to entire data frame
norm_company_data <- as.data.frame(lapply(company_data1[,c(1:11)],normalize))
final_company_data <- cbind(norm_company_data,company_data1[12])
# Data Partition
library(caret)
inTraininglocal <- createDataPartition(final_company_data$sales_cat.,p=.80,list = F)
training<-final_company_data[inTraininglocal,]
testing<- final_company_data[-inTraininglocal,]



# Prepare the decision tree model 
sales_value <- C5.0(sales_cat.~.,data = training)
# Generate the model summary
summary(sales_value)
# Predict for test data set
pred_tmodel <- predict.C5.0(sales_value,testing[,-12])
# Accuracy of algorithm 
accu_sm <- table(testing$Sales,pred_tmodel)
sum(diag(accu_sm))/sum(accu_sm)
# Visualize the decision tree
plot(sales_value)
confusionMatrix(pred_tmodel,testing$sales_cat.)


# Checking the training accuracy 
# Prediction 
pred1 <- predict(sales_value,training)
table(pred1,training$sales_cat.)
confusionMatrix(pred1,training$sales_cat.)





