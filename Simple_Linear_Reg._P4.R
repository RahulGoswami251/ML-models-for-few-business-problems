
############## Salary_hike -> Build a prediction model for Salary_hike ############### 


# Load "Salary_Data" dataset 
salary.data <- read.csv("File Path") 

# Visualization of data 
install.packages("lattice")
library(lattice)
dotchart(salary.data$YearsExperience, main='experinece year', color= blues9)
dotchart(salary.data$Salary,main='salary', color = blues9 )
dotplot(salary.data$YearsExperience, main='experinece year', col= 'blue')
dotplot(salary.data$Salary,main='salary', col= 'green')
barplot(salary.data$YearsExperience, main='experinece year', col= 'blue')
barplot(salary.data$Salary,main='salary', col='purple')
boxplot(salary.data$YearsExperience, main='experinece year')
boxplot(salary.data$Salary,main='salary')
hist(salary.data$YearsExperience, main= 'experience year',col = 'blue' )
hist(salary.data$Salary, main ='salary',col='purple')

# Display data column names 
colnames(salary.data)

# Normalize the data 
salary.data1 <- log(salary.data)

## Simple Linear Regression Model and Summary
salary_hike <- lm(Salary~YearsExperience, data= salary.data1)
summary(salary_hike)


# Full Model Building process
# Regression Model and Summary
salary_hike <- lm(Salary~., data= salary.data1)
summary(salary_hike)
# Diagnostic Plots:
# Residual Plots, QQ-Plos, Std. Residuals vs Fitted
plot(salary_hike) 
# Residuals vs Regressors
library(car)
residualPlots(salary_hike)
# Added Variable Plots
avPlots(salary_hike)
# QQ plots of stadentized residuals
qqPlot(salary_hike)
# Deletion Diagnostics
influenceIndexPlot(salary_hike) # Index Plots of the influence measures

### 1st Iteration 
### Transformation of input variable
salary.data1["YearsExperience2"] <- salary.data1$YearsExperience*salary.data1$YearsExperience

# Preparing new model after transformation & remove observations
salary_hike1 <- lm(Salary~., data= salary.data1)
summary(salary_hike1)

plot(salary_hike1) 
residualPlots(salary_hike1)
qqPlot(salary_hike1)
influenceIndexPlot(salary_hike1)

### 2nd Iteration 
### Transformation of input variable
salary.data1["YearsExperience3"] <- salary.data1$YearsExperience2*salary.data1$YearsExperience2

# Preparing new model after transformation & remove observations
salary_hike2 <- lm(Salary~., data= salary.data1)
summary(salary_hike2)

plot(salary_hike2) 
residualPlots(salary_hike2)
qqPlot(salary_hike2)
influenceIndexPlot(salary_hike2)

### 3rd Iteration 
### Transformation of input variable
salary.data1["YearsExperience4"] <- salary.data1$YearsExperience3*salary.data1$YearsExperience3

# Preparing new model after transformation & remove observations
salary_hike3 <- lm(Salary~., data= salary.data1)
summary(salary_hike3)

plot(salary_hike2) 
residualPlots(salary_hike3)
qqPlot(salary_hike3)
influenceIndexPlot(salary_hike3)


# Final Prediction weights for consuming calories from historical data
final_pred_sal_hike <- predict(salary_hike3)
# Print predicted value
final_pred_sal_hike

# Prepare final data  with data frame of historical data, predicted churn out rate and error
final_data <- data.frame(salary.data1,final_pred_sal_hike,
                         "Error"=salary.data1$Salary - final_pred_sal_hike)
final_data



## As per boxplot visualization there is no outliers in data set & also summary 
## of model states that there is no insignificant variable(no colinearity problem in data) 
## in data set.But As per residuals plots error had pattern in input variables data   
## then we transformed the input variable & got the increased multiple R^2 value(0.9516) 
## or 95.16% increased model accuracy. 

