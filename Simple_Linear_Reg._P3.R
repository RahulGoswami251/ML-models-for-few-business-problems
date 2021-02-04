
########## Emp_data -> Build a prediction model for Churn_out_rate ############

# Load "emp_data" dataset 
employ_data <- read.csv("File Path")

# Visualizations
install.packages("lattice")
library(lattice)
dotchart(employ_data$Churn_out_rate, main = 'churn out rate', color = blues9)
dotchart(employ_data$Salary_hike,main = 'salary hike', color = blues9)
dotplot(employ_data$Churn_out_rate, main = 'churn out rate', col = 'blue')
dotplot(employ_data$Salary_hike,main = 'salary hike', col = 'green')
boxplot(employ_data$Churn_out_rate,main = 'churn out rate')
boxplot(employ_data$Salary_hike,main = 'salary hike')
barplot(employ_data$Churn_out_rate, main = 'churn out rate', col = 'blue')
barplot(employ_data$Salary_hike,main = 'salary hike', col = 'green')
hist(employ_data$Churn_out_rate, main = 'churn out rate', col = 'blue')
hist(employ_data$Salary_hike,main = 'salary hike', col = 'green')


# Columns name
colnames(employ_data)

# Normalize the data 
employ_data1 <- log(employ_data)

## Regression Model and Summary
Employ_model <- lm(Churn_out_rate~Salary_hike, data = employ_data1)
summary(Employ_model)


# Full Model Building process
# Regression Model and Summary
Employ_model <- lm(Churn_out_rate~., data = employ_data1)
summary(Employ_model)
# Diagnostic Plots:
# Residual Plots, QQ-Plos, Std. Residuals vs Fitted
plot(Employ_model) 
# Residuals vs Regressors
library(car)
residualPlots(Employ_model)
# Added Variable Plots
avPlots(Employ_model)
# QQ plots of stadentized residuals
qqPlot(Employ_model)
# Deletion Diagnostics
influenceIndexPlot(Employ_model) # Index Plots of the influence measures

### 1st Iteration 
### Remove the 1st and 10th observation & Transformation of input variable
employ_data2 <- employ_data1[c(-1,-10),]
employ_data1["Salary_hike2"] <- employ_data1$Salary_hike*employ_data1$Salary_hike 

# Preparing new model after transformation & remove observations
Employ_model1 <- lm(Churn_out_rate~., data = employ_data2)
summary(Employ_model1)

plot(Employ_model1  ) 
residualPlots(Employ_model1 )
qqPlot(Employ_model1  )
influenceIndexPlot(Employ_model1  )


## Remove the 9th & 2nd observation 
employ_data2["Salary_hike3"] <- employ_data2$Salary_hike*employ_data2$Salary_hike 
employ_data3 <- employ_data2[c(-2,-9),]

# Preparing new model after transformation & remove observations
Employ_model2 <- lm(Churn_out_rate~., data = employ_data3)
summary(Employ_model2)

plot(Employ_model2) 
residualPlots(Employ_model2)
qqPlot(Employ_model2)
influenceIndexPlot(Employ_model2)


# Final Prediction weights for consuming calories from historical data
final_pred_emp_data <- predict(Employ_model2)
# Print predicted value
final_pred_emp_data

# Prepare final data  with data frame of historical data, predicted churn out rate and error
final_data <- data.frame(employ_data3,final_pred_emp_data,
                         "Error"=employ_data3$Churn_out_rate - final_pred_emp_data)
final_data



## As per boxplot visualization there is no outliers in data set & also summary 
## of model states that there is no insignificant variable(no colinearity problem in data) 
## in data set.But As per residuals plots error had pattern in input variables data   
## then we transformed the input variable & got the increased multiple R^2 value(0.9885) 
## or 98.85% increased model accuracy. 





