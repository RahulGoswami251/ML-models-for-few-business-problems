
######## Calories_consumed-> prediction of weight gained using calories consumed ########

# Load "calories_consumed" dataset
calories_consumed <- read.csv("File Path")

# Visualization of data 
install.packages("lattice")
library(lattice)
dotchart(calories_consumed$Calories.Consumed, main='calories cosume', color = blues9)
dotchart(calories_consumed$Weight.gained..grams., main='weight gain', color=blues9)
dotplot(calories_consumed$Calories.Consumed, main='calories cosume',col='blue')
dotplot(calories_consumed$Weight.gained..grams., main='weight gain', col='blue')
boxplot(calories_consumed$Calories.Consumed,main='calories consume')
boxplot(calories_consumed$Weight.gained..grams., main='weight gain')
barplot(calories_consumed$Calories.Consumed,main = 'calories consume', col = 'blue')
barplot(calories_consumed$Weight.gained..grams., main = 'weight gain', col= 'green')
hist(calories_consumed$Calories.Consumed,main='calories consume',col = 'blue')
hist(calories_consumed$Weight.gained..grams., main = 'weight gain', col = 'purple')

# Display column names
colnames(calories_consumed)
# Normalize the data 
norm_data <- log(calories_consumed)

## Simple Linear Regression Model and Summary
cal_wgt_model <- lm(Weight.gained..grams.~Calories.Consumed,data = calories_consumed1)
summary(cal_wgt_model)


# Full Model Building process
# Regression Model and Summary
cal_wgt_model <- lm(Weight.gained..grams.~.,data = calories_consumed1)
summary(cal_wgt_model)
# Diagnostic Plots:
# Residual Plots, QQ-Plos, Std. Residuals vs Fitted
plot(cal_wgt_model) 
# Residuals vs Regressors
library(car)
residualPlots(cal_wgt_model)
# Added Variable Plots
avPlots(cal_wgt_model)
# QQ plots of studentized residuals
qqPlot(cal_wgt_model)
# Deletion Diagnostics
influenceIndexPlot(cal_wgt_model) # Index Plots of the influence measures


### 1st Iteration 
### Transformation of models data
calories_consumed1["Calories.Consumed2"] <- calories_consumed$Calories.Consumed*calories_consumed$Calories.Consumed
calories_consumed2 <- calories_consumed1[-c(7,9),]
# Preparing new model after transformation 
cal_wgt_model1 <- lm(Weight.gained..grams.~.,data = calories_consumed2)
summary(cal_wgt_model1)

# Diagnostic Plots:
# Residual Plots, QQ-Plos, Std. Residuals vs Fitted
plot(cal_wgt_model1) 
# Residuals vs Regressors
library(car)
residualPlots(cal_wgt_model1)
# Added Variable Plots
avPlots(cal_wgt_model1)
# QQ plots of standardized residuals
qqPlot(cal_wgt_model1)
# Deletion Diagnostics
influenceIndexPlot(cal_wgt_model1) # Index Plots of the influence measures


### 2nd Iteration
## Remove the 10th observation 
calories_consumed3 <- calories_consumed2[-c(1,10,14),]
 
## Prepare second model for predicting weights  
cal_wgt_model2 <- lm(Weight.gained..grams.~.,data = calories_consumed3)
summary(cal_wgt_model2)

plot(cal_wgt_model2 ) 
residualPlots(cal_wgt_model2)
qqPlot(cal_wgt_model2 )
influenceIndexPlot(cal_wgt_model2)

### Iteration 3
## Remove the outliers 
calories_consumed4 <- calories_consumed3[-c(6,10,14)]

## Prepare second model for predicting weights  
cal_wgt_model3 <- lm(Weight.gained..grams.~.,data = calories_consumed4)
summary(cal_wgt_model3)

plot(cal_wgt_model3) 
residualPlots(cal_wgt_model3)
qqPlot(cal_wgt_model3)
influenceIndexPlot(cal_wgt_model3)

### Iteration 4
calories_consumed5 <- calories_consumed4[-c(6,10,14),]

## Prepare second model for predicting weights  
cal_wgt_model4 <- lm(Weight.gained..grams.~.,data = calories_consumed5)
summary(cal_wgt_model4)

plot(cal_wgt_model4) 
residualPlots(cal_wgt_model4)
qqPlot(cal_wgt_model4)
influenceIndexPlot(cal_wgt_model4)


# Final Prediction weights for consuming calories from historical data
final_pred_wgt <- predict(cal_wgt_model4)
# Print predicted value
final_pred_wgt
# Prepare  final data  with data frame of historical data, predicted weights and error
final_data <- data.frame(calories_consumed5,final_pred_wgt,
                        "Error"=calories_consumed5$Weight.gained..grams.-final_pred_wgt)
final_data



## As per boxplot visualization there is no outliers in data set & also summary 
## of model states that there is no insignificant variable(no colinearity problem in data) 
## in data set.But As per residuals plots error had pattern in input variables data   
## then we transformed the input variable & got the increased multiple R^2 value(0.9827) 
## or 98.27% increased model accuracy. 





