

########## Delivery_time ->Prediction delivery time using sorting time ############ 

# Load "delivery_time" dataset 
Delivery_Time <- read.csv("File Path")

## Visualization of data 
install.packages("lattice")
library(lattice)
dotchart(Delivery_Time$Sorting.Time,main = 'sorting time', color = blues9)
dotchart(Delivery_Time$Delivery.Time,main = 'delivery time', color = blues9)
dotplot(Delivery_Time$Sorting.Time,main = 'sorting time', col = 'green')
dotplot(Delivery_Time$Delivery.Time,main = 'delivery time', col='red')
barplot(Delivery_Time$Sorting.Time, main = 'sorting time', col='blue')
barplot(Delivery_Time$Delivery.Time,main = 'delivery time', col='green')
boxplot(Delivery_Time$Sorting.Time, main='sorting time')
boxplot(Delivery_Time$Delivery.Time, main='delivery time')
hist(Delivery_Time$Sorting.Time, main = 'sorting time', col = 'blue')
hist(Delivery_Time$Delivery.Time, main = 'delivery time', col = 'purple')

# Column names
colnames(Delivery_Time)

# Normalize the data 
Delivery_Time1 <- log(Delivery_Time)


# Simple Linear Regression Model and Summary
Del_Sort_time <- lm(Delivery.Time~Sorting.Time, data = Delivery_Time1)
summary(Del_Sort_time)



# Full Model Building process
# Regression Model and Summary
Del_Sort_time <- lm(Delivery.Time~., data = Delivery_Time1)
summary(Del_Sort_time)
# Diagnostic Plots:
# Residual Plots, QQ-Plos, Std. Residuals vs Fitted
plot(Del_Sort_time) 
# Residuals vs Regressors
library(car)
residualPlots(Del_Sort_time)
# Added Variable Plots
avPlots(Del_Sort_time)
# QQ plots of stadentized residuals
qqPlot(Del_Sort_time)
# Deletion Diagnostics
influenceIndexPlot(Del_Sort_time) # Index Plots of the influence measures

### 1st Iteration 
### Transformation of models data
Delivery_Time1["Sorting.Time2"] <- Delivery_Time1$Sorting.Time*Delivery_Time1$Sorting
Delivery_Time2 <- Delivery_Time1[-c(21,5),]

# Preparing new model after transformation 
Del_Sort_time1 <- lm(Delivery.Time~., data = Delivery_Time2)
summary(Del_Sort_time)

# Diagnostic Plots:
# Residual Plots, QQ-Plos, Std. Residuals vs Fitted
plot(Del_Sort_time1) 
# Residuals vs Regressors 
library(car)
residualPlots(Del_Sort_time1)
# Added Variable Plots
avPlots(Del_Sort_time1)
# QQ plots of stadentized residuals
qqPlot(Del_Sort_time1)
# Deletion Diagnostics
influenceIndexPlot(Del_Sort_time1) # Index Plots of the influence measures

### 2nd Iteration
## Remove the 5th observation & Transformation of input variable
Delivery_Time3 <- Delivery_Time2[-c(3,4),]
Delivery_Time2["Sorting.Time3"] <- Delivery_Time2$Sorting.Time2*Delivery_Time2$Sorting.Time2

# Preparing new model after transformation 
Del_Sort_time2 <- lm(Delivery.Time~., data = Delivery_Time3)
summary(Del_Sort_time2)

plot(Del_Sort_time2 ) 
residualPlots(Del_Sort_time2 )
qqPlot(Del_Sort_time2 )
influenceIndexPlot(Del_Sort_time2 )


# Final Prediction weights for consuming calories from historical data
final_pred_del_time <- predict(Del_Sort_time2)
# Print predicted value
final_pred_del_time

# Prepare final data  with data frame of historical data, predicted delivery time and error
final_data <- data.frame(Delivery_Time3,final_pred_del_time,
                         "Error"=Delivery_Time3$Delivery.Time - final_pred_del_time)
final_data



## As per boxplot visualization there is no outliers in data set & also summary 
## of model states that there is no insignificant variable(no colinearity problem in data) 
## in data set.But As per residuals plots error had pattern in input variables data   
## then we transformed the input variable & got the increased multiple R^2 value(0.8807) 
## or 88.07% increased model accuracy. 





