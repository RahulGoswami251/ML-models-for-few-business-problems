

################### A prediction model for profit of 50_startups data ###################


# Load "50_Startups" dataset or import it in global environment 
fifty_start_up <- read.csv("File Path")

## Convert categorical column state of data set into numerical 
fifty_start_up$State <- as.factor(fifty_start_up$State)
fifty_start_up$State

fifty_start_up$State <- as.numeric(factor(fifty_start_up$State))
fifty_start_up$State


# Visualization
install.packages("lattice")
library(lattice)
dotchart(fifty_start_up$R.D.Spend,main = 'R&D Spend',xlab='R&D expenditure', color = blues9)
dotchart(fifty_start_up$Administration, main = 'Administration spend', xlab='Administraion expenditure', color = blues9)
dotchart(fifty_start_up$Marketing.Spend, main='Marketing spend', xlab = 'Marketing Expenditure',color = blues9)
dotchart(fifty_start_up$State, main = 'State',xlab = 'number of state', color = blues9)
dotchart(fifty_start_up$Profit, main = 'profit', xlab = 'profit anmount',color = blues9)

dotplot(fifty_start_up$R.D.Spend, main= 'R.D. spend', xlab = 'R & D Expenditure',col='orange')
dotplot(fifty_start_up$Administration, main='Administration spend',xlab = 'Administration expenditure', col= 'blue')
dotplot(fifty_start_up$Marketing.Spend, main='Marketing spend',xlab = 'marketing expenditure', col= 'purple')
dotplot(fifty_start_up$State, main='State',xlab = 'number of state', col='green')
dotplot(fifty_start_up$Profit, main='Profit', xlab = 'profit amount', col='blue')

barplot(fifty_start_up$R.D.Spend, main= 'R.D. spend', ylab = 'R & D Expenditure',col='orange')
barplot(fifty_start_up$Administration, main='Administration spend',ylab = 'Administration expenditure', col= 'blue')
barplot(fifty_start_up$Marketing.Spend, main='Marketing spend',ylab = 'marketing expenditure', col= 'purple')
barplot(fifty_start_up$State, main='State',ylab = 'number of state', col='green')
barplot(fifty_start_up$Profit, main='Profit', ylab = 'profit amount', col='blue')

boxplot(fifty_start_up$R.D.Spend, main= 'R.D. spend',ylab='R&D expediture')
boxplot(fifty_start_up$Administration, main='Administration spend',ylab='administration expenditure')
boxplot(fifty_start_up$Marketing.Spend, main='Marketing spend', ylab='marketing expenditure')
boxplot(fifty_start_up$State, main='State', ylab='number of state')
boxplot(fifty_start_up$Profit, main='Profit', ylab='profit amount')

hist(fifty_start_up$R.D.Spend, main= 'R.D. spend', xlab = 'R & D Expenditure',col='orange')
hist(fifty_start_up$Administration, main='Administration spend',xlab = 'Administration expenditure', col= 'blue')
hist(fifty_start_up$Marketing.Spend, main='Marketing spend',xlab = 'marketing expenditure', col= 'purple')
hist(fifty_start_up$State, main='State',xlab = 'number of state', col='green')
hist(fifty_start_up$Profit, main='Profit', xlab = 'profit amount', col='blue')

# Normalize the data 
install.packages("rcompanion")
library(rcompanion)
fifty_start_up1 <- sqrt(fifty_start_up)



## Multilinear Regression model & summary
fifty_st_up <- lm(Profit~R.D.Spend+Administration+Marketing.Spend+State,data=fifty_start_up1)
summary(fifty_st_up)
# Multi-colinearity
install.packages("car")
library(car)
# Variance Inflation Factor
car::vif(fifty_st_up)
##Subset selection
library(MASS)
stepAIC(fifty_st_up)


## Full Model Building Process
fifty_st_up <- lm(Profit~.,data=fifty_start_up1)
summary(fifty_st_up)
## Digonastic Plots
## Residual Plots, QQ-Plots, Std. Residual Vs Fitted 
plot(fifty_st_up)
# Residual Plot
residualPlots(fifty_st_up)
# QQ-Plots
qqPlot(fifty_st_up)
# Deletion Diagnostics
influenceIndexPlot(fifty_st_up)


## 1st Iteration 
# Transformation of data input variable
fifty_start_up1["R.D.Spend2"] <- fifty_start_up1$R.D.Spend*fifty_start_up1$R.D.Spend
fifty_start_up1["Administration2"] <- fifty_start_up1$Administration*fifty_start_up1$Administration
fifty_start_up1["Marketing.Spend2"] <- fifty_start_up1$Marketing.Spend*fifty_start_up1$Marketing.Spend


## Prepare New Model after data input variable transformation 
fifty_st_up1 <- lm(Profit~.,data=fifty_start_up1)
summary(fifty_st_up1)

plot(fifty_st_up1) 
residualPlots(fifty_st_up1)
qqPlot(fifty_st_up1 )
influenceIndexPlot(fifty_st_up1)

## 2nd Iteration 
# Transformation of data input variable
fifty_start_up1["R.D.Spend3"] <- fifty_start_up1$R.D.Spend2*fifty_start_up1$R.D.Spend2
fifty_start_up1["Administration3"] <- fifty_start_up1$Administration2*fifty_start_up1$Administration2
fifty_start_up1["Marketing.Spend3"] <- fifty_start_up1$Marketing.Spend2*fifty_start_up1$Marketing.Spend2
fifty_start_up2 <- fifty_start_up1[-c(46,50),]

## Prepare New Model after data input variable transformation 
fifty_st_up2 <- lm(Profit~.,data=fifty_start_up2)
summary(fifty_st_up2)

plot(fifty_st_up2) 
residualPlots(fifty_st_up2)
qqPlot(fifty_st_up2)
influenceIndexPlot(fifty_st_up2)

## Prediction of profit for 50_start_ups 
Predict_profits <- predict(fifty_st_up2) 
Predict_profits

# Prepare  final data  with data frame of historical data, predicted weights and error
final_data <- data.frame(fifty_start_up2,Predict_profits,
                         "Error"=fifty_start_up2$Profit - Predict_profits)
final_data


## Table of R^2 value as per build models 
R_square_model <- data.frame(Name=c("fifty_st_up","fifty_st_up1","fifty_st_up2"),
                         Rsquare_value = c(0.9313,0.9459,0.9747))
R_square_model










