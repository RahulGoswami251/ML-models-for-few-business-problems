

##################### Prepare Multilinear regression model for Predicting Price of the computer #########################


## Load "Computer_Data" dataset or import it in dataset
computer_d <- read.csv("File Path")

## Convert categorical column state of data set into numerical 
computer_d$cd <- as.factor(computer_d$cd)
computer_d$cd

computer_d$cd <- as.numeric(factor(computer_d$cd))
computer_d$cd


computer_d$multi <- as.factor(computer_d$multi)
computer_d$multi

computer_d$multi <- as.numeric(factor(computer_d$multi))
computer_d$multi



computer_d$premium <- as.factor(computer_d$premium)
computer_d$premium

computer_d$premium <- as.numeric(factor(computer_d$premium))
computer_d$premium

# Scatter Plot Matrix:
pairs(computer_d)

# Correlation Matrix:###
cor(computer_d)

## After correlation we remove few columns which are not important 
computer_d1 <- computer_d[, c(-6,-7,-8)]
computer_d1 

## Visualization
install.packages("lattice")
library(lattice)
dotchart(computer_d$price, main='prices',xlab='prices', color=blues9)
dotchart(computer_d$speed, main = 'speed', xlab = 'speed', color = blues9)
dotchart(computer_d$hd, main = 'hd', xlab = 'hd', color = blues9)
dotchart(computer_d$ram, main = 'ram', xlab = 'ram size', color = blues9)
dotchart(computer_d$screen, main = 'screen', xlab = 'screen size', color = blues9)
dotchart(computer_d$cd, main = 'cd', xlab = 'cd', color = blues9)
dotchart(computer_d$multi, main = 'multi', xlab = 'multi', color = blues9)
dotchart(computer_d$premium, main = 'premium', xlab = 'premium', color = blues9)
dotchart(computer_d$ads, main = 'ads', xlab = 'ads', color = blues9)
dotchart(computer_d$trend, main = 'trend', xlab = 'trend', color = blues9)

boxplot(computer_d$price, main='price')
boxplot(computer_d$speed, main = 'speed')
boxplot(computer_d$hd, main = 'hd')
boxplot(computer_d$ram, main = 'ram')
boxplot(computer_d$screen, main = 'screen')
boxplot(computer_d$ads, main = 'ads')
boxplot(computer_d$trend, main = 'trend')

# Normalize the data 
computer_d2 <- sqrt(computer_d1)
# Check the NA values 
sum(is.na(computer_d2))
## MultiRegression model & summary
computer_pr <- lm(price~speed+hd+ram+screen+ads+trend, data=computer_d2) 
summary(computer_pr)

# Multi-colinearity
install.packages("car")
library(car)
# Variance Inflation Factor
car::vif(computer_pr)
## Subset selection
library(MASS)
stepAIC(computer_pr)



## Full Model Building Process
computer_pr <- lm(price~., data=computer_d2 ) 
summary(computer_pr)
## Digonastic Plots
## Residual Plots, QQ-Plots, Std. Residual Vs Fitted 
plot(computer_pr)
# Residual Plot
library(car)
residualPlots(computer_pr)
# QQ-Plots
qqPlot(computer_pr)
# Deletion Diagnostics
influenceIndexPlot(computer_pr)

## 1st Iteration 
#  Tranformation of data input varaiable & Remove outliers
outlierTest(computer_pr)
computer_d3 <- computer_d2[-c(1701,1441,4410,4092),]
computer_d2["speed2"] <- computer_d2$speed*computer_d2$speed
computer_d2["hd2"] <- computer_d2$hd*computer_d2$hd
computer_d2["ram2"] <- computer_d2$ram*computer_d2$ram
computer_d2["screen2"] <- computer_d2$screen*computer_d2$screen
computer_d2["ads2"] <- computer_d2$ads*computer_d2$ads
computer_d2["trend2"] <- computer_d2$trend*computer_d2$trend


## Prepare New Model after data input variable transformation 
computer_pr1 <- lm(price~., data=computer_d3) 
summary(computer_pr1)

plot(computer_pr1) 
residualPlots(computer_pr1)
qqPlot(computer_pr1)
influenceIndexPlot(computer_pr1)


## 2nd Iteration 
#  Transformation of data input variable & Remove outliers
outlierTest(computer_pr1)
computer_d4 <- computer_d3[-c(3991,4074,4283),]
computer_d3["speed3"] <- computer_d3$speed*computer_d3$speed
computer_d3["hd3"] <- computer_d3$hd*computer_d3$hd
computer_d3["ram3"] <- computer_d3$ram*computer_d3$ram
computer_d3["screen3"] <- computer_d3$screen*computer_d3$screen
computer_d3["ads3"] <- computer_d3$ads*computer_d3$ads
computer_d3["trend3"] <- computer_d3$trend*computer_d3$trend



## Prepare New Model after data input varaiable transformation 
computer_pr2 <- lm(price~., data=computer_d4 ) 
summary(computer_pr2)

plot(computer_pr2) 
residualPlots(computer_pr2)
qqPlot(computer_pr2)
influenceIndexPlot(computer_pr2)

## 3rd Iteration 
#  Transformation of data input variable & Remove outliers
outlierTest(computer_pr2)
computer_d5 <- computer_d4[-c(3991,4074,4283,4686,2820),]
computer_d4["speed4"] <- computer_d4$speed*computer_d4$speed
computer_d4["hd4"] <- computer_d4$hd*computer_d4$hd
computer_d4["ram4"] <- computer_d4$ram*computer_d4$ram
computer_d4["screen4"] <- computer_d4$screen*computer_d4$screen
computer_d4["ads4"] <- computer_d4$ads*computer_d4$ads
computer_d4["trend4"] <- computer_d4$trend*computer_d4$trend

## Prepare New Model after data input variable transformation 
computer_pr3 <- lm(price~., data=computer_d5 ) 
summary(computer_pr3)

plot(computer_pr3) 
residualPlots(computer_pr3)
qqPlot(computer_pr3)
influenceIndexPlot(computer_pr3)


# Final Prediction price for computer from historical data
final_pred_price <- predict(computer_pr3)
# Print predicted value
final_pred_price

# Prepare  final data  with data frame of historical data, predicted weights and error
final_data <- data.frame(computer_d5,final_pred_price,
                         "Error"=computer_d5$price-final_pred_price)
final_data



