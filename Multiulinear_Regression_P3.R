

######### Prepare a Multilinear regression model for predicting Price ############

#Variable
#Price(Y)
#Age
#Kilometers
#HP
#Gears
#CC
#Doors
#QuartTax
#Weight
#Description
#Offer price in Euros
#Age in months as on August 2004
#Accumulated kilometers on odometer
#Horsepower
#Number of gears
#Cylinder volume in cubic centimeters
#Number of doors
#Quarterly Road Tax 
#Weight in kilograms


## Load "ToyotaCorolla" dataset or Import it in global environment 
toyota_r<-read.csv("File Path")

# Check if NA values are there
colSums(is.na(toyota_r))
toyota<-na.omit(toyota_r)
colnames(toyota)
toyota<-toyota[,c(3,4,7,9,16,13,14,17,18)]

# toyota1<-subset(toyota,select = c("Price","cc") )
# Scatter Plot Matrix:
pairs(toyota)
#Correlation Matrix:
cor(toyota)

# Normalize the data 
toyota1 <- log(toyota)

# Multilinear Regression Model and Summary
model.car<-lm(Price~Age_08_04+KM+HP+Gears+cc+Doors+Quarterly_Tax+Weight,data = toyota1)
summary(model.car)


# Full Model Building process
# Multilinear Regression Model and Summary
model.Toyota<-lm(Price~.,data = toyota)
summary(model.car)

# Model Validation
# Diagnostic Plots:
# Residual Plots, QQ-Plos, Std. Residuals vs Fitted
plot(model.car) 
# Residuals vs Regressers
library(car)
residualPlots(model.car)
# Added Variable Plots
avPlots(model.car)
# QQ plots of standardized residuals
qqPlot(model.car)
# Deletion Diagnostics
influenceIndexPlot(model.car) # Index Plots of the influence measures


#### Iteration 1 
# Remove 81th observation
toyota1['Age2']<-toyota1$Age_08_04*toyota1$Age_08_04
toyota1['KM2']<-toyota1$KM*toyota1$KM
toyota1['Gears2']<-toyota1$Gears*toyota1$Gears
toyota1['cc2']<-toyota1$cc*toyota1$cc
toyota1['Doors2']<-toyota1$Doors*toyota1$Doors
toyota1['Weight2']<-toyota1$Weight*toyota1$Weight
toyota1['Quarterly_Tax2'] <- toyota1$Quarterly_Tax*toyota1$Quarterly_Tax

toyota2<-toyota1[-c(81),]
model.car1<-lm(Price~.,data = toyota2)
summary(model.car1)


plot(model.car1) 
library(car)
residualPlots(model.car1)
qqPlot(model.car1)
influenceIndexPlot(model.car1)



## Iteration 2 
toyota2['Age3']<-toyota2$Age2*toyota2$Age2
toyota2['KM3'] <- toyota2$KM2*toyota2$KM2
toyota2['Gears3'] <- toyota2$Gears2*toyota2$Gears2



model.car2<-lm(Price~.,data = toyota2)
summary(model.car2)
plot(model.car2)
library(car)
residualPlots(model.car2)


# Final Prediction price for toyota_corolla from historical data
final_pred_price <- predict(model.car2)
# Print predicted value
final_pred_price

# Prepare  final data  with data frame of historical data, predicted weights and error
final_data <- data.frame(toyota2,final_pred_price,
                         "Error"=toyota2$Price-final_pred_price)
final_data
