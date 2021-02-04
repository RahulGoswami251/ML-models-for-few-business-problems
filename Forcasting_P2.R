


############ Forecast the Coca Cola sales prices for next 4quater ############

## Load & View the the data
# Load "CocaCola_Sales_Rawdata" dataset
install.packages("readxl")
library(readxl)
CocaCola_Sales_Rawdata <- read_xlsx("File Path")
# Set the environment 
install.packages("Metrics")
library(Metrics)
plot(CocaCola_Sales_Rawdata$Sales,type="l")

# So creating 4 dummy variables 
# Creating dummies for 4quarter
Q1 <-  ifelse(grepl("Q1",CocaCola_Sales_Rawdata$Quarter),'1','0')
Q2 <-  ifelse(grepl("Q2",CocaCola_Sales_Rawdata$Quarter),'1','0')
Q3 <-  ifelse(grepl("Q3",CocaCola_Sales_Rawdata$Quarter),'1','0')
Q4 <-  ifelse(grepl("Q4",CocaCola_Sales_Rawdata $Quarter),'1','0')

cocacola_salesd <- cbind(CocaCola_Sales_Rawdata,Q1,Q2,Q3,Q4)
View(cocacola_salesd)

## Find some values for making  
cocacola_salesd["t"]<- 1:42
# Find t^2 value 
cocacola_salesd["t_square"]<-cocacola_salesd["t"]*cocacola_salesd["t"]
# trakdata["log_sales"]<-log(trakdata["Sales"])
cocacola_salesd["log_sales"] <- log(cocacola_salesd["Sales"])
View(cocacola_salesd)
## Data Partition
train<-cocacola_salesd[1:36,]
test<-cocacola_salesd[37:42,]


########################### LINEAR MODEL #############################

linear_model<-lm(Sales~t,data=train)

linear_pred<-data.frame(predict(linear_model,interval='predict',newdata =test))
View(linear_pred)
rmse_linear<-rmse(test$Sales,linear_pred$fit)
rmse_linear

######################### Exponential #################################


expo_model<-lm(log_sales~t,data=train)

expo_pred<-data.frame(predict(expo_model,interval='predict',newdata=test))
rmse_expo<-rmse(test$Sales,exp(expo_pred$fit))
rmse_expo 

######################### Quadratic ####################################

Quad_model<-lm(Sales~t+t_square,data=train)

Quad_pred<-data.frame(predict(Quad_model,interval='predict',newdata=test))
rmse_Quad<-rmse(test$Sales,Quad_pred$fit)
rmse_Quad 

######################### Additive Seasonality #########################

sea_add_model<-lm(Sales~Q1+Q2+Q3+Q4,data=train)

sea_add_pred<-data.frame(predict(sea_add_model,newdata=test,interval='predict'))
rmse_sea_add<-rmse(test$Sales,sea_add_pred$fit)
rmse_sea_add

######################## Additive Seasonality with Linear #################

Add_sea_Linear_model<-lm(Sales~t+Q1+Q2+Q3+Q4,data=train)

summary(Add_sea_Linear_model)
Add_sea_Linear_pred<-data.frame(predict(Add_sea_Linear_model,interval='predict',newdata=test))
rmse_Add_sea_Linear<-rmse(test$Sales,Add_sea_Linear_pred$fit)
rmse_Add_sea_Linear

######################## Additive Seasonality with Quadratic #################

Add_sea_Quad_model<-lm(Sales~t+t_square+Q1+Q2+Q3+Q4,data=train)

Add_sea_Quad_pred<-data.frame(predict(Add_sea_Quad_model,interval='predict',newdata=test))
rmse_Add_sea_Quad<-rmse(test$Sales,Add_sea_Quad_pred$fit)

rmse_Add_sea_Quad 

######################## Multiplicative Seasonality #########################

multi_sea_model<-lm(log_sales~Q1+Q2+Q3+Q4,data = train)

multi_sea_pred<-data.frame(predict(multi_sea_model,newdata=test,interval='predict'))
rmse_multi_sea<-rmse(test$Sales,exp(multi_sea_pred$fit))

rmse_multi_sea

######################## Multiplicative Seasonality Linear trend ##########################

multi_add_sea_model<-lm(log_sales~t+Q1+Q2+Q3+Q4,data = train)
summary(multi_add_sea_model) 
multi_add_sea_pred<-data.frame(predict(multi_add_sea_model,newdata=test,interval='predict'))
rmse_multi_add_sea<-rmse(test$Sales,exp(multi_add_sea_pred$fit))

rmse_multi_add_sea 

# Preparing table on model and it's RMSE values 

table_rmse<-data.frame('Model'=c("rmse_linear","rmse_expo","rmse_Quad","rmse_sea_add","rmse_Add_sea_Linear","rmse_Add_sea_Quad","rmse_multi_sea","rmse_multi_add_sea"),'RMSE'=c(rmse_linear,rmse_expo,rmse_Quad,rmse_sea_add,rmse_Add_sea_Linear,rmse_Add_sea_Quad,rmse_multi_sea,rmse_multi_add_sea))

colnames(table_rmse)<-c("model","RMSE")
View(table_rmse)

# Use entire data : Additive Seasonality with Quadratic  has least RMSE value
Final_model <- lm(Sales~t+t_square+Q1+Q2+Q3+Q4,data=cocacola_salesd)

# Getting residuals 
resid <- residuals(Final_model)
acf(resid,lag.max = 10)

# By principal of parcimony we will consider lag -1 as we have so 
# many significant lags 
# Building Autoregressive model on residuals consider lags 1 
k <- arima(resid, order=c(1,0,0))
pred_res<- predict(arima(resid,order=c(1,0,0)),n.ahead = 4)
str(pred_res)
pred_res$pred
acf(k$residuals)


####################### Predicting new data #############################
multi_add_sea_pred<-data.frame(predict(Final_model,newdata=cocacola_salesd,interval='predict'))
Final_model_fit <- Final_model$fitted.values
Quarter <- as.data.frame(cocacola_salesd$Quarter)
Final <- as.data.frame(cbind(Quarter,cocacola_salesd$Sales,Final_model_fit))



## ARIMA model or data driven approach 
## Load "CocaCola_Sales_Rawdata" dataset
install.packages("readxl")
library(readxl)
CocaCola_Sales_Rawdata <- read_xlsx("File Path")

## Set the environment
install.packages(c("forecast","fpp","smooth","tseries"))
library(forecast)
library(fpp)
library(smooth)
library(tseries)

# Converting data into time series object
CCS<-ts(CocaCola_Sales_Rawdata$Sales,frequency =4 ,start=c(86))
View(CCS)
plot(CCS)
# Dividing entire data into training and testing data 
train<-CCS[1:38]
test<- CCS[39:42]

# Converting time series object
train<-ts(train,frequency = 4)
test<-ts(test,  frequency = 4)
plot(train)
acf(train)
pacf(train)

# Auto.Arima model on the data 
library(forecast)
model_CCS <- auto.arima(train) 
summary(model_CCS)

# Checking the residuals of model 
acf(model_CCS$residuals)
## Forecasting the passenger traveled for next 12month 
windows()
plot(forecast(model_CCS,h=4),xaxt="n") # xaxt- x axis text



















