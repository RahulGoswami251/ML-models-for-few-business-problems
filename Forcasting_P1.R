


############ Forecast the Airlines Passengers for next 12months ############

## Load "Airlines+Data" dataset
install.packages("readxl")
library(readxl)
Airlines_Data <- read_xlsx("File path")


# Set the environment 
install.packages("Metrics")
library(Metrics)
# Seasonality 12 months
plot(Airlines_Data$Passengers,type="l")

# So creating 12 dummy variables 
X<- data.frame(outer(rep(month.abb,length = 96), month.abb,"==") + 0 )

# Assigning month names 
colnames(X)<-month.abb 
View(X)
ADdata<-cbind(Airlines_Data,X)
View(ADdata)
# Find t value 
ADdata["t"]<- 1:96
View(ADdata)
# Find t^2 value  
ADdata["t_square"]<-ADdata["t"]*ADdata["t"]

# ADdata["log_Passengers"]<-log(ADdata["Passengers"])
ADdata["log_Passengers"]<-log(ADdata["Passengers"])

## Data Partition
train<-ADdata[1:84,]
test<-ADdata[85:96,]



########################### LINEAR MODEL #############################

linear_model<-lm(Passengers~t,data=train)
linear_pred<-data.frame(predict(linear_model,interval='predict',newdata =test))
View(linear_pred)
rmse_linear<-rmse(test$Passengers,linear_pred$fit)
rmse_linear

######################### Exponential #################################


expo_model<-lm(log_Passengers~t,data=train)

expo_pred<-data.frame(predict(expo_model,interval='predict',newdata=test))
rmse_expo<-rmse(test$Passengers,exp(expo_pred$fit))

rmse_expo 

######################### Quadratic ####################################

Quad_model<-lm(Passengers~t+t_square,data=train)

Quad_pred<-data.frame(predict(Quad_model,interval='predict',newdata=test))
rmse_Quad<-rmse(test$Passengers,Quad_pred$fit)
rmse_Quad 

######################### Additive Seasonality #########################

sea_add_model<-lm(Passengers~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
sea_add_pred<-data.frame(predict(sea_add_model,newdata=test,interval='predict'))
rmse_sea_add<-rmse(test$Passengers,sea_add_pred$fit)

rmse_sea_add

######################## Additive Seasonality with Linear #################

Add_sea_Linear_model<-lm(Passengers~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(Add_sea_Linear_model)
Add_sea_Linear_pred<-data.frame(predict(Add_sea_Linear_model,interval='predict',newdata=test))
rmse_Add_sea_Linear<-rmse(test$Passengers,Add_sea_Linear_pred$fit)

rmse_Add_sea_Linear

######################## Additive Seasonality with Quadratic #################

Add_sea_Quad_model<-lm(Passengers~t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)

Add_sea_Quad_pred<-data.frame(predict(Add_sea_Quad_model,interval='predict',newdata=test))
rmse_Add_sea_Quad<-rmse(test$Passengers,Add_sea_Quad_pred$fit)

rmse_Add_sea_Quad 

######################## Multiplicative Seasonality #########################

multi_sea_model<-lm(log_Passengers~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)

multi_sea_pred<-data.frame(predict(multi_sea_model,newdata=test,interval='predict'))
rmse_multi_sea<-rmse(test$Passengers,exp(multi_sea_pred$fit))

rmse_multi_sea

######################## Multiplicative Seasonality Linear trend ##########################

multi_add_sea_model<-lm(log_Passengers~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(multi_add_sea_model) 
multi_add_sea_pred<-data.frame(predict(multi_add_sea_model,newdata=test,interval='predict'))
rmse_multi_add_sea<-rmse(test$Passengers,exp(multi_add_sea_pred$fit))

rmse_multi_add_sea 

# Preparing table on model and it's RMSE values 

table_rmse<-data.frame('Model'=c("rmse_linear","rmse_expo","rmse_Quad","rmse_sea_add","rmse_Add_sea_Linear","rmse_Add_sea_Quad","rmse_multi_sea","rmse_multi_add_sea"),'RMSE'=c(rmse_linear,rmse_expo,rmse_Quad,rmse_sea_add,rmse_Add_sea_Linear,rmse_Add_sea_Quad,rmse_multi_sea,rmse_multi_add_sea))

colnames(table_rmse)<-c("model","RMSE")
View(table_rmse)

# Use entire data : Multiplicative Seasonality with Linear trend has least RMSE value
Final_model <- lm(Passengers~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=ADdata)

# Getting residuals 
resid <- residuals(Final_model)
acf(resid,lag.max = 10)

# By principal of parsimony we will consider 3-lags  as we have so many 
# significant lags 
# Building Autoregressive model on residuals consider 3-lags 
k <- arima(resid, order=c(3,0,0))
pred_res<- predict(arima(resid,order=c(3,0,0)),n.ahead = 12)
str(pred_res)
pred_res$pred
acf(k$residuals)

####################### Predicting new data #############################
library(readxl)
test_data<-read_excel(file.choose(),1) #Load Predict_new.xlsx

View(test_data)
# test_data<-Predict_new
pred_new<-data.frame(predict(Final_model,newdata=test_data,interval = 'predict'))
View(pred_new)
# pred_re<-pred_res$pred[1:12]
pred_new$fit <- pred_new$fit+pred_res$pred
View(pred_new)



## ARIMA model or Data driven approach  
## Load "Airlines+Data" dataset
install.packages("readxl")
library(readxl)
Airlines_Data <- read_xlsx("File path")

## Set the environment
install.packages(c("forecast","fpp","smooth","tseries"))
library(forecast)
library(fpp)
library(smooth)
library(tseries)

# Converting data into time series object
pagrs<-ts(Airlines_Data$Passengers,frequency =12 ,start=c(1995))
View(pagrs)
plot(pagrs)
# Dividing entire data into training and testing data 
train<-pagrs[1:84]
test<- pagrs[85:96]

# Converting time series object
train<-ts(train,frequency = 12)
test<-ts(test,frequency = 12)
plot(train)
acf(train)
pacf(train)

# Auto.Arima model on the data 
library(forecast)
model_AA <- auto.arima(train) 
summary(model_AA)

# Checking the residuals of model 
acf(model_AA$residuals)
## Forecasting the passenger traveled for next 12month 
windows()
plot(forecast(model_AA,h=12),xaxt="n") # xaxt- x axis text







