

############ Forecast the Plastic Sales  for next 12months ############


## Load "PlasticSales" dataset
Plastic_sales <- read.csv("File Path")

# Set the environment 
install.packages("Metrics")
library(Metrics)
# Seasonality 12 months
plot(Plastic_sales$Sales,type="l")

# So creating 12 dummy variables 
X<- data.frame(outer(rep(month.abb,length = 60), month.abb,"==") + 0 )

# Assigning month names 
colnames(X)<-month.abb 
View(X)
PSdata<-cbind(Plastic_sales,X)
View(PSdata)
# Find t value 
PSdata["t"]<- 1:60
View(PSdata)
# Find t^2 value  
PSdata["t_square"]<-PSdata["t"]*PSdata["t"]

# ADdata["log_Sales"]<-log(ADdata["Sales"])
PSdata["log_Sales"]<-log(PSdata["Sales"])

## Data Partition
train<-PSdata[1:48,]
test<-PSdata[49:60,]


########################### LINEAR MODEL #############################

linear_model<-lm(Sales~t,data=train)
linear_pred<-data.frame(predict(linear_model,interval='predict',newdata =test))
View(linear_pred)
rmse_linear<-rmse(test$Sales,linear_pred$fit)
rmse_linear

######################### Exponential #################################


expo_model<-lm(log_Sales~t,data=train)

expo_pred<-data.frame(predict(expo_model,interval='predict',newdata=test))
rmse_expo<-rmse(test$Sales,exp(expo_pred$fit))

rmse_expo 

######################### Quadratic ####################################

Quad_model<-lm(Sales~t+t_square,data=train)

Quad_pred<-data.frame(predict(Quad_model,interval='predict',newdata=test))
rmse_Quad<-rmse(test$Sales,Quad_pred$fit)
rmse_Quad 

######################### Additive Seasonality #########################

sea_add_model<-lm(Sales~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
sea_add_pred<-data.frame(predict(sea_add_model,newdata=test,interval='predict'))
rmse_sea_add<-rmse(test$Sales,sea_add_pred$fit)

rmse_sea_add

######################## Additive Seasonality with Linear #################

Add_sea_Linear_model<-lm(Sales~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(Add_sea_Linear_model)
Add_sea_Linear_pred<-data.frame(predict(Add_sea_Linear_model,interval='predict',newdata=test))
rmse_Add_sea_Linear<-rmse(test$Sales,Add_sea_Linear_pred$fit)

rmse_Add_sea_Linear

######################## Additive Seasonality with Quadratic #################

Add_sea_Quad_model<-lm(Sales~t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)

Add_sea_Quad_pred<-data.frame(predict(Add_sea_Quad_model,interval='predict',newdata=test))
rmse_Add_sea_Quad<-rmse(test$Sales,Add_sea_Quad_pred$fit)

rmse_Add_sea_Quad 

######################## Multiplicative Seasonality #########################

multi_sea_model<-lm(log_Sales~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)

multi_sea_pred<-data.frame(predict(multi_sea_model,newdata=test,interval='predict'))
rmse_multi_sea<-rmse(test$Sales,exp(multi_sea_pred$fit))

rmse_multi_sea

######################## Multiplicative Seasonality Linear trend ##########################

multi_add_sea_model<-lm(log_Sales~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(multi_add_sea_model) 
multi_add_sea_pred<-data.frame(predict(multi_add_sea_model,newdata=test,interval='predict'))
rmse_multi_add_sea<-rmse(test$Sales,exp(multi_add_sea_pred$fit))

rmse_multi_add_sea 

# Preparing table on model and it's RMSE values 

table_rmse<-data.frame('Model'=c("rmse_linear","rmse_expo","rmse_Quad","rmse_sea_add","rmse_Add_sea_Linear"
,"rmse_Add_sea_Quad","rmse_multi_sea","rmse_multi_add_sea"),'RMSE'=c(rmse_linear,rmse_expo,rmse_Quad,rmse_sea_add,rmse_Add_sea_Linear,rmse_Add_sea_Quad,rmse_multi_sea,rmse_multi_add_sea))

colnames(table_rmse)<-c("model","RMSE")
View(table_rmse)

# Use entire data : Additive Seasonality with Linear trend has least RMSE value
Final_model <- lm(Sales~t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=PSdata)

# Getting residuals 
resid <- residuals(Final_model)
acf(resid,lag.max = 10)

# By principal of parcimony we will consider lag - 2  as we have so 
# many significant lags 
# Building Autoregressive model on residuals consider lag-2 
k <- arima(resid, order=c(2,0,0))
pred_res<- predict(arima(resid,order=c(2,0,0)),n.ahead = 12)
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



## ARIMA model or data driven approach 
## Load "PlasticSales" dataset 
Plastic_sales <- read.csv("File Path")

## Set the environment
install.packages(c("forecast","fpp","smooth","tseries"))
library(forecast)
library(fpp)
library(smooth)
library(tseries)


# Converting data into time series object
PS<- ts(Plastic_sales$Sales,frequency =12 ,start=c(49))
View(PS)
plot(PS)
# Dividing entire data into training and testing data 
train<-PS[1:48]
test<- PS[49:60]

# Converting time series object
train<-ts(train,frequency = 12)
test<-ts(test,frequency = 12)
plot(train)
acf(train)
pacf(train)

# Auto.Arima model on the data 
library(forecast)
model_PS <- auto.arima(train) 
summary(model_PS)

# Checking the residuals of model 
acf(model_PS$residuals)
## Forecasting the passenger traveled for next 12month 
windows()
plot(forecast(model_PS,h=12),xaxt="n") # xaxt- x axis text







