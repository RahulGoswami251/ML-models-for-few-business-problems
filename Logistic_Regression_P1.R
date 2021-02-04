

######### The client has subscribed a term deposit? (binary: "yes","no") ###########

## Load "bank-full" dataset 
bank.full <- read.csv("File Path")


## Logistic Regression Model & Summary
bank_term_dp_model <- glm(y~age+job+marital+education+default
                          +balance+housing+loan+contact+day+month+duration+campaign+
                           pdays+previous+poutcome,family="binomial", data = bank.full)

bank_term_dp_model


# Confusion Matrix Table
# predict(modelobject,testdataset)
prob=predict(bank_term_dp_model,type=c("response"),bank.full)

# table(dataframe1,dataframe2) ..to create 2X2 matrix
confusion<-table(prob>0.5,bank.full$y)
confusion

# Model Accuracy
# adding diagonal elements in the confusion matrix
Accuracy<-sum(diag(confusion))/sum(confusion)
Accuracy


## Full Model Building Process
## Logistic Regression Model & Summary
bank_term_dp_model <- glm(y~age+job+marital+education+default
                          +balance+housing+loan+contact+day+month+duration+campaign+
                            pdays+previous+poutcome,family="binomial", data = bank.full)
bank_term_dp_model

## Plot Visualizationb 
library(lattice)
library(MASS)
library(car)
plot(bank_term_dp_model) 
residualPlots(bank_term_dp_model)
influenceIndexPlot(bank_term_dp_model)

## Find the outliers
outlierTest(bank_term_dp_model)


## 1st Iteration
bank.full1<- bank.full[c(-24149,-24096),]
bank.full1


## Logistic Regression Model & Summary
bank_term_dp_model1 <- glm(y~age+job+marital+education+default
                          +balance+housing+loan+contact+day+month+duration+campaign+
                            pdays+previous+poutcome,family="binomial", data = bank.full1)
bank_term_dp_model1

# Confusion Matrix Table
# predict(modelobject,testdataset)
prob=predict(bank_term_dp_model1,type=c("response"),bank.full1)

# table(dataframe1,dataframe2) ..to create 2X2 matrix
confusion1<-table(prob>0.5,bank.full1$y)
confusion1

# Model Accuracy
# adding diagonal elements in the confusion matrix
Accuracy<-sum(diag(confusion1))/sum(confusion1)
Accuracy

