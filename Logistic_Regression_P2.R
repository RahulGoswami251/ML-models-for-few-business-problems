


######### Classify whether credit card application is accepted or not ###########

## Load "creditcard" dataset or import it in global environment  
credit_card <- read.csv("File Path")


## Covert categorical variable levels   into into numeric values    
credit_card$card <- as.factor(credit_card$card)

credit_card$owner <- as.factor(credit_card$owner)

credit_card$selfemp <- as.factor(credit_card$selfemp)


## Logistic Regression Model & Summary
CC_model <- glm(card~reports+age+income+share+expenditure+owner+
                selfemp+dependents+months+majorcards+active, family="binomial",
                data=credit_card)
CC_model


# Confusion Matrix Table
# predict(modelobject,testdataset)
prob=predict(CC_model,type=c("response"),credit_card)

# table(dataframe1,dataframe2) ..to create 2X2 matrix
confusion<-table(prob>0.5,credit_card$card)
confusion

# Model Accuracy
# adding diagonal elements in the confusion matrix
Accuracy<-sum(diag(confusion))/sum(confusion)
Accuracy


## Full Model Building Process
## Logistic Regression Model & Summary
CC_model <- glm(card~reports+age+income+share+expenditure+owner+
                  selfemp+dependents+months+majorcards+active, family="binomial",
                data=credit_card)
CC_model

## Plot Visualization 
library(lattice)
library(MASS)
library(car)
plot(CC_model) 
residualPlots(CC_model)
influenceIndexPlot(CC_model)

## Find the outliers
outlierTest(CC_model)


## 1st Iteration
credit_card1<- credit_card[c(-12,-13,-22,-27,-29,-33,-37,-46,-48,-51,-559),]
credit_card1


## Logistic Regression Model & Summary
CC_model1 <- glm(card~reports+age+income+share+expenditure+owner+
                  selfemp+dependents+months+majorcards+active, family="binomial",
                data=credit_card1)
CC_model1

# Confusion Matrix Table
# predict(modelobject,testdataset)
prob=predict(CC_model1,type=c("response"),credit_card1)

# table(dataframe1,dataframe2) ..to create 2X2 matrix
confusion<-table(prob>0.5,credit_card1$card)
confusion

# Model Accuracy
# adding diagonal elements in the confusion matrix
Accuracy<-sum(diag(confusion))/sum(confusion)
Accuracy





