

###### Classify the Size_Categorie using SVM ########

## Load "forestfires" dataset 
forest_fires <- read.csv("File Path")

## Convert categorical column state of data set into numerical
forest_fires$month <- as.numeric(factor(forest_fires$month))
forest_fires$day <- as.numeric(factor(forest_fires$day))
forest_fires$size_category <- as.factor(forest_fires$size_category)


# Normalize the features 
norm_data <- scale(forest_fires[,c(1:30)])
forest_fires1 <- cbind(norm_data,forest_fires[31])

# Divide into training and test data
set.seed(86)
library(caTools)
sample <- sample.int(n = nrow(forest_fires1),
                     size = floor(.80*nrow(forest_fires1)),
                     replace = F)
forest_fire_train <- forest_fires1[sample, ]
forest_fire_test  <- forest_fires1[-sample, ]


# Begin by training a simple linear SVM
install.packages("kernlab")
library(kernlab)
burn_area_classifier <- ksvm(size_category ~ ., data = forest_fire_train,
                          kernel = "vanilladot")


## Evaluating model performance ----
#  Predictions on testing dataset
burn_area_predictions <- predict(burn_area_classifier, forest_fire_test)
head(burn_area_predictions)
# table(burn_area_predictions, forest_fire_test$size_catagory )
agreement <- burn_area_predictions == forest_fire_test$size_category
prop.table(table(agreement))
## Improving model performance ----
burn_area_classifier_rdfdot <- ksvm(size_category ~ ., 
                                    data = forest_fire_train, 
                                    kernel = "rbfdot")
burn_area_predictions_rbf <- predict(burn_area_classifier_rdfdot, forest_fire_test)
head(burn_area_predictions_rbf)
# table(burn_area_predictions_rbf, forest_fire_test$size_catagory )
agreement_rbf <- burn_area_predictions_rbf == forest_fire_test$size_category
table(agreement_rbf)
prop.table(table(agreement_rbf))

