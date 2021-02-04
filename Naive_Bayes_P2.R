

##### Classification using Naive Bayes for SMS_raw data ######

# Load "sms_raw_NB" data set or Import it in global environment 
sms_raw <- read.csv("File Path", stringsAsFactors = FALSE)
# Examine the structure of the sms data
str(sms_raw)
# Convert spam/ham to factor
sms_raw$type <- factor(sms_raw$type)
# Build a corpus using the text mining (tm) package
library(tm)
sms_corpus <- Corpus(VectorSource(sms_raw$text))
# Clean up the corpus using tm_map()
corpus_clean <- tm_map(sms_corpus, tolower)
corpus_clean <- tm_map(corpus_clean, removeNumbers)
corpus_clean <- tm_map(corpus_clean, removeWords, stopwords())
corpus_clean <- tm_map(corpus_clean, removePunctuation)
corpus_clean <- tm_map(corpus_clean, stripWhitespace)
inspect(corpus_clean)
# Create a document-term sparse matrix
sms_dtm <- DocumentTermMatrix(corpus_clean)
# Creating training and test datasets
sms_raw_train <- sms_raw[1:4169, ]
sms_raw_test  <- sms_raw[4170:5559, ]

sms_dtm_train <- sms_dtm[1:4169, ]
sms_dtm_test  <- sms_dtm[4170:5559, ]

sms_corpus_train <- corpus_clean[1:4169]
sms_corpus_test  <- corpus_clean[4170:5559]

# Check that the proportion of spam is similar
prop.table(table(sms_raw_train$type))
prop.table(table(sms_raw_test$type))


# Word cloud visualization
library(wordcloud)
library(quanteda)
wordcloud(sms_corpus_train, min.freq = 30)
wordcloud(sms_corpus_train, min_count = 30,
                   color = c('red', 'pink', 'green', 'purple', 'orange', 'blue'))


# Subset the training data into spam and ham groups
spam <- subset(sms_raw_train, type == "spam")
ham  <- subset(sms_raw_train, type == "ham")

wordcloud(spam$text, max.words = 100, scale = c(3, .2),colors = 'blue')
wordcloud(ham$text, max.words = 100, scale = c(3, 0.2))


# Indicator features for frequent words
sms_dict<-findFreqTerms(sms_dtm_train, 3)
# sms_dict <- Dictionary(findFreqTerms(sms_dtm_train, 3))
sms_train <- DocumentTermMatrix(sms_corpus_train, list(dictionary = sms_dict))
sms_test  <- DocumentTermMatrix(sms_corpus_test, list(dictionary = sms_dict))
# Convert counts to a factor
convert_counts <- function(x) {
  x <- ifelse(x > 0, 1, 0)
  x <- factor(x, levels = c(0, 1), labels = c("No", "Yes"))
}

# apply() convert_counts() to columns of train/test data
sms_train <- apply(sms_train, MARGIN = 2, convert_counts)
sms_test  <- apply(sms_test, MARGIN = 2, convert_counts)

## Training a model on the data
install.packages("e1071")
library(e1071)
sms_classifier <- naiveBayes(sms_train, sms_raw_train$type)

## Evaluating model performance 
sms_test_pred <- predict(sms_classifier, sms_test)

table(sms_test_pred, sms_raw_test$type)

# Model performance 
sms_classifier2 <- naiveBayes(sms_train, sms_raw_train$type, laplace = 1)
sms_test_pred2 <- predict(sms_classifier2, sms_test)
table(sms_test_pred2, sms_raw_test$type)

