


######## Recommend a best book based on the ratings by recommendation system ######

## Set the environment 
install.packages("recommenderlab", dependencies=TRUE)
install.packages("registry ")
install.packages("Matrix")
library(recommenderlab)
library(Matrix)
library(caTools)

## Load "book" ratings dataset 
book <- read.csv("File Path")
book <- book[,-1]
# Metadata about the variable
# Movie_rate_data<-Movie
str(book)
# Rating distribution
hist(book$Book.Rating)
head(book$Book.Title,10)
# Store data in data frame  
book <- as.data.frame(book)
# The datatype should be realRatingMatrix inorder to build recommendation engine
book_rate_data_matrix <- as(book, 'realRatingMatrix')
head(book_rate_data_matrix@data)

# Popular based book recommendation model   
book_recomm_model <- Recommender(book_rate_data_matrix, method="POPULAR")

# Predictions popular book for 2 user 
recommended_book <- predict(book_recomm_model, book_rate_data_matrix[413:414], n=5)
d<-as(recommended_book, "list")
d


# User Based Collaborative Filtering reccomendation model 
book_recomm_model1 <- Recommender(book_rate_data_matrix, method="UBCF")

# Predictions popular book for 2 users 
recommended_book2 <- predict(book_recomm_model, book_rate_data_matrix[413:414], n=5)
d<-as(recommended_book, "list")
d





