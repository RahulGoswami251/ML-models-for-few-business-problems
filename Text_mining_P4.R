


############  Extract Joker movie reviews from IMDB & sentiment analysis on it by Emotion Mining ############

install.packages(c("rvest","XML","magrittr"))
library(rvest)
library(XML)
library(magrittr)

## Movie Reviews 
jurl <- "https://www.imdb.com/title/tt7286456/reviews?ref_=tt_urv"
Joker_reviews <- NULL
for (i in 1:15){
  murl <- read_html(as.character(paste(jurl,i,sep="=")))
  rev <- murl %>% html_nodes(".show-more__control") %>% html_text()
  Joker_reviews <- c(Joker_reviews,rev)
}

## Setup environment for emotion mining
install.packages("syuzhet")
install.packages("tibble")
library("syuzhet")
library(lubridate,ggplot2)
library(ggplot2)
library(scales)
library(dplyr)
library(reshape2)
library(tibble)

# Joker movie review data
Joker_reviews <- iconv(Joker_reviews, "UTF-8")
x <- get_nrc_sentiment(Joker_reviews)
head(Joker_reviews,n=5)


# Each sentences by eight nrc sentiments 
example<-get_sentences(Joker_reviews)
nrc_data<-get_nrc_sentiment(example)
# Bar plot for emotion mining
windows()
barplot(colSums(nrc_data), las = 1, col = rainbow(10), ylab = 'Count', main = 'Emotion scores')


## Sentiment analysis on all three laxicons 
sentiment_vector<-get_sentiment(example,method="bing")
sentiment_afinn<-get_sentiment(example,method="afinn")
sentiment_nrc<-get_sentiment(example,method="nrc")

sum(sentiment_afinn)
mean(sentiment_afinn)
summary(sentiment_afinn)

windows()
plot(sentiment_vector,type='l',main='Plot trajectory',xlab='Narative time',ylab='Emotional valence')
abline(h=0,color='red')

plot(
  sentiment_vector, 
  type="h", 
  main="Example Plot Trajectory", 
  xlab = "Narrative Time", 
  ylab= "Emotional Valence"
)

## Shape smoothing and normalization using a Fourier based transformation and 
## low pass filtering is achieved using the get_transformed_values function as 
## shown below.

ft_values <- get_transformed_values(
  sentiment_vector, 
  low_pass_size = 3, 
  x_reverse_len = 100,
  padding_factor = 2,
  scale_vals = TRUE,
  scale_range = FALSE
)

plot(
  ft_values, 
  type ="l", 
  main ="Joker movie reviews using Transformed Values", 
  xlab = "Narrative Time", 
  ylab = "Emotional Valence", 
  col = "red"
)

# Most Negative and Positive reviews
negative<-example[which.min(sentiment_vector)]
negative

positive<-example[which.max(sentiment_vector)]
positive


