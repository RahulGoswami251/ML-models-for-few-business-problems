

############ Extract Next of Kin: My Conversations with Chimpanzees book reviews from goodreads & sentiment analysis on it by text & Emotion Mining ############

## Text Mining 
#  Setup the environment 
install.packages(c("rvest","XML","magrittr"))
install.packages("selectr")
library(rvest)
library(XML)
library(magrittr)
library(selectr)

# Book Reviews 
NKurl <- "https://www.goodreads.com/book/show/325779.Next_of_Kin"
NK_reviews <- NULL
for (i in 1:10){
  murl <- read_html(as.character(paste(NKurl,i,sep="=")))
  rev <- murl %>% html_nodes(".readable") %>% html_text()
  NK_reviews <- c(NK_reviews,rev)
}


length(NK_reviews)
write.table(NK_reviews,"shakuntaladevi.txt",row.names = F)
getwd()

## Set the environment 
#  For text mining
install.packages("tm")  
# For text stemming
install.packages(c("SnowballC","textstem")) 
# Word-cloud generator 
install.packages("wordcloud")
# Color palettes
install.packages("RColorBrewer") 

library('tm')
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library('textstem')

# Importing apple reviews data
nk <- as.character(NK_reviews)
nk <- iconv(nk, "UTF-8")

# Load the data as a corpus
nk <- Corpus(VectorSource(nk))
inspect(nk[1])
# Convert the text to lower case
NK1 <- tm_map(nk, tolower)
inspect(NK1[1])
# Remove numbers
NK1 <- tm_map(NK1, removeNumbers)
# Remove punctuations
NK1 <- tm_map(NK1, removePunctuation)
# Remove english common stopwords
NK1 <- tm_map(NK1, removeWords, stopwords('english'))
# Remove your own stop word
# Specify your stopwords as a character vector
NK1 <- tm_map(NK1, removeWords, c("roger", "fouts","chimpanzees","movie","my","conservations","will","are","with")) 
# striping white spaces
NK1 <- tm_map(NK1, stripWhitespace)
inspect(NK1[1])
# Text lemmatization
NK1<-lemmatize_words(NK1)

# Term document matrix 
# Converting unstructured data to structured format using TDM
tdm <- TermDocumentMatrix(NK1)
tdm <- as.matrix(tdm)

# Frequency of term documents 
v <- sort(rowSums(tdm),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

# Bar plot of term document matrix 
w <- rowSums(tdm)
w_sub <- subset(w, w >= 300)
barplot(w_sub, las=3, col = rainbow(20))
# Term Next of Kin repeats in all most all documents
NK1 <- tm_map(NK1, removeWords, c('next','of','kin',"is","will","book","language","can","also"))
NK1 <- tm_map(NK1, stripWhitespace)
tdm <- TermDocumentMatrix(NK1)
tdm <- as.matrix(tdm)
w1 <- rowSums(tdm)

## Word cloud
#  With all the words
wordcloud(words = names(w1), freq = w1, 
          random.order = F, colors = rainbow(20), 
          scale=c(2,.2), rot.per = 0.3)

# lOADING +VE AND -VE dictionaries
pos.words = scan(file.choose(), what="character", comment.char=";")	# read-in positive-words.txt
neg.words = scan(file.choose(), what="character", comment.char=";") 	# read-in negative-words.txt
pos.words = c(pos.words,"wow", "kudos", "hurray") # including our own positive words to the existing list

# Positive wordcloud
pos.matches = match(names(w), c(pos.words))
pos.matches = !is.na(pos.matches)
freq_pos <- w[pos.matches]
p_names <- names(freq_pos)

wordcloud(p_names,freq_pos,scale=c(3.5,.5),colors = rainbow(20))

# Negative wordcloud
neg.matches = match(names(w), c(neg.words))
neg.matches = !is.na(neg.matches)
freq_neg <- w[neg.matches]
n_names <- names(freq_neg)
wordcloud(n_names,freq_neg,scale=c(3.5,.5),colors = brewer.pal(8,"Dark2"))
# Association between words
tdm <- TermDocumentMatrix(NK1)
findAssocs(tdm, c("animal"),corlimit = 0.3)



## Emotion Mining
#  Setup the environment 
install.packages(c("rvest","XML","magrittr"))
install.packages("selectr")
library(rvest)
library(XML)
library(magrittr)
library(selectr)

# Book Reviews 
NKurl <- "https://www.goodreads.com/book/show/325779.Next_of_Kin"
NK_reviews <- NULL
for (i in 1:10){
  murl <- read_html(as.character(paste(NKurl,i,sep="=")))
  rev <- murl %>% html_nodes(".readable") %>% html_text()
  NK_reviews <- c(NK_reviews,rev)
}


length(NK_reviews)
write.table(NK_reviews,"shakuntaladevi.txt",row.names = F)
getwd()

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

# "Next of Kin" book review data
NK_reviews <- iconv(NK_reviews, "UTF-8")
NK1 <- get_nrc_sentiment(NK_reviews)
head(NK_reviews,n=5)


# Each sentences by eight nrc sentiments 
example<-get_sentences(NK_reviews)
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
  main ="Next of kin book reviews using Transformed Values", 
  xlab = "Narrative Time", 
  ylab = "Emotional Valence", 
  col = "red"
)

# Most Negative and Positive reviews
negative<-example[which.min(sentiment_vector)]
negative

positive<-example[which.max(sentiment_vector)]
positive


