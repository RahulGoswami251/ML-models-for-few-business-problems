

### Movie shakuntala devi sentiment analysis on its review & user comments ######

##Sentiment Analysis on movie review 
install.packages(c("rvest","XML","magrittr"))
install.packages("stringr")
library(rvest)
library(XML)
library(magrittr)
library(stringr)

# Movie Reviews 
shakuntala_reviews <- read_html("https://timesofindia.indiatimes.com/entertainment/hindi/movie-reviews/shakuntala-devi/movie-review/77269254.cms") %>%
                      html_nodes(".section1") %>% 
                      html_text()
shakuntala_reviews

length(shakuntala_reviews)
write.table(shakuntala_reviews,"shakuntaladevi.txt",row.names = F)
getwd()


## Set the environment 
# For text mining
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
x <- as.character(shakuntala_reviews)
x <- iconv(x, "UTF-8")

# Load the data as a corpus
x <- Corpus(VectorSource(x))
inspect(x)
# Convert the text to lower case
x1 <- tm_map(x, tolower)
inspect(x1)
# Remove numbers
x1 <- tm_map(x1, removeNumbers)
# Remove punctuations
x1 <- tm_map(x1, removePunctuation)
# Remove english common stopwords
x1 <- tm_map(x1, removeWords, stopwords('english'))
# Remove your own stop word
# specify your stopwords as a character vector
x1 <- tm_map(x1, removeWords, c("shakuntala", "devi","movie","will")) 
#striping white spaces
x1 <- tm_map(x1, stripWhitespace)
inspect(x1)
# Text lemmatization
x1<-lemmatize_words(x1)

# Term document matrix 
# Converting unstructured data to structured format using TDM
tdm <- TermDocumentMatrix(x1)
tdm <- as.matrix(tdm)

#Frequency of term documents 
v <- sort(rowSums(tdm),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

# Bar plot of TDM
w <- rowSums(tdm)
w_sub <- subset(w, w >= 1)
barplot(w_sub, las=3, col = rainbow(20))
# Term shakuntala devi repeats in all most all documents
x1 <- tm_map(x1, removeWords, c('shakuntala','devi','can','will',"are",'stage'))
x1 <- tm_map(x1, stripWhitespace)
tdm <- TermDocumentMatrix(x1)
tdm <- as.matrix(tdm)
w1 <- rowSums(tdm)

# Word cloud
# with all the words

wordcloud(words = names(w1), freq = w1, 
          random.order = F, colors = rainbow(20), 
          scale=c(2,.2), rot.per = 0.3)

# lOADING +VE AND -VE dictonaries
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



## Sentiment Analysis on user comment
install.packages(c("rvest","XML","magrittr"))
install.packages("stringr")
library(rvest)
library(XML)
library(magrittr)
library(stringr)

# User comments on movie  
shakuntala_ucs <- read_html("https://timesofindia.indiatimes.com/entertainment/hindi/movie-reviews/shakuntala-devi/movie-review/77269254.cms") %>%
  html_nodes(".commentTruncate") %>% 
  html_text()
shakuntala_ucs

length(shakuntala_ucs)
write.table(shakuntala_ucs,"shakuntaladevi.txt",row.names = F)
getwd()


# Install
install.packages("tm")  # for text mining
install.packages(c("SnowballC","textstem")) # for text stemming
install.packages("wordcloud") # word-cloud generator 
install.packages("RColorBrewer") # color palettes

library('tm')
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library('textstem')

# Importing apple reviews data
y <- as.character(shakuntala_ucs)
y <- iconv(y, "UTF-8")

# Load the data as a corpus
y <- Corpus(VectorSource(y))
inspect(y[1])
# Convert the text to lower case
y1 <- tm_map(y, tolower)
inspect(y[1])
# Remove numbers
y1 <- tm_map(y1, removeNumbers)
# Remove punctuations
y1 <- tm_map(y1, removePunctuation)
# Remove english common stopwords
y1 <- tm_map(y1, removeWords, stopwords('english'))
# Remove your own stop word
# specify your stopwords as a character vector
y1 <- tm_map(y1, removeWords, c("shakuntala", "devi","movie","will","maths","review")) 
#striping white spaces
y1 <- tm_map(y1, stripWhitespace)
inspect(y)
# Text lemmatization
y1<-lemmatize_words(y1)

# Term document matrix 
# converting unstructured data to structured format using TDM
tdm <- TermDocumentMatrix(y1)
tdm <- as.matrix(tdm)

#Frequency
v <- sort(rowSums(tdm),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

# Bar plot
w <- rowSums(tdm)
w_sub <- subset(w, w >= 1)
barplot(w_sub, las=3, col = rainbow(20))
# Term shakuntala devi repeats in all most all documents
y1 <- tm_map(y1, removeWords, c('shakuntala','devi','can','will',"are",'stage'))
y1 <- tm_map(y1, stripWhitespace)
tdm <- TermDocumentMatrix(y1)
tdm <- as.matrix(tdm)
w1 <- rowSums(tdm)

# Word cloud
# with all the words

wordcloud(words = names(w1), freq = w1, 
          random.order = F, colors = rainbow(20), 
          scale=c(2,.2), rot.per = 0.3)

# lOADING +VE AND -VE dictonaries
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
tdm <- TermDocumentMatrix(y1)
findAssocs(tdm, c("vidya"),corlimit = 0.3)



