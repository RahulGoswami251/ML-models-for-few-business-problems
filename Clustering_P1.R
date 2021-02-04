

############### Perform K-means Clustering for the crime data and identify the number of clusters formed and draw inferences ###############

##Load "crime_data" dataset
crime_data <- read.csv("File Path")

##Identigfy the number of clusters 
#Elbow method
install.packages('factoextra')
library(factoextra)
fviz_nbclust(crime_data[,-1], kmeans, method = "wss") +
  labs(subtitle = "Elbow method")

###Cluster algorithm building
km <- kmeans(crime_data[,-1],4) 
km$centers
km$cluster
clust<-data.frame("crime_data"=crime_data[,1],"cluster"=km$cluster)

##Animation
install.packages("animation")
library(animation)
km <- kmeans.ani(crime_data[,-1], 4)


## As per above algorithm we found the maximum four cluster in crime data set & 
## also we can say that  assault crime rate is more as compare to all other crimes.   
