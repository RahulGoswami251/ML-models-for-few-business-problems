
#########Perform Principal component analysis and perform clustering using first 3 principal ########  
## component scores (both hierarchical and k mean clustering(scree plot or elbow curve) and obtain
## optimum number of clusters and check whether we have obtained same number of clusters with the 
## original data(class column we have ignored at the beginning who shows it has 3 clusters)df
  
## Load "wine" dataset  
wine <- read.csv("File path")

## Standardization of data 
wine_std <- scale(wine[,c(2:14)])

## Finding direction of variance through correlation or covariance matrix function 
wine_pca<-princomp(wine_std, cor = TRUE,scores = TRUE, covmat = NULL)
summary(wine_pca)

## Findout 1st 3 scores of data  
wine_pca$scores
wine_nd<-wine_pca$scores[,1:3]

## Visualization of 3 scores of data 
library(lattice)
plot(wine_pca$scores[,1:3],col="Blue",cex = 0.2)
text(wine_pca$scores[,1:3], labels=c(1:178), cex= .7)

###### Note: Kindly go for both clustering one by one with 3-PCA scores
#            means that PCA + Hierarchical clustering & PCA + K means clustering
#            due to 1st 3 PCA scores data "wine_nd" is using in both clustering. 

####### Hirerarical Clustering of 1st 3 PCA Scores ###########

# Computing the distance matrix    
wine_nd <- dist(wine_std, method = "manhattan") 

# Building the algorithm with 'average method'
wine_fit <- hclust(wine_nd, method="centroid") 

# Display dendogram
plot(wine_fit)

# Cut tree into 3 clusters
wine_clusters <- cutree(wine_fit, k=8) 

# Draw dendogram with red borders around the 8 clusters 
rect.hclust(wine_fit, k=8, border="red")

# Attach the cluster numbers to passenger ID
Final_output=data.frame('WINE'=wine[,1],'Cluster' =wine_clusters)

View(Final_output)


####### K-means clustering of 1st 3 PCA Scores ###########

# Elbow method
install.packages('factoextra')
library(factoextra)
fviz_nbclust(wine_nd, kmeans, method = "wss") +labs(subtitle = "Elbow method")

### Cluster algorithm building
wine_c <- kmeans(wine_nd,5) 
wine_c$centers
wine_c$cluster
clust<-data.frame("Wine"=wine_nd,"cluster"=wine_c$cluster)

## Animation
install.packages("animation")
library(animation)
wine_c <- kmeans.ani(wine_nd, 5)



