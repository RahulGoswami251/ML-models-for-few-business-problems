

######## Perform clustering K means clustering for the airlines data to obtain optimum number of clusters ##########

### Load "EastWestAirlines" dataset 
EastWestAirlines <- read_excel("File Path")


# Elbow method
install.packages('factoextra')
library(factoextra)
fviz_nbclust(EastWestAirlines[,-1], kmeans, method = "wss") +
  labs(subtitle = "Elbow method")

# Cluster algorithm building
E_W_A <- kmeans(EastWestAirlines[,-1],5) 
E_W_A$centers
E_W_A$cluster
clust<-data.frame("EastWestAirlines"=EastWestAirlines[,1],"cluster"=E_W_A$cluster)

# Animation
install.packages("animation")
library(animation)
E_W_A <- kmeans.ani(EastWestAirlines[,-1], 5)


## As per above algorithm we got 5 number of cluster for this dataset or we can say that we 
## divided 3999 passenger into 5 clusters as per their similar chartristics with their Id's. 



###########Perform clustering hierarchical clustering for the airlines data to obtain optimum number of clusters##########

### Load "EastWestAirlines" dataset
library(readxl)
EastWestAirlines <- read_excel("File path")
##                  or import dataset excel file in global environment 

# Checking the correlation
cor(EastWestAirlines)
# Remove ID column 
EastWestAirlines <- EastWestAirlines[,-1]

## Summary of all continuous features 
summary(EastWestAirlines$Balance)
summary(EastWestAirlines$Qual_miles)
summary(EastWestAirlines$cc1_miles)
summary(EastWestAirlines$cc2_miles)
summary(EastWestAirlines$cc3_miles)
summary(EastWestAirlines$Bonus_miles)
summary(EastWestAirlines$Flight_miles_12mo)
summary(EastWestAirlines$Flight_trans_12)
summary(EastWestAirlines$Days_since_enroll)
summary(EastWestAirlines$`Award?`)



# Set the environment for automated EDA
install.packages("DataExplorer")
library(DataExplorer)
install.packages("tidyverse")
library(tidyverse)


## Visualiation of all features & report generation of them 
plot_intro(EastWestAirlines)
plot_missing(EastWestAirlines)

# Histogram
DataExplorer::plot_histogram(EastWestAirlines)
# Histogram for transformed variable featuers 
DataExplorer::plot_histogram(log(EastWestAirlines))

# Density plot
plot_density(EastWestAirlines)

# Correlation between features 
plot_correlation(EastWestAirlines, cor_args = list( 'use' = 'complete.obs'))

# Box plots by indivisual feature based against all features 
plot_boxplot(EastWestAirlines, by= 'Balance',  ncol = 2)
plot_boxplot(EastWestAirlines, by= 'Qual_miles',  ncol = 2)
plot_boxplot(EastWestAirlines, by= 'cc1_miles',  ncol = 2)
plot_boxplot(EastWestAirlines, by= 'cc2_miles',  ncol = 2)
plot_boxplot(EastWestAirlines, by= 'cc3_miles',  ncol = 2)
plot_boxplot(EastWestAirlines, by= 'Bonus_miles',  ncol = 2)
plot_boxplot(EastWestAirlines, by= 'Bonus_trans',  ncol = 2)
plot_boxplot(EastWestAirlines, by= 'Flight_miles_12mo',  ncol = 2)
plot_boxplot(EastWestAirlines, by= 'Flight_trans_12',  ncol = 2)
plot_boxplot(EastWestAirlines, by= 'Days_since_enroll',  ncol = 2)
plot_boxplot(EastWestAirlines, by= 'Award?',  ncol = 2)



# Box plots by transofmed indivisual feature based against all features 
plot_boxplot(log(EastWestAirlines), by= 'Balance',  ncol = 2)
plot_boxplot(log(EastWestAirlines), by= 'Qual_miles',  ncol = 2)
plot_boxplot(log(EastWestAirlines), by= 'cc1_miles',  ncol = 2)
plot_boxplot(log(EastWestAirlines), by= 'cc2_miles',  ncol = 2)
plot_boxplot(log(EastWestAirlines), by= 'cc3_miles',  ncol = 2)
plot_boxplot(log(EastWestAirlines), by= 'Bonus_miles',  ncol = 2)
plot_boxplot(log(EastWestAirlines), by= 'Bonus_trans',  ncol = 2)
plot_boxplot(log(EastWestAirlines), by= 'Flight_miles_12mo',  ncol = 2)
plot_boxplot(log(EastWestAirlines), by= 'Flight_trans_12',  ncol = 2)
plot_boxplot(log(EastWestAirlines), by= 'Days_since_enroll',  ncol = 2)


# Scatter plots by indivisual feature based against all features 
plot_scatterplot(EastWestAirlines, by= 'Balance')
plot_scatterplot(EastWestAirlines, by= 'Qual_miles')
plot_scatterplot(EastWestAirlines, by= 'cc1_miles')
plot_scatterplot(EastWestAirlines, by= 'cc2_miles')
plot_scatterplot(EastWestAirlines, by= 'cc3_miles')
plot_scatterplot(EastWestAirlines, by= 'Bonus_miles')
plot_scatterplot(EastWestAirlines, by= 'Bonus_trans')
plot_scatterplot(EastWestAirlines, by= 'Flight_miles_12mo')
plot_scatterplot(EastWestAirlines, by= 'Flight_trans_12')
plot_scatterplot(EastWestAirlines, by= 'Days_since_enroll')


# Visualization HTML report generation in webbrowser of all features 
create_report(EastWestAirlines)

## Check duplicacy in dataset 
sum(duplicated(EastWestAirlines))

# Remove duplicates & create new dataset 
EastWestAirlines1 <- distinct(EastWestAirlines)
##data standardization
EWA <- scale(EastWestAirlines1[,1:11])

# Computing the distance natrix
EWA_dm <- dist(EWA, method = "manhattan") 

# Building the algorithm with 'centroid method'
EWA_fit <- hclust(EWA_dm, method="centroid") 

#Display dendogram
plot(EWA_fit)

# Cut tree into 5 clusters
EWA_clusters <- cutree(EWA_fit, k=5) 

# Draw dendogram with red borders around the 5 clusters 
rect.hclust(EWA_fit, k=5, border="red")

# Attach the cluster numbers to passenger ID
Final_output=data.frame('EWA'=EastWestAirlines1[,1],'Cluster' =EWA_clusters)

View(Final_output)















