# Load all row data
alldata.orig <- read.csv("Clustering\\WA_Fn-UseC_-HR-Employee-Attrition-Clustering.csv", header = TRUE)

# Create datasets
alldata.all <- 
  alldata.orig[,c(1,6,10,19,27,
                  11,13,22,26,3,5,
                  21,28,30,
                  9,12,15,24,29)]
alldata.profile <- alldata.orig[,c(1,6,10,19,27)]
alldata.finance <- alldata.orig[,c(11,13,22,26)]
alldata.work <- alldata.orig[,c(3,5,21,28,30)]
alldata.ratings <- alldata.orig[,c(9,12,15,24,29)]

alldata.financeRatings <- 
  alldata.orig[,c(11,13,22,26,9,12,15,24,29)]
alldata.workRatings <- 
  alldata.orig[,c(3,5,21,28,30,9,12,15,24,29)]
alldata.financeRatingswork <- 
  alldata.orig[,c(11,13,22,26,9,12,15,24,29,
                  3,5,21,28,30)]
alldata.profileRatings  <- alldata.orig[,c(1,6,10,19,27,
                  9,12,15,24,29)]
alldata.profileRatingsWork  <- alldata.orig[,c(1,6,10,19,27,
                                           9,12,15,24,29,
                                           3,5,21,28,30)]


currdata <- alldata.financeRatings

# REFERENCE https://www.youtube.com/watch?v=Ilf1XR-K3ps 
# Exploratory Factor Analysis and PCA
# Used to determine numbre of factors
# PCA - determine the number of components
# Do this before cluster analysis
currdata  <- scale(currdata)
currdata.pca <- princomp(currdata)
summary(currdata.pca) # look at variance
plot(currdata.pca) # how many bars stick out?

currdata.fa1 <- factanal(currdata, factors=12, rotation = "varimax")
currdata.fa1

#REFERENCE: http://marketing-yogi.blogspot.com/2012/12/segmentation-tools-in-r-session-5.html
# Scale data
currdata <- scale(currdata) 
mydata <- currdata
# Ward Hierarchical Clustering
d <- dist(mydata, method = "euclidean") # distance matrix
fit <- hclust(d, method="ward")
plot(fit) # display dendogram

k1=4 # eyeball the no. of clusters

# cut tree into k1 clusters
groups <- cutree(fit, k=k1)
# draw dendogram with red borders around the k1 clusters
rect.hclust(fit, k=k1, border="red")

# Determine number of clusters #
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata,centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
# Look for an "elbow" in the scree plot #

# Use optimal no. of clusters in k-means #
k1=3

# K-Means Cluster Analysis
fit <- kmeans(mydata, k1) # k1 cluster solution

# get cluster means
aggregate(mydata,by=list(fit$cluster),FUN=mean)
# append cluster assignment
mydata1 <- data.frame(mydata, fit$cluster)

# Cluster Plot against 1st 2 principal components
# vary parameters for most readable graph
library(cluster)
clusplot(mydata, fit$cluster, color=TRUE, shade=TRUE,labels=2, lines=0)

#Model Based Clustering
library(mclust)
fit <- Mclust(mydata)
fit # view solution summary

fit$BIC # lookup all the options attempted
classif = fit$classification # classifn vector
mydata1 = cbind(mydata, classif) # append to dataset
mydata1[1:10,] #view top 10 rows

fit1=cbind(classif)
rownames(fit1)=rownames(mydata)
library(cluster)
clusplot(mydata, fit1, color=TRUE, shade=TRUE,labels=2, lines=0)

summary(fit)
fit$centers
