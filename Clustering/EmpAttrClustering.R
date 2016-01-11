# Load all row data
alldata.orig <- read.csv("Clustering\\WA_Fn-UseC_-HR-Employee-Attrition-Clustering.csv", header = TRUE)
alldata <- alldata.orig[,c(1:3,5:6,9:13,15,19,21:24,26:33)]
str(alldata)

#REFERENCE: http://marketing-yogi.blogspot.com/2012/12/segmentation-tools-in-r-session-5.html
# Scale data
mydata <- scale(alldata) 

# Ward Hierarchical Clustering
d <- dist(mydata, method = "euclidean") # distance matrix
fit <- hclust(d, method="ward")
plot(fit) # display dendogram

k1 = 4 # eyeball the no. of clusters

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
k1=2

# K-Means Cluster Analysis
fit <- kmeans(mydata, k1) # k1 cluster solution

# get cluster means
aggregate(mydata.orig,by=list(fit$cluster),FUN=mean)
# append cluster assignment
mydata1 <- data.frame(mydata.orig, fit$cluster)

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
mydata1 = cbind(mydata.orig, classif) # append to dataset
mydata1[1:10,] #view top 10 rows

fit1=cbind(classif)
rownames(fit1)=rownames(mydata)
library(cluster)
clusplot(mydata, fit1, color=TRUE, shade=TRUE,labels=2, lines=0)

summary(fit)
fit$centers

# REFERENCE https://www.youtube.com/watch?v=Ilf1XR-K3ps 
# Exploratory Factor Analysis and PCA
# Used to determine numbre of factors
# PCA - determine the number of components

alldata.pca <- princomp(alldata)
summary(alldata.pca) # look at variance
plot(alldata.pca) # 3 or 4 bars stick out

alldata.fa1 <- factanal(alldata, factors=3, rotation = "varimax")
alldata.fa1

