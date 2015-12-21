# Refer: http://connor-johnson.com/2014/08/29/decision-trees-in-r-using-the-c50-package/
# Load all row data
alldataC50 <- read.csv("WA_Fn-UseC_-HR-Employee-Attrition-Updated-Cleaned.csv", header = TRUE)

# Process data and data types
str(alldataC50)
## Convert some ints to factors
alldataC50$Education <- as.factor(alldataC50$Education)
alldataC50$EnvironmentSatisfaction <- as.factor(alldataC50$EnvironmentSatisfaction)
alldataC50$JobInvolvement <- as.factor(alldataC50$JobInvolvement)
alldataC50$JobSatisfaction <- as.factor(alldataC50$JobSatisfaction)
alldataC50$PerformanceRating <- as.factor(alldataC50$PerformanceRating)
alldataC50$RelationshipSatisfaction <- as.factor(alldataC50$RelationshipSatisfaction)
alldataC50$WorkLifeBalance <- as.factor(alldataC50$WorkLifeBalance)
alldataC50$JobLevel <- as.factor(alldataC50$JobLevel)
alldataC50$StockOptionLevel <- as.factor(alldataC50$StockOptionLevel)

## Create Independent and Dependent data
alldataC50 <- alldataC50[ sample( nrow( alldataC50 ) ), ]
C50x <- alldataC50[,-c(2)] # independent
C50y <- alldataC50[,c(2)] # dependent

## Create train and test sets for x and y

C50xtrain <- C50x[1:1176, ]
C50xtest <- C50x[1177:1470, ]

C50ytrain <- C50y[1:1176]
C50ytest <- C50y[1177:1470]

## Install and load C50
library(C50)
c50model <- C50::C5.0( C50xtrain, C50ytrain )
summary( c50model )
## Specificity is still bad - too many false positives

## Try Boosting
c50model <- C50::C5.0( C50xtrain, C50ytrain, trials=10 )
summary( c50model )
## BOOM - much better precision AND specificity
## Refer: https://uberpython.wordpress.com/2012/01/01/precision-recall-sensitivity-and-specificity/

# Evaluate the model using the Test Set (y)
p <- predict( c50model, C50xtest, type="class" )
sum( p == C50ytest ) / length( p )
## 85.7% accurate

pprob <- predict( c50model, C50xtest, type="prob" )
pprob

C5imp(c50model, metric = "usage", pct = TRUE)

