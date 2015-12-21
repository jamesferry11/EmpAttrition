# See process doc

# Load all row data
alldata <- read.csv("WA_Fn-UseC_-HR-Employee-Attrition-Tenure-FeatureEngineering-ForCSV.csv", header = TRUE)

# Process data and data types
str(alldata)
## Convert some ints to factors
alldata$Education <- as.factor(alldata$Education)
alldata$EnvironmentSatisfaction <- as.factor(alldata$EnvironmentSatisfaction)
alldata$JobInvolvement <- as.factor(alldata$JobInvolvement)
alldata$JobSatisfaction <- as.factor(alldata$JobSatisfaction)
alldata$PerformanceRating <- as.factor(alldata$PerformanceRating)
alldata$RelationshipSatisfaction <- as.factor(alldata$RelationshipSatisfaction)
alldata$WorkLifeBalance <- as.factor(alldata$WorkLifeBalance)
#alldata$JobLevel <- as.factor(alldata$JobLevel)
#alldata$StockOptionLevel <- as.factor(alldata$StockOptionLevel)

## Check data types
str(alldata)

# Split data 80/20 train/test
## 80% of the sample size
smp_size <- floor(0.8 * nrow(alldata))

## Set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(alldata)), size = smp_size)

train <- alldata[train_ind, ]
test <- alldata[-train_ind, ]

## Check train and test str
str(train)
str(test)

# Exploratory Data Analysis

## Trying out some plots
## Installed ggplot2 and dependencies
## Load the ggplot2 library after installing
library(ggplot2)
library(scales)

# Form Hypothesis - Are there "stories" we can tell from the data?
## Hypothesis: Past "Loyalty" has an effect on tenure
ggplot(train, aes(x = YearsPerCompany, fill = TenureGroup)) +
  geom_histogram(width = 0.5) +
  xlab("YearsPerCompany") + 
  ylab("Count") +
  labs(fill = "TenureGroup")
# YES! 
ggplot(train, aes(x = YearsPerCompany)) + geom_bar(aes(fill = TenureGroup), position = 'fill') +
  scale_y_continuous(labels = percent_format())

## Hypothesis: Different Job Roles have different Tenures
ggplot(train, aes(x = JobRole, fill = TenureGroup)) +
  geom_histogram(width = 0.5) +
  xlab("JobRole") + 
  ylab("Count") +
  labs(fill = "TenureGroup")
# YES! 
ggplot(train, aes(x = JobRole)) + geom_bar(aes(fill = TenureGroup), position = 'fill') +
  scale_y_continuous(labels = percent_format())

## Hypothesis: Overtime as a proxy for hourly/non-hourly affects tenure
ggplot(train, aes(x = OverTime)) + geom_bar(aes(fill = TenureGroup), position = 'fill') +
  scale_y_continuous(labels = percent_format())

## Hypothesis: Marital Status affects tenure
ggplot(train, aes(x = MaritalStatus)) + geom_bar(aes(fill = TenureGroup), position = 'fill') +
  scale_y_continuous(labels = percent_format())

## Form potential stories - Explore further: Two variables - YearsPerCompany and JobRole
ggplot(train, aes(x = YearsPerCompany)) + geom_bar(aes(fill = TenureGroup), position = 'fill') +
  scale_y_continuous(labels = percent_format()) + 
  facet_wrap(~JobRole)
# YES! Pretty good clustering across job roles - so no surprises

## Form potential stories - Explore further: Two variables - YearsPerCompany and Department
ggplot(train, aes(x = YearsPerCompany)) + geom_bar(aes(fill = TenureGroup), position = 'fill') +
  scale_y_continuous(labels = percent_format()) + 
  facet_wrap(~Department)
# YES! Pretty good clustering across departments - so no surprises

## Form potential stories - Explore further: Two variables - YearsPerCompany and BusinessTravel
ggplot(train, aes(x = YearsPerCompany)) + geom_bar(aes(fill = TenureGroup), position = 'fill') +
  scale_y_continuous(labels = percent_format()) + 
  facet_wrap(~BusinessTravel)


# Create Model
library(randomForest)
library(rpart)
library(caret)
str(train)
## Random Forest Model
randomForestModel <- randomForest(TenureGroup~BusinessTravel + Department + DistanceFromHome + Education + EducationField + EnvironmentSatisfaction + Gender + JobInvolvement + JobRole + JobSatisfaction + MaritalStatus + NumCompaniesWorked + OverTime + PercentSalaryHike + PerformanceRating + RelationshipSatisfaction + TrainingTimesLastYear + WorkLifeBalance + YSLP + YINCR + YWCM + YearsPerCompany
,data=train,ntree=500,mtry=5, importance=TRUE) 
print(randomForestModel)
## 80% accuracy 
#Confusion matrix:
#  TG1 TG2 TG3 class.error
#TG1 165  27  18   0.2142857
#TG2  35 156  23   0.2710280
#TG3  11  14 178   0.1231527

## Check variable importance
importance(randomForestModel)
varImpPlot(randomForestModel, type=1, pch=19,col=1,cex=1.0, main="")

## Decision Tree Model
decTree <- rpart(TenureGroup~BusinessTravel + Department + DistanceFromHome + Education + EducationField + EnvironmentSatisfaction + Gender + JobInvolvement + JobRole + JobSatisfaction + MaritalStatus + NumCompaniesWorked + OverTime + PercentSalaryHike + PerformanceRating + RelationshipSatisfaction + TrainingTimesLastYear + WorkLifeBalance + YSLP + YINCR + YWCM + YearsPerCompany,data=train)

# Evaluate Models
randomForestResult <- predict(randomForestModel, test, type=c("class"))
decTreeResult <- predict(decTree, test, type=c("class"))
table(randomForestResult, test$TenureGroup)
table(decTreeResult, test$TenureGroup)

## C50 Model
# Load all row data for C50
alldataC50 <- read.csv("WA_Fn-UseC_-HR-Employee-Attrition-Tenure-FeatureEngineering-ForCSV.csv", header = TRUE)
## Create Independent and Dependent data
alldataC50 <- alldataC50[ sample( nrow( alldataC50 ) ), ]
C50x <- alldataC50[,-c(23)] # independent
C50y <- alldataC50[,c(23)] # dependent

## Create train and test sets for x and y

C50xtrain <- C50x[1:627, ]
C50xtest <- C50x[628:784, ]

C50ytrain <- C50y[1:627]
C50ytest <- C50y[628:784]

## Install and load C50
library(C50)
c50model <- C50::C5.0( C50xtrain, C50ytrain )
summary( c50model )
## 95% accurate
# Decision Tree   
#----------------  
#  Size      Errors  
#47   28( 4.5%)   << 95.5% accurate
# Confusion Matrix  
#  (a)   (b)   (c)    <-classified as
#----  ----  ----
#  196     7     6    (a): class TG1
#1   211    10    (b): class TG2
#2     2   192    (c): class TG3

## Try Boosting
c50model <- C50::C5.0( C50xtrain, C50ytrain, trials=10 )
summary( c50model )
## 83% accuracy

# Evaluate the model using the Test Set (y)
p <- predict( c50model, C50xtest, type="class" )
sum( p == C50ytest ) / length( p )
## 84.7% accurate with 10 trials

pprob <- predict( c50model, C50xtest, type="prob" )
pprob

C5imp(c50model, metric = "usage", pct = TRUE)
