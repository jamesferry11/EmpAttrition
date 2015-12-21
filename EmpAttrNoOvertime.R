# Load all row data
alldataNoOvertime <- read.csv("WA_Fn-UseC_-HR-Employee-Attrition-NoOvertime.csv", header = TRUE)

# Process data and data types
str(alldataNoOvertime)
## Convert some ints to factors
alldataNoOvertime$Education <- as.factor(alldataNoOvertime$Education)
alldataNoOvertime$EnvironmentSatisfaction <- as.factor(alldataNoOvertime$EnvironmentSatisfaction)
alldataNoOvertime$JobInvolvement <- as.factor(alldataNoOvertime$JobInvolvement)
alldataNoOvertime$JobSatisfaction <- as.factor(alldataNoOvertime$JobSatisfaction)
alldataNoOvertime$PerformanceRating <- as.factor(alldataNoOvertime$PerformanceRating)
alldataNoOvertime$RelationshipSatisfaction <- as.factor(alldataNoOvertime$RelationshipSatisfaction)
alldataNoOvertime$WorkLifeBalance <- as.factor(alldataNoOvertime$WorkLifeBalance)
alldataNoOvertime$JobLevel <- as.factor(alldataNoOvertime$JobLevel)
alldataNoOvertime$StockOptionLevel <- as.factor(alldataNoOvertime$StockOptionLevel)

# Split data 80/20 train/test
## 80% of the sample size
smp_size <- floor(0.8 * nrow(alldataNoOvertime))

## Set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(alldataNoOvertime)), size = smp_size)

trainNoOvertime <- alldata[train_ind, ]
testNoOvertime <- alldata[-train_ind, ]


## Random Forest Model 1 - Using highly "predictive" variables
randomForestModel <- randomForest(Attrition~Age + BusinessTravel + JobInvolvement + JobRole + MonthlyIncome + WorkLifeBalance,data=trainNoOvertime,ntree=500,mtry=5, importance=TRUE)
print(randomForestModel)
## False positive 81% of the time still - Not precise at all - need a better model or more data

