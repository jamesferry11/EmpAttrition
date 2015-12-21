# Load all row data
alldata <- read.csv("WA_Fn-UseC_-HR-Employee-Attrition-Updated.csv", header = TRUE)

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
alldata$JobLevel <- as.factor(alldata$JobLevel)
alldata$StockOptionLevel <- as.factor(alldata$StockOptionLevel)

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
## Interesting features: JobSatisfaction,EnvironmentSatisfaction,
## Interesting features: JobInvolvement,PerformanceRating,
## Interesting features: WorkLifeBalance,DistanceFromHome
table(alldata$JobSatisfaction) 
table(alldata$EnvironmentSatisfaction)
table(alldata$JobInvolvement)
table(alldata$PerformanceRating)
table(alldata$WorkLifeBalance)
table(alldata$OverTime) 
### It looks like there is some relationship between JobSatisfaction and EnvironmentSatisfaction
### And JobInvolvement and WorkLifeBalance

## Trying out some plots
## Installed ggplot2 and dependencies
## Load the ggplot2 library after installing
library(ggplot2)
library(scales)

# Form Hypothesis - Are there "stories" we can tell from the data?
## Hypothesis: Low JobSatisfaction leads to attrition
ggplot(train, aes(x = JobSatisfaction, fill = Attrition)) +
  geom_histogram(width = 0.5) +
  xlab("Job Satisfaction") + 
  ylab("Count") +
  labs(fill = "Attrition")

## Hypothesis: Low EnvironmentSatisfaction leads to attrition
ggplot(train, aes(x = EnvironmentSatisfaction, fill = Attrition)) +
  geom_histogram(width = 0.5) +
  xlab("Environment Satisfaction") + 
  ylab("Count") +
  labs(fill = "Attrition")

## Hypothesis: Low WorkLifeBalance leads to attrition
ggplot(train, aes(x = WorkLifeBalance, fill = Attrition)) +
  geom_histogram(width = 0.5) +
  xlab("Work Life Balance") + 
  ylab("Count") +
  labs(fill = "Attrition")

## Explore: Travel and MaritalStatus- no discernable pattern
ggplot(train, aes(x = MaritalStatus, fill = Attrition)) +
  geom_bar(binwidth = 0.5) +
  facet_wrap(~BusinessTravel) + 
  ggtitle("Business Travel") +
  xlab("MaritalStatus") +
  ylab("Count") +
  labs(fill = "Attrition")

## Hypothesis: Some JobRole have high attrition rates
ggplot(train, aes(x = JobRole, fill = Attrition)) +
  geom_histogram(width = 0.5) +
  xlab("Job Role") + 
  ylab("Count") +
  labs(fill = "Attrition")
# YES! 
ggplot(train, aes(x = JobRole)) + geom_bar(aes(fill = Attrition), position = 'fill') +
  scale_y_continuous(labels = percent_format())

## Hypothesis: Some JobLevel have high attrition rates
ggplot(train, aes(x = JobLevel, fill = Attrition)) +
  geom_histogram(width = 0.5) +
  xlab("Job Level") + 
  ylab("Count") +
  labs(fill = "Attrition")
# YES! 
ggplot(train, aes(x = JobLevel)) + geom_bar(aes(fill = Attrition), position = 'fill') +
  scale_y_continuous(labels = percent_format())

## Explore: Hourly/Exempt - Overtime (as a proxy) affects attrition
ggplot(train, aes(x = OverTime, fill = Attrition)) +
  geom_histogram(width = 0.5) +
  xlab("Overtime") + 
  ylab("Count") +
  labs(fill = "Attrition")
# YES! 
ggplot(train, aes(x = OverTime)) + geom_bar(aes(fill = Attrition), position = 'fill') +
  scale_y_continuous(labels = percent_format())

## Explore actionable features: JobInvolvement 
ggplot(train, aes(x = JobInvolvement, fill = Attrition)) +
  geom_histogram(width = 0.5) +
  xlab("Job Involvement") + 
  ylab("Count") +
  labs(fill = "Attrition")
# YES! 
ggplot(train, aes(x = JobInvolvement)) + geom_bar(aes(fill = Attrition), position = 'fill') +
  scale_y_continuous(labels = percent_format())

## Explore actionable features: BusinessTravel 
ggplot(train, aes(x = BusinessTravel, fill = Attrition)) +
  geom_histogram(width = 0.5) +
  xlab("Business Travel") + 
  ylab("Count") +
  labs(fill = "Attrition")
# YES! 
ggplot(train, aes(x = BusinessTravel)) + geom_bar(aes(fill = Attrition), position = 'fill') +
  scale_y_continuous(labels = percent_format())

## Form potential stories - Explore further: Two variables - JobRole and BusinessTravel
ggplot(train, aes(x = BusinessTravel)) + geom_bar(aes(fill = Attrition), position = 'fill') +
  scale_y_continuous(labels = percent_format()) + 
  facet_wrap(~JobRole)
# YES! Story: There is high attrition among Sales Reps who frequently travel - Actionable?

## Form potential stories - Explore further: Two variables - JobRole and Overtime as a proxy for Hourly/Exempt
ggplot(train, aes(x = OverTime)) + geom_bar(aes(fill = Attrition), position = 'fill') +
  scale_y_continuous(labels = percent_format()) + 
  facet_wrap(~JobRole)
# YES! Story: There is high attrition among Houlry Sales Reps and Hourly Lab Technicians - Actionable?

## Form potential stories - Explore further: Two variables - JobRole and MonthlyIncome 
ggplot(train, aes(x = MonthlyIncome)) + geom_bar(aes(fill = Attrition), position = 'fill') +
  scale_y_continuous(labels = percent_format()) + 
  facet_wrap(~JobRole)
# YES! Story: There is high attrition among Sales Reps but it goes down as monthly income goes up - Actionable?

## Form potential stories - Explore further: Two variables - JobRole and WorkLifeBalance 
ggplot(train, aes(x = WorkLifeBalance)) + geom_bar(aes(fill = Attrition), position = 'fill') +
  scale_y_continuous(labels = percent_format()) + 
  facet_wrap(~JobRole)
# YES! Story: It seems work life balance is important to Lab Techs but not necessarily to Sales Reps - Actionable?

## Form potential stories - Explore further: Two variables - JobRole and JobInvolvement 
ggplot(train, aes(x = JobInvolvement)) + geom_bar(aes(fill = Attrition), position = 'fill') +
  scale_y_continuous(labels = percent_format()) + 
  facet_wrap(~JobRole)
# YES! Story: It seems work life balance is important to most people - but not necessarily to Sales Reps - Actionable?

## Form potential stories - Explore further: Two variables - OverTime and MonthlyIncome 
ggplot(train, aes(x = MonthlyIncome)) + geom_bar(aes(fill = Attrition), position = 'fill') +
  scale_y_continuous(labels = percent_format()) + 
  facet_wrap(~OverTime)
# YES! Story: It seems a high monthly income may lessen attrition


# Create Model
library(randomForest)
library(rpart)
library(caret)

## Random Forest Model
randomForestModel <- randomForest(Attrition~Age + BusinessTravel + DailyRate + Department + DistanceFromHome + Education + EducationField + EnvironmentSatisfaction + Gender + HourlyRate + JobInvolvement + JobLevel + JobRole + JobSatisfaction + MaritalStatus + MonthlyIncome + MonthlyRate + NumCompaniesWorked + Over18 + OverTime + PercentSalaryHike + PerformanceRating + RelationshipSatisfaction + StandardHours + StockOptionLevel + TotalWorkingYears + TrainingTimesLastYear + WorkLifeBalance + YearsAtCompany + YearsInCurrentRole + YearsSinceLastPromotion + YearsWithCurrManager,data=train,ntree=500,mtry=5, importance=TRUE) 
print(randomForestModel)
## False positive 84% of the time still - Not precise at all - need a better model or more data

## Check variable importance
importance(randomForestModel)
varImpPlot(randomForestModel, type=1, pch=19,col=1,cex=1.0, main="")

## Decision Tree Model
decTree <- rpart(Attrition~Age + BusinessTravel + DailyRate + Department + DistanceFromHome + Education + EducationField + EnvironmentSatisfaction + Gender + HourlyRate + JobInvolvement + JobLevel + JobRole + JobSatisfaction + MaritalStatus + MonthlyIncome + MonthlyRate + NumCompaniesWorked + Over18 + OverTime + PercentSalaryHike + PerformanceRating + RelationshipSatisfaction + StandardHours + StockOptionLevel + TotalWorkingYears + TrainingTimesLastYear + WorkLifeBalance + YearsAtCompany + YearsInCurrentRole + YearsSinceLastPromotion + YearsWithCurrManager,data=train)

# Evaluate Models
randomForestResult <- predict(randomForestModel, test, type=c("class"))
decTreeResult <- predict(decTree, test, type=c("class"))
###test$PAtt <-- predict(randomForestModel, test, type="response")
###prediction <- function(t) ifelse(randomForestResult > t, 1,0)
randomForestModel
decTreeResult
table(decTreeResult, test$Attrition)

## Random Forest Model 2 - Using highly "predictive" variables
randomForestModel <- randomForest(Attrition~Age + BusinessTravel + JobInvolvement + JobRole + MonthlyIncome + OverTime + WorkLifeBalance,data=train,ntree=500,mtry=5, importance=TRUE)
print(randomForestModel)
importance(randomForestModel)
varImpPlot(randomForestModel, type=1, pch=19,col=1,cex=1.0, main="")
## 85% accurate but many false positives based on the confusion matrix
## Too many false positives - so we have okay accuracy and great specificity (No) but horrible precision (Yes)
## We need to get more precise - one way to do that is to use a more precise dataset
## False positive 72% of the time still - Not precise at all - need a better model or more data



