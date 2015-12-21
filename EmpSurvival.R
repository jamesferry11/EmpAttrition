# Load Survival package
library(survival)

# Load data
alldata <- read.csv("Survival\\WA_Fn-UseC_-HR-Employee-Attrition-Updated-Survival.csv", header = TRUE)
attach(alldata)

# Define variables
time <- YearsAtCompany
event <- Attrition
x <- cbind(OverTime,Age,MonthlyIncome,JobRole,BusinessTravel,DistanceFromHome,JobInvolvement)
group <- OverTime

# Summary Statistics
summary(time)
summary(event)
summary(x)
summary(group)

# Kaplan-Meier non-parametric analysis
kmsurvival <- survfit(Surv(time,event) ~1)
summary(kmsurvival)
plot(kmsurvival, xlab="Time",ylab="Survival Probability")

# Kaplan-Meier non-parametric analysis by group
kmsurvival <- survfit(Surv(time,event) ~group)
summary(kmsurvival)
plot(kmsurvival, xlab="Time",ylab="Survival Probability")