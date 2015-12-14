# Load all row data
alldata <- read.csv("WA_Fn-UseC_-HR-Employee-Attrition-Updated.csv", header = TRUE)

# Split data 80/20 train/test
# 80% of the sample size
smp_size <- floor(0.8 * nrow(alldata))

# Set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(alldata)), size = smp_size)

train <- alldata[train_ind, ]
test <- alldata[-train_ind, ]