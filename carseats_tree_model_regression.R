### working on car seat data set


# load the libraries
library(ISLR2)

data("Carseats")

# exploratory steps for the dataset
?Carseats

head(Carseats)

names(Carseats)
summary(Carseats)

# split the dataset
train <- sample(1: nrow(Carseats), nrow(Carseats) / 2)

# all the indices of the training dataset
train

# dimension of the dataset
dim(Carseats)

# obtain the rpart default regression tree for the outcome variable sales
car.train.default <- rpart(Sales ~ .,data = Carseats, subset = train)
rpart.plot(car.train.default)

# let's also obtain a full tree and then prune it
car.train.full <- rpart(Sales ~ ., data = Carseats, subset = train)


# find an acceptable pruned tree and plot it -- find a good value for cp
cptable <- car.train.full$cptable
cptable

plotcp(car.train.full, col = "red", lwd = 1.5)

# let's first find the minimum deviance
minDeviance <- which.min(cptable[, "xerror"])
minDeviance

# finding the dotted value --- minimum xerror + standard deviation xstd
dotted <- cptable[minDeviance, "xerror"] + cptable[minDeviance, 5]
dotted

# draw a line 
abline(h = dotted, col = "red", lty = 2)

# which is the first row less than this value?
cpPrunning <- cptable[cptable[, "xerror"] < dotted, ][1, ]

# looking at the output of new cp value, it suggests that split of 5 or 6 would 
# be a good choice
cpPrunning

prune.carseats <- prune(car.train.default, cp = 0.03217247)

# print the prunned tree
rpart.plot(prune.carseats)

# obtain some predictions


























