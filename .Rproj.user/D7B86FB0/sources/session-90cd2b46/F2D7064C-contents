## Tree based models - Regression


# install the libraries that are not already installed
install.packages("tree")
install.packages("partykit") 

# load the required libraries
library(ISLR2)
library(rpart) # package for tree fitting
library(tree) # package for tree fitting used by James et el
library(partykit) # visualozation of tree structures
library(rpart.plot) # visualization of tree structures
library(ggplot2)

# we are working with Boston data for this exercise
?Boston

# exploratory tasks of the dataset
# How many rows are there in the dataset?
nrow(Boston)


# How many columns are there?
ncol(Boston)

# ouput the variable names
names(Boston)

# the column medv is the outcome variable and this is the median value of 
# owner occupied homes in each Boston suburb in $1000s 

# output the summary statistics for the outcome varibale
summary(Boston$medv)

# let's obtain the boxplot for the medv column
boxplot(Boston$medv, col = "lightgreen", lwd = 0.5, horizontal = TRUE)

# scatterplot of the medv against each of the other predictor
# code copied from chatgpt -- To do things a bit quicker
# List of variable names excluding 'medv'
# Install and load ggplot2 package
install.packages("reshape2")
library(reshape2)  # for melting the dataset

# Melt the dataset to long format for ggplot2
boston_long <- melt(Boston, id.vars = "medv")

# Create a faceted scatterplot
ggplot(boston_long, aes(x = value, y = medv)) +
  geom_point(col = "darkgreen") +
  facet_wrap(~ variable, scales = "free") +
  labs(title = "Scatterplots of medv vs Other Variables",
       x = "Value of Variable",
       y = "medv") +
  theme_minimal()

# Obtain a boxplot of medv split by the variable chas
boxplot(medv ~ chas, data = Boston)

# Create the boxplot using ggplot2
ggplot(Boston, aes(x = factor(chas), y = medv)) +
  geom_boxplot() +
  labs(title = "Boxplot of medv by chas",
       x = "chas (0 = No, 1 = Yes)",
       y = "medv") +
  theme_minimal()

# what is the median pupil-teacher ratio among the suburbs in this dataset
median(Boston$ptratio)

# which suburb of Boston has the lowest medv?



# let's model the regression tree
set.seed(1)

# creating the training set
train <- sample(1:nrow(Boston), nrow(Boston)/2)
train

# fitting the model to the training data
tree.boston <- rpart(medv ~ ., data = Boston, subset = train)
# display the tree
print(tree.boston)

# plot the the tree using rpart.plot
rpart.plot(tree.boston)

# this is tree plot using the basic R code
plot(tree.boston)
text(tree.boston, pretty = 1)


# we can prune the tree
# this part is using the rpart library
# but for consistency with the book, use the basic R code in the next line
prune.boston <- prune.rpart(tree.boston, cp = 0.1)
rpart.plot(prune.boston)

# now let's check if prunning the tree improves the performance
# this is not accepting the current tree.boston
# as that might be an object of rpart, not tree which is basic R code
# we create a new tree using the basic R code
tree.boston.basic <- tree(medv ~ ., data = Boston, subset = train)
cv.boston <- cv.tree(tree.boston.basic)

# Plot it and let's see if prunning improves the performance
plot(cv.boston$size, cv.boston$dev, type = "b", lwd = 2, col = "darkgreen")

# we will continue prunning the tree using the exact code from the book
prune.boston <- prune.tree(tree.boston.basic, best = 5)

# plot the prunned tree
plot(prune.boston)
text(prune.boston, pretty = 0)

# We make the unprunned tree to make predictions
yhat <- predict(tree.boston.basic, newdata = Boston[-train, ])

# we now match them with the real values of test set for medv variable
# extracting the real values of medv from test set
boston.test <- Boston[-train, "medv"]
boston.test

# plot the yhat(predicted) and the observed (real values) of medv
plot(yhat, boston.test, col = "darkblue", lwd = 1.5)
abline(0, 1)

# compute the mean squared error
mean((yhat - boston.test) ^ 2)
# the MSE obtained is 35.29 and square root of that is around 5.941
# which is the true median of medv
# this model will give predictions that is within approximation of the median
# value for medv ----- Things to observe :)


# Now we leave the book and go to the exercise sheet 
# ================================================================>
# The following part uses the exercise sheet codes ===============>
# Let's work on pruning the tree using the exercise sheet codes
set.seed(1)
tree.boston <- rpart(medv ~ ., data = Boston, subset = train)
print(tree.boston)

# plot the tree and try to interpret
rpart.plot(tree.boston)

# looking at the above tree let's answer this question:
# What is the median house price for a flat located in a suburb with
#rm = 6 and lstat = 12?

# Answer: After checking the tree I think that will fall somewhere around
# $23000   --- After verification put your remark here: -----------------------

# let's now consider prunning the tree with cp = 0 which gives the most complex
# tree for us
tree.boston.full <- rpart(medv ~ ., data = Boston, subset = train, cp = 0)

# let's print the full tree
print(tree.boston.full)

# plot the tree in it's most complex form
rpart.plot(tree.boston.full)

# table with cp values from cross validation
cptable <- tree.boston.full$cptable
cptable
plotcp(tree.boston.full, col = "red", lwd = 1.5)

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

# Now that we have got an ideal value for CP we can again train a tree model
# and provide the cp value that give a less complex tree
prune.boston <- prune(tree.boston, cp = 0.0104877316)

# print the prunned tree
rpart.plot(prune.boston)

# compute the MSE for the full, prunned and default trees
# on training data full tree
pred.train.full <- predict(tree.boston.full, newdata = Boston[train, ])
# MSE -- for the full tree
mean((Boston$medv[train] - pred.train.full) ^ 2)

## prunned tree
pred.train.pruned <- predict(prune.boston, newdata = Boston[train, ])

# MSE -- for the prunned tree
mean((Boston$medv[train] - pred.train.pruned) ^ 2)

# default tree
pred.train.default <- predict(tree.boston, newdata = Boston[train, ])

# MSE -- for the default tree
mean((Boston$medv[train] - pred.train.default) ^ 2)

#### Finding MSEs for the test data
# full tree
pred.test.full <- predict(tree.boston.full, newdata = Boston[-train, ])

# MSE -- For the full tree
mean((Boston$medv[-train] - pred.test.full) ^ 2)

# Prunned tree
pred.test.prunned <- predict(prune.boston, newdata = Boston[-train, ])

# MSE --  for the prunned tree
mean((Boston$medv[-train] - pred.test.prunned) ^ 2)

# defult tree
pred.test.default <- predict(tree.boston, newdata = Boston[-train, ])

# MSE -- for the default tree
mean((Boston$medv[-train] - pred.test.default) ^ 2)


# The prunned tree gives MSE of 35.2(after rounding) and square root of that 
# will be around 5.93 
# This indicates that the model will give test predictions that are within around
# $5930 of the true median home value (medv) for each suburb.
# let's once again check the median of medv
median(Boston$medv)

# plot the medv values against the pruned tree predictions
boston.test <- Boston[-train, "medv"]

plot(pred.test.prunned, boston.test, col = "darkorange")
abline(c(0,1), col = "red", lty = 2)




# ===== I will not remove the following texts from the previous session :)
# The full tree is quiet complex, the pruning rule suggested by the authors
# of the rpart library says to choose the smallest number of nodes (largest cp value)
# which lies within 1 standard deviation of the cross-validation smallest deviance
# that is below the the dotted line -- So compare the xerror with the xerror+xstd
# for the last row


