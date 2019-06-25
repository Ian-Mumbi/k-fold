setwd("C:\\Users\\hp\\Documents\\Intro to data science\\Social Network Ads")

# Read in dataset.
dataset <- read.csv("Social_Network_Ads.csv")
dataset <- dataset[3:5]

summary(dataset)
str(dataset)

# Encode categorical variable.
dataset$Purchased <- factor(dataset$Purchased, levels = c(0, 1))

table(dataset$Purchased)
# Split the dataset into training_seting and testing_seting set
library(caTools)
split <- sample.split(Y = dataset$Purchased, SplitRatio = 0.75)
training_set <- subset(dataset, split == T)
testing_set <- subset(dataset, split == F)


# Feature Scaling.
head(training_set, 2)
head(training_set[-3], 2)
training_set[-3] <- scale(training_set[-3])
testing_set[-3] <- scale(testing_set[-3])

# Build the model.
require(e1071)
vars <- colnames(training_set[-3])
modelFormula <- paste("Purchased", paste(vars, collapse = " + "), sep = " ~ ")
# Build model.
classifier <- svm(
  as.formula(modelFormula),
  data = training_set,
  type = "C-classification",
  kernel = "radial"
)

# Predict.
y_pred <- predict(classifier, newdata = testing_set[-3])

# Confusion matrix.
cm <- table(truth = testing_set$Purchased, predictions = y_pred)
cm
accuracy <- (cm[1,1] + cm[2,2]) / sum(cm)

# K-Fold cross validation
library(caret)
folds <- createFolds(training_set$Purchased, k = 10)
cv <- lapply(folds, function (x) {
  training_fold <- training_set[-x, ]
  testing_fold <- training_set[x, ]
  classifier <- svm(
    as.formula(modelFormula),
    data = training_fold,
    type = "C-classification",
    kernel = "radial"
  )
  y_pred <- predict(classifier, newdata = testing_fold[-3])
  cm <- table(testing_fold[, 3], y_pred)
  accuracy <- (cm[1,1] + cm[2,2]) / sum(cm)
  return(accuracy)
})

# Mean accuracies.
accuracy <- mean(as.numeric(cv)) # 0.9131591




















































