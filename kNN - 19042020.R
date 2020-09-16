rm(list=ls())

library(mlbench)
library(class)

# Loading dataset

?PimaIndiansDiabetes

data("PimaIndiansDiabetes")
data <- PimaIndiansDiabetes
head(data)

summary(data)
str(data)

diabetes <- data$diabetes

normalize <- function(x) {
  num <- x - min(x)
  denom <- max(x) - min(x)
  return(num/denom)
}

dim(data)

mode(data)

lapply(data[,1:8], normalize)

data <- as.data.frame((lapply(data[,1:8], normalize)))
head(data)

data$diabetes <-  diabetes

head(data)

summary(data)

# Spliting dataset
train <- data[1:600,]
test <- data[601:768,]

head(train)
head(test)

########################################################################################
## Build a kNN model (with k = 10) on the training dataset in R to predict the diabetes 
## (pos or neg). So here we will consider "diabetes" as Class variable. Then test the 
## model on the testing dataset. Calculate accuracy and error rate.


cl <- train$diabetes # defining class - predictor variable

model <- knn(train[-9], test[-9], cl, k = 10)
# model # predicted value on test data set

Accuracy <- mean(model == test$diabetes)
Accuracy#  0.7678571

# Even number k can give different accuracy at each iteration.

model <- knn(train[-9], test[-9], cl, k = 11)
Accuracy <- mean(model == test$diabetes)
Accuracy

############################################################################
## Perform k-fold validation (with k = 10) on PimaIndiansDiabetes data.

# K fold with k = 10

library(caret)

control <- trainControl(method = "cv", number = 10, classProbs=TRUE,
                        summaryFunction = twoClassSummary)

# will compute the sensitivity, specificity and area under the ROC curve


fit_knn <- train(diabetes ~ ., data=PimaIndiansDiabetes, method="knn",
                 metric="ROC", trControl=control)

fit_knn

# k-Nearest Neighbors 
# 
# 768 samples
# 8 predictors
# 2 classes: 'neg', 'pos' 
# 
# No pre-processing
# Resampling: Cross-Validated (10 fold) 
# Summary of sample sizes: 691, 691, 691, 691, 691, 691, ... 
# Resampling results across tuning parameters:
#   
# k  ROC        Sens   Spec     
# 5  0.7405755  0.816  0.5146724
# 7  0.7639858  0.830  0.5484330
# 9  0.7805883  0.838  0.5330484
# 
# ROC was used to select the optimal model using the largest value.
# The final value used for the model was k = 9.
