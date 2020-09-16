rm(list = ls())

titanic_data <- read.csv('titanic_data.csv')
# View(titanic_data)

# Survived   - The passenger survived or not; 0 - did not, 1 - Survived

# Pclass_1   - The passenger was travelling in 1st class or not
# Pclass_2   - The passenger was travelling in 2nd class or not
# Sex_female - 1 - Female; 0 - Male
# Age        - Passenger's Age
# SibSp      - Number of Siblings/Spouse the passenger travelling with 
# Parch      - Number of Parents/Childern the passenger travelling with
# Fare       - Passenger's Ticket Fare
# Embarked_C - Did the passenger embark from C? 1 yes and 0 no  
# Embarked_Q - Did the passenger embark from Q? 1 yes and 0 no  

head(titanic_data)
str(titanic_data)
dim(titanic_data)

## Converting "Survived", "Pclass_1", "Pclass_2", "Sex_female", "Embarked_C" and "Embarked_Q" to factors

for (i in c("Survived", "Pclass_1", "Pclass_2", "Sex_female", "Embarked_C", "Embarked_Q")){
  titanic_data[,i] = as.factor(titanic_data[,i])
}

# titanic_data[,"Survived"] = as.factor(titanic_data[,"Survived"])
# str(titanic_data[,"Survived"])

str(titanic_data)

# install.packages('caTools', dependencies = T)
library(caTools)

set.seed(10) # this wil give the same sample everytime
split = sample.split(Y = titanic_data$Survived, SplitRatio = 0.8) # dependent variable 
split[1:20] # vector of true and false with 80% trues; irrespective of class of survived

sum(split) # T - 1 and F - 0
711/889

# without set.seed, everytime split will yeild different result
# split1[1:20]
# [1] FALSE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE

train = titanic_data[split,]
head(train)
nrow(train)

test = titanic_data[!split,]
head(test)
nrow(test)

###############################################################################
# Logistic Regression to Predict Survived
###############################################################################

model <- glm(Survived ~ ., # glm - Generalised Linear Models; Dependent ~ Independent
             family = binomial(link = 'logit'), # Assumption of Logistic Regression
             data = train)


###############################################################################
# Evaluate Logistic Regression
###############################################################################

result <- predict(model, test) # log(odds)
result[1:10]

result <- predict(model, test, type = 'response') # predicting prob
result[1:10]

head(test)

result <- ifelse(result > 0.5, 1, 0) # Threshold is 0.5
result[1:10] # converting o/op in class

test$Survived[1:10]

result == test$Survived # true for number of times my model is predicting correctly

accuracy = mean(result == test$Survived)
accuracy # 0.8370787

# result <- predict(model, test, type = 'response') # 
# result <- ifelse(result > 0.9, 1, 0) # Threshold is 0.5
# accuracy = mean(result == test$Survived)
# accuracy 

# Higher the accuracy value, better is the model

##########################################################
## ROC - AUC
##########################################################

# install.packages('ROCR', dependencies = T)
# install ROCR

library(ROCR)

# pred <- prediction(model$fitted.values[1:5], train$Survived[1:5])
# pred

pred <- prediction(model$fitted.values, train$Survived)

head(train)

perf <- performance(pred,"tpr","fpr") # tpr - Sensitivity; fpr = 1 - TNR = Specificity
perf

plot(perf, avg = "threshold", colorize=T, print.cutoffs.at = seq(0,1,.1), text.adj=c(-.2, 1.7),lwd=3, main="ROC Curve")

abline(0,1,lty = 10)

AUCLog2 <- performance(pred, measure = "auc")@y.values[[1]] # subsetting the list
cat("AUC: ", AUCLog2) # Concatenate and Print

# AUC:  0.8551525













