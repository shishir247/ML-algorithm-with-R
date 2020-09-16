rm(list=ls())

##########################################################
## Advertising Data
##########################################################

data <- read.csv('Advertising.csv', header = T)
summary(data)

head(data)

dim(data)

# What are the features/ independent variables/ explainatory variables?
# - **TV:** advertising dollars spent on TV for a single product in a given market (in thousands of dollars)
# - **Radio:** advertising dollars spent on Radio
# - **Newspaper:** advertising dollars spent on Newspaper
# 
# What is the response/ dependent variable/ predictor variable?
# - **Sales:** sales of a single product in a given market (in thousands of items)
# 
# What else do we know?
# - Because the response variable is continuous, this is a **regression** problem.
# - There are 200 **observations** (represented by the rows), and each observation is a single market.

###############################################################################
#### First Linear Model - Simple Linear Model
###############################################################################

model <- lm(Sales ~ TV, data = data) # lm(dependent ~ independent, data)

formula(model)

# Model Evaluation Technique - R-Squared Value

Rsqd <- summary(model)$r.squared
Rsqd # 0.6118751

# Model Evaluation Technique - RMSE

# requires predicted values

predicted_sales <- predict(model, data) # yhat - equation is provided in model from data it gets independent variable
predicted_sales[1:10] # vector[first 10 values] # yhat

data$Sales[1:10] # vector[first 10 values] # yi

(data$Sales - predicted_sales)[1:10] # error
((data$Sales - predicted_sales)^2)[1:10] # squared error
mean((data$Sales - predicted_sales)^2) # # mean squared error
sqrt(mean((data$Sales - predicted_sales)^2)) # sqrt(mean squared error)

RMSE = sqrt(mean((data$Sales - predicted_sales)^2))
RMSE # 3.242322 - same units as your dependent variable

# rmse(data$Sales,predicted_sales)

# To Do :: Build a model for predicting Sales from Radio and Sales from Newspaper


###############################################################################
### Multiple Linear Regression
###############################################################################

model1 <- lm(Sales ~ TV + Newspaper + Radio, data = data) # dependent variable ~ independent variables

# model1 <- lm(Sales ~ ., data = data)

###############################################################

# Model Evaluation Technique - R-Squared Value

Rsqd <- summary(model1)$r.squared # explained variation of the model
Rsqd

# 0.8972106 = 89%

# Model Evaluation Technique - RMSE

formula(model1) 

predicted_sales <- predict(model1, data)
predicted_sales[1:10]
length(predicted_sales)

data$Sales[1:10]

# data$Sales - predicted_sales # error
# (data$Sales - predicted_sales)^2 # squared error
# mean((data$Sales - predicted_sales)^2) # mean squared error

RMSE = sqrt(mean((data$Sales - predicted_sales)^2)) # root mean squared error
RMSE # 1.66857 - same units as your dependent variable

#==========================#

# with TV    61%    3.23
# with ALL   89%    1.66

#==========================#

summary(model1)

# Ho: There is no linear relation between dependent and independent variables
# Ha: There is linear relation between dependent and independent variables

pvalue <- 2.2e-16
alpha <-  0.05

pvalue < alpha # if this is true = whenever p_value is less than alpha; we reject the null hypothesis

# Rej the null hypothesis and there is linear relation between dependent and independent variables

###############################################################

# Ho: b1 = 0 ; TV has no effect on Sales
# Ha: b1 != 0 ; TV has effect on Sales

pvalue <- 2.2e-16
alpha <-  0.05

pvalue < alpha # if this is true = whenever p_value is less than alpha; we reject the null hypothesis

# Rej Ho, TV has effect on Sales

###############################################################

# Ho: b2 = 0 ; Newspaper has no effect on Sales
# Ha: b2 != 0 ; Newspaper has effect on Sales

pvalue <- 0.86 
alpha <-  0.05

pvalue < alpha # if this is true = whenever p_value is less than alpha; we reject the null hypothesis

# Do not rej Ho, Newspaper has no effect on Sales

###############################################################

# Ho: b3 = 0 ; Radio has no effect on Sales
# Ha: b3 != 0 ; Radio has effect on Sales

pvalue <- 2e-16
alpha <-  0.05

pvalue < alpha # if this is true = whenever p_value is less than alpha; we reject the null hypothesis

# Rej Ho, Radio has effect on Sales

###############################################################

# y = b0 + b1x1 + b2x2 + b3x3

# Sales = 2.938889 + 0.045765 * TV + (-0.001037) * Newspaper + 0.188530 * Radio

Sales_1 = 2.938889 + 0.045765 * 1 + (-0.001037) * 1 + 0.188530 * 1
Sales_1

Sales_1001 = 2.938889 + 0.045765 * 1001 + (-0.001037) * 1 + 0.188530 * 1
Sales_1001

Sales_1001 - Sales_1 # 45.765 = b1 * 1000

Sales_2 = 2.938889 + 0.045765 * 2 + (-0.001037) * 1 + 0.188530 * 1
Sales_2

Sales_2 - Sales_1 # b1

# How do we interpret the **TV coefficient** ?

# - For a given amount of Radio and Newspaper ad spending, **a "unit" increase in TV ad spending** 
# is associated with a **0.0475 (b1) "unit" increase in Sales**.

# **an additional $1,000 spent on TV ads** is associated with an **increase in sales of 47.5 items**.
