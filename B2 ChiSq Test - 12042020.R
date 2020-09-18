# Steps for Hypothesis Testing:
#   
#   Hypothesize about a population by stating H0 and H1. 
#   Build ChiSq Test 
#   Check the p_value
#   Compare the p_value with alpha; if p_value is less than alpha, Rej Ho
#   Result of the test

rm(list = ls())

# Hypothesize about a population by stating H0 and H1. 

# Ho: There is no relationship between the service level and the salary
# Ha: There is relationship between the service level and the salary

tbl3 <- read.csv('restuarant1.csv')
tbl3

#   Build ChiSq Test 

chisq.test(tbl3)

# Pearson's Chi-squared test
# 
# data:  tbl3
# X-squared = 18.658, df = 4, p-value = 0.0009172

#   Check the p_value

pvalue = 0.0009172

#   Compare the p_value with alpha; if p_value is less than alpha, Rej Ho

alpha = 0.05

pvalue < alpha 
# if this is true = whenever p_value is less than alpha; we reject the null hypothesis

#   Result of the test

# Result :: Rej the null hypothesis, there is relationship between the service level and the salary


#############################################################################
rm(list = ls())

df = read.csv('trainl.csv')
head(df)

str(df)

# Hypothesize about a population by stating H0 and H1. 

# Ho: The education of the customer is independent of the loan status or 
# Ho: There is no relationship between education and the loan status of the customer
# 
# H1: The education of the customer is dependent of the loan status or 
# H1: There is relatioship between education and the loan status of the customer

#   Build ChiSq Test 

tbl = table(df$Education, df$Loan_Status)
tbl # contingency table

test = chisq.test(tbl)
test

#   Check the p_value

pvalue = test$p.value
pvalue

#   Compare the p_value with alpha; if p_value is less than alpha, Rej Ho

alpha = 0.05

pvalue < alpha 

# if this is true = whenever p_value is less than alpha; we reject the null hypothesis

#   Result of the test

# Result: Rej the Ho, There is relatioship between education and the loan status of the customer







