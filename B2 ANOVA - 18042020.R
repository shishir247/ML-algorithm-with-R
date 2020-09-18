# Steps for Hypothesis Testing:
#   
#   Hypothesize about a population by stating H0 and H1. 
#   Build an ANOVA model 
#   Check the p_value
#   Compare the p_value with alpha; if p_value is less than alpha, Rej Ho
#   Result of the test

rm(list=ls())

#--------------------------------------------------------
# Hypothesize about a population by stating H0 and H1. 
#--------------------------------------------------------

# Ho: mu1 = mu2 = mu3 = mu4
# H1: at least one is different

# Ho: There is no effect of Sororities on the grades
# H1/Ha: There is effect of Sororities on the grades

# Ho: There is no significant difference in scores across the Sororities
# H1: There is significant difference in scores across the Sororities

#--------------------------------------------------------
# Build an ANOVA model - test 
#--------------------------------------------------------

df2 = read.csv("Sorority.csv", header = TRUE)
head(df2)
df2

str(df2)
# 'data.frame':	20 obs. of  2 variables:
# $ Score   : num  2.17 1.85 2.83 1.69 3.33 2.63 1.77 3.25 1.86 2.21 ...
# $ Sorority: Factor w/ 4 levels "Sorority 1","Sorority 2",..: 1 1 1 1 1 2 2 2 2 2 ...

#-----------------------------------------------------------------------------------------------------
# ANOVA is used to test relationship between one quantitative and one or more qualitative variable.
#-----------------------------------------------------------------------------------------------------

av = aov(Score ~ Sorority, data = df2)

# first is the quantitative/numerical and second is qualitative/categorical/factor variable

summary(av)

#             Df Sum Sq Mean Sq F value Pr(>F)
# Sorority     3  2.887  0.9624    2.23  0.124
# Residuals   16  6.904  0.4315   

#--------------------------------------------------------
# Check the p_value
#--------------------------------------------------------

pvalue = 0.124 # prob value
alpha = 0.05

# pf(2.23, 3,16, lower.tail = F) p_value

#------------------------------------------------------------------------
# Compare the p_value with alpha; if p_value is less than alpha, Rej Ho
#------------------------------------------------------------------------

pvalue < alpha # if this is true = whenever p_value is less than alpha; we reject the null hypothesis
# [1] FALSE

#------------------------------------------------------------------------
# Result of the test
#------------------------------------------------------------------------

# Result: We do not reject the null hypothesis

# There is no effect of Sororities on the grades

###################################################################################################################

df3 = read.csv("trainl.csv", header = TRUE)
head(df3)

str(df3)

nrow(df3)

# Ho: There is no affect of Education on the Applicant income
# Ha: There is significant affect of Education on the Applicant income

# Ho: Education Level and the Applicant income are independent
# Ha: Education Level and the Applicant income are not independent


av = aov(ApplicantIncome ~ Education, data = df3)
summary(av)

alpha = 0.05
pvalue = 0.000468

pvalue < alpha # if this is true = whenever p_value is less than alpha; we reject the null hypothesis

# Result: We reject the null hypothesis
# There is significant affect of Education on the Applicant income

##############################################################################

#-----------------------------------------------------------------------------------------------------
# ANOVA is used to test relationship between one quantitative and one or more qualitative variable.
#-----------------------------------------------------------------------------------------------------


# Ho: There is no affect of Gender and Education on the Applicant income
# Ha: There is significant affect of Gender and Education on the Applicant income

av = aov(ApplicantIncome ~ Gender + Education, df3)
summary(av)

alpha = 0.05
pvalue_Gender = 0.141675
pvalue_Education = 0.000354

pvalue_Gender < alpha 
pvalue_Education < alpha 
# if this is true = whenever p_value is less than alpha; we reject the null hypothesis

# Result: We do not reject the Null Hypothesis. There is significant affect of Gender on the Applicant income
# Result: We reject the Null Hypothesis. There is significant affect of Education on the Applicant income

















