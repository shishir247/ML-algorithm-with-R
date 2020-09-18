rm(list = ls())

# Steps for Hypothesis Testing:
#   
#   1. Hypothesize about a population by stating H0 and H1.
#   2. Calculate the test statistic 
#   3. Specify the level of significance (alpha) and calculate the pvalue or the critical value
#   4. Compare the p_value with alpha; if p_value is less than alpha, Rej Ho
#                        OR 
#   4. Compare the critical value; Check if it lies in rejection region, if yes, reject ho.

#      Looking at at the normal curve.

#   5. Result of the test

##############################################
##### Two Sided Test for Population Means
##############################################

# Problem
# A manufacturer produces bolts with a thickness of exactly 1 inch (allegedly).
# A customer takes a random sample of 100 bolts and finds that mean is 1.2 inches. Assume the 
# population standard deviation is 0.40 inches. 

# Should the manufacturer's claim, that the bolts are exactly 
# 1 inch (on average) be rejected? Test at alpha = 0.01.

# 1. Formulate the Hypothesis

# We will always create the hypothesis for the population and test it on the sample

# Ho: The bolts have a thickness of exactly 1 inch
# H1: The bolts do not have a thickness of exactly 1 inch

# Ho: mu = 1
# H1: mu != 1

mu = 1         # Population mean; mean under the hypothesis
xbar = 1.2     # Sample mean
sigma = 0.4    # Population SD
n = 100        # Sample size

# 2. Calculate the test statistic 

# take a decision 

# Assumption : xbar follows N(mu, sigma/sqrt(n)) ---- Central Limit Theorem

z = (xbar - mu)/(sigma/sqrt(n)) # z_cal
z # 5

# 3. Specify the level of significance (alpha) and calculate the critical value

alpha = 0.01

alpha/2

Critical_Value_neg = qnorm(alpha/2) # alpha/2 will be considered to be on left hand side
Critical_Value_neg
# -2.575829

Critical_Value_pos = qnorm(alpha/2, lower.tail = F) # (1-alpha/2) will be considered to be on right hand side
Critical_Value_pos
# 2.575829

# 4. Compare the critical value; Check if it lies in rejection region.
#      Looking at the normal curve.

z

confidence_interval = c(Critical_Value_neg, Critical_Value_pos)
confidence_interval


# (-2.575829  2.575829)

# z lies in the rejection region, so we reject the Ho hypothesis

# 5. Result of the test

# Rej the Ho

# The bolts do not have a thickness of exactly 1 inch


##############################################
##### Two Sided Test for Population Means
##############################################

rm(list = ls())

# Problem
# Suppose the mean weight of King Penguins found in an Antarctic colony last year 
# was 15.4 kg. In a sample of 35 penguins same time this year in the same colony, 
# the mean penguin weight is 14.6 kg. Assume the population standard deviation is 2.5 kg.

# At 0.05 significance level, can we reject the null hypothesis that the mean penguin 
# weight does not differ from last year?

# 1. Formulate the Hypothesis

# We will always create the hypothesis for the population ans test it on the sample

# Ho:	The mean penguin weight does not differ from last year
# mu = 15.4 kgs

##############################################################################
### When we say differ, we mean statistically or significantly different
### We DO NOT mean numerical difference
##############################################################################

# H1 or Ha:	The mean penguin weight differs from last year
# mu != 15.4 kgs

mu = 15.4            # Population mean; mean under the hypothesis
xbar = 14.6          # Sample mean
sigma = 2.5          # Population SD
n = 35               # Sample size

# 2. Calculate the test statistic 

# Assumptiom: xbar follows N(mu, sigma/sqrt(n)) --- Central Limit Theorem

z = (xbar - mu)/(sigma/sqrt(n))
z # -1.893146

# 3. Specify the level of significance (alpha) and calculate the pvalue or the critical value

alpha = 0.05

alpha/2 # 0.025

Critical_Value_neg = qnorm(alpha/2)
Critical_Value_neg
# -1.959964

Critical_Value_pos = qnorm(1-alpha/2)
Critical_Value_pos
# 1.959964

#   4. Compare the critical values/ confidence interval with test statistic and rej/ do not rej the Ho

z

confidence_interval = c(Critical_Value_neg, Critical_Value_pos)
confidence_interval

# Non-critical Region

# Since, the test statictic lies in the confidence_interval region, 
# we do not rej the Ho.

# We do not have enough evidence to reject the null hypothesis

# 5. Result of the test

# Do not rej the Ho

# H1 or Ha:	The mean penguin weight differs from last year

#########################################
##### One Sided Test for Population Means
#########################################

rm(list = ls())

# Problem
# Suppose the manufacturer claims that the mean lifetime of a light bulb is more than 10,000 hours.
# In a sample of 30 light bulbs, it was found that they only last 9,900 hours on average. 
# Assume the population standard deviation is 120 hours. 

# At .05 significance level, can we reject the claim by the manufacturer?

# 1. Formulate the Hypothesis

# We will always create the hypothesis for the population ans test it on the sample

# Ho:	The mean lifetime of a light bulb is more than 10,000 hours
# mu > = 10,000

# H1 or Ha:	The mean lifetime of a light bulb is not more than 10,000 hours
# mu < 10,000

mu = 10000               # Popluation mean; mean under the hypothesis
xbar = 9900              # sample mean
sigma = 120              # Popluation SD
n = 30  

# 2. Calculate the test statistic 

# Assumptiom: xbar follows N(mu, sigma/sqrt(n)) --- Central Limit Theorem

z = (xbar - mu)/(sigma/sqrt(n))
z #  -4.564355

# 3. Specify the level of significance (alpha) and calculate the pvalue or the critical value

alpha = 0.05

# Since, we are working with one sided test, we will not work with alpha/2

# Rejection Region or Critical Region will be on left

Critical_Value_neg = qnorm(alpha)
Critical_Value_neg
# -1.644854

#   4. Compare the critical value with test statistic rej the Ho

z # -4.564355

Critical_Value_neg # -1.644854

z > Critical_Value_neg 

# 5. Result of the test

# Since, test statistic lies in Critical Region, we rej the NULL Hypothesis

# H1 or Ha:	The mean lifetime of a light bulb is not more than 10,000 hours

#####################################################################
# One Sided Test for Population Means
#####################################################################

rm(list = ls())

# Suppose the food label on a cookie bag states that there is at most 2 grams of saturated 
# fat in a single cookie. In a sample of 35 cookies, it is found that the mean amount of 
# saturated fat per cookie is 2.1 grams. Assume that the population standard deviation is 
# 0.25 grams. At 0.05 significance level, can we reject the claim on food label?

# 1. Formulate the Hypothesis

# We will always create the hypothesis for the population ans test it on the sample

# Ho: saturated fat in a single cookie < = 2 grams; mu <= 2

# Ha: saturated fat in a single cookie > 2 grams; mu > 2

mu = 2                  # Popluation mean; mean under the hypothesis
xbar = 2.1              # sample mean
sigma = 0.25            # Popluation SD
n = 35  

# 2. Calculate the test statistic 

# Assumptiom: xbar follows N(mu, sigma/sqrt(n)) --- Central Limit Theorem

z = (xbar - mu)/(sigma/sqrt(n))
z # 2.366432

# 3. Specify the level of significance (ÃÂ±) and calculate the pvalue or the critical value

alpha = 0.05

Critical_Value_pos = qnorm(1-alpha) # qnorm(alpha, lower.tail = F)
Critical_Value_pos
# 1.644854

#   4. Compare the critical value with test statistic rej the Ho

z
Critical_Value_pos

Critical_Value_pos < z # TRUE

# 5. Result of the test

# Since, test statistic lies in Critical Region, we rej the NULL Hypothesis

# Rej the Ho

# Ha: saturated fat in a single cookie > 2 grams; mu > 2

####==============================================================================

#   3. Specify the level of significance (alpha) and calculate the pvalue

####==============================================================================

alpha

z

# pvalue = p = Pr(Z >= Test Statistic)

pvalue = pnorm(z, lower.tail = F) # z to right
pvalue
# 0.008980239

####==============================================================================

#   4. Compare the p_value with alpha; if p_value is less than alpha, Rej Ho

####==============================================================================

pvalue < alpha # TRUE - we rej the Ho, if pvalue < alpha

# 5. Result of the test

# Since, pvalue < alpha, we rej Ho

# Ha: saturated fat in a single cookie > 2 grams; mu > 2

#####################################################################
# Population Mean with Unknown Population Variance
#####################################################################

rm(list = ls())

# Problem
# 
# Suppose the food label on a cookie bag states that there is at most 2 grams
# of saturated fat in a single cookie. In a sample of 35 cookies, it is found
# that the mean amount of saturated fat per cookie is 2.1 grams. Assume that the
# sample standard deviation is 0.3 gram. At .05 significance level, can we reject
# the claim on food label?

# 1. Formulate the Hypothesis

# We will always create the hypothesis for the population ans test it on the sample

# Ho: saturated fat in a single cookie < = 2 grams; mu <= 2
# Ha: saturated fat in a single cookie > 2 grams; mu > 2

mu = 2                  # Popluation mean; mean under the hypothesis
xbar = 2.1              # sample mean
s = 0.3                 # Sample SD
n = 35  

# 2. Calculate the test statistic 

# Assumptiom: xbar follows t(mu, s/sqrt(n)) --- Central Limit Theorem

t = (xbar - mu)/(s/sqrt(n))
t # 1.972027

# 3. Specify the level of significance (Î±) and calculate the pvalue

alpha = 0.05
pvalue = pt(t, lower.tail = F) # Error in pt(t) : argument "df" is missing, with no default

pvalue = pt(t, lower.tail = F, n-1)
pvalue # 0.02839295

#   4. Compare the p_value with alpha; if p_value is less than alpha, Rej Ho

pvalue < alpha # TRUE - we rej the Ho, if pvalue < alpha

# 5. Result of the test

# Since, pvalue < alpha, we rej Ho; saturated fat in a single cookie > 2 grams; mu > 2
