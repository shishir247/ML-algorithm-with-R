rm(list=ls())

# Load the libraries
library(arules)

#library(arulesViz)
library(datasets)

# Load the data set
data(Groceries)
?Groceries

## Lets explore the data before we make any rules:

itemFrequencyPlot(Groceries)
itemFrequencyPlot(Groceries, topN = 20)
itemFrequencyPlot(Groceries, topN=20, type="absolute")

## We will always have to pass the minimum required support and confidence.
## We set the minimum support to 0.001
## We set the minimum confidence of 0.8
## We then show the top 5 rules

9835  * 0.001 # 9.8
# Support - if a particular product is sold 9 times in the month, consider it while creating the rules

# Get the rules

rules <- apriori(Groceries, parameter = list(supp = 0.001, conf = 0.80), control = list(verbose = F))
inspect(rules[1:5])

summary(rules)

# If lift is greater than 1, it suggests that the presence of the items on the LHS has 
# increased the probability that the items on the right hand side will occur on this 
# transaction. 

## we will want the most relevant rules first. Lets say we wanted to have the most 
## likely rules. We can easily sort by confidence by executing the following code.

rules <- sort(rules, by ="confidence", decreasing=TRUE)
inspect(rules[1:5])

rules<-sort(rules, by="support", decreasing=TRUE)
inspect(rules[1:5])

rules <- apriori(Groceries, parameter = 
                   list(supp = 0.001, conf = 0.8, maxlen = 3), control = list(verbose=F))
inspect(rules[1:5])

summary(rules)

# We wanted to target items to generate rules. There are two types of targets we 
# might be interested in that are illustrated with an example of "whole milk":
# 1. What are the customers likely to buy before buying whole milk?
# 2. What are the customers likely to buy if they have purchased whole milk?
# This essentially means we want to set either the Left Hand Side and Right Hand 
# Side.

rules <- apriori(data=Groceries, parameter=list(supp=0.001,
                                              conf = 0.8),
               appearance = list(default = "lhs", rhs = "whole milk"),
               control = list(verbose=F))
rules<-sort(rules, decreasing=TRUE,by="confidence")
inspect(rules[1:5])

rules <- apriori(data=Groceries, 
                 parameter=list(supp=0.001,conf = 0.15,minlen=2),
                 appearance = list(default="rhs",lhs="whole milk"),
                 control = list(verbose=F))

rules<-sort(rules, decreasing=TRUE, by="confidence")
inspect(rules[1:5])




