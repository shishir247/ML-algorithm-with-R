rm(list=ls())

data <- read.csv("driver-data.csv")

head(data)
str(data)
dim(data)
colnames(data)

# For the sake of simplicity, you will take only two features : 
# mean distance driven per day and 
# the mean percentage of time a driver was >5 mph over the speed limit. 
# 
# Here are what the data represent:
# * id: Unique Id of the driver.
# * mean_dist_day: Mean distance driven by driver per day
# * mean_over_speed_perc: Mean percentage of time a driver was > 5 mph over the speed limit

################################################################
### Scaling or Standardizing
################################################################

# When we work with an algorithm, where we need to find distances; 
# we would want the measure of the data to similar or range of the data to be smaller. 
# This eases the calculations for large amount of data

head(data)

# id column is indentity column for the drivers

id = data$id # saving id information in id vector

data$id <- NULL # dropping the id column

head(data)

scaled_data = scale(data) # subtract the column mean and divide with the col std
head(scaled_data)

(71.24 - mean(data$mean_dist_day))/sd(data$mean_dist_day) # -0.08979917
(28 - mean(data$mean_over_speed_perc))/sd(data$mean_over_speed_perc) # 1.260455

head(scaled_data)

summary(data)
summary(scaled_data)

# scaling will change the class to matrix
class(scaled_data)

scaled_data <- as.data.frame(scaled_data) # converting to a dataframe

scaled_data$id <- id

head(scaled_data)

################################################################
### kMeans - 2 Clusters
################################################################

set.seed(20) # seed 
driverCluster <- kmeans(scaled_data[,1:2],2) # columns, k

library(ggplot2)
ggplot(scaled_data, aes(data$mean_dist_day, data$mean_over_speed_perc, 
                        color = driverCluster$cluster)) + geom_point()

table(driverCluster$cluster)

################################################################
### kMeans - 4 Clusters
################################################################

set.seed(20) # seed for random number generation
driverCluster <- kmeans(scaled_data[, 1:2], 10) # columns, k

ggplot(scaled_data, aes(data$mean_dist_day, data$mean_over_speed_perc, 
                        color = driverCluster$cluster)) + geom_point()
################################################################
### How do I select optimum value of k? - Number of clusters
################################################################

# Elbow Method for finding the optimal number of clusters

set.seed(20)

# Compute and plot wss for k = 2 to k = 10.
k.max <- 10
wss <- sapply(1:k.max, 
              function(k){kmeans(data, k)$tot.withinss})
wss

plot(1:k.max, wss,
     type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters K",
     ylab= "Total within-clusters sum of squares",
     main = 'Elbow Plot')



