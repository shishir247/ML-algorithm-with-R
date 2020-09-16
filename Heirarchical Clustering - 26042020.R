rm(list=ls())

## Heirarchical clustering
## For heirarchical clustering , we are going to use the mtcars dataset

# Look at the column names
?mtcars
colnames(mtcars)

head(mtcars)
dim(mtcars)

# Find distances 
cars.dist <- dist(mtcars) 
cars.dist

View(as.matrix(round(cars.dist,0)))

# use hclust function
clusters <- hclust(cars.dist)
clusters

# create dendograms
plot(clusters)

# apply rectangles for specified no. of clusters
plot(clusters)
rect.hclust(clusters, k = 5)

plot(clusters)
rect.hclust(clusters, k = 4)

# apply rectangles at specified height
plot(clusters)
rect.hclust(clusters, h = 300)

plot(clusters)
rect.hclust(clusters) # Error in rect.hclust(clusters) : specify exactly one of 'k' and 'h'








