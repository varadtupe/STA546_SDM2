#Q2
rm(list = ls())
setwd("~/Documents/GitHub/STA546_SDM2/Assignment/Assignment 2")

#part a
#Generating data
set.seed(123)
dats <- matrix(rnorm(20 * 50, mean = 5, sd = 10), ncol = 50)
set.seed(124)
dats = rbind(dats,matrix(rnorm(20 * 50, mean = 105, sd = 10), ncol = 50))
set.seed(125)
dats = rbind(dats,matrix(rnorm(20 * 50, mean = 50, sd = 10), ncol = 50))

dats_class <- c(rep(1, 20), rep(2, 20), rep(3, 20))


#part b
pcaMod <- prcomp(dats)
summary(pcaMod)
plot(pcaMod$x[, 1:2], col = 1:3, xlab = "PC 1", ylab = "PC 2", pch = c(4,2,3),main = "Plot of first 2 Principal Components")

#part c
#Kmeans
km <- kmeans(dats, 3, nstart = 20)
km$cluster

plot(dats[, c(1,2)], col = km$cluster, main = "Example k-means")
table(km$cluster,dats_class)


#part d
#Kmeans with 2
km <- kmeans(dats, 2, nstart = 20)
km$cluster

plot(dats[, c(2,3)], col = km$cluster, main = "Example k-means")
table(km$cluster,dats_class)


#part e
#Kmeans with 4
km <- kmeans(dats, 4, nstart = 20)
km$cluster

plot(dats[, c(2,3)], col = km$cluster, main = "Example k-means")
table(km$cluster,dats_class)


#part f
km <- kmeans(pcaMod$x[, 1:2], 3, nstart = 20)
km$cluster

plot(pcaMod$x[, 1:2], col = km$cluster, main = "Example k-means")
table(km$cluster,dats_class)


#part g
dats_scaled = scale(dats)

km <- kmeans(dats_scaled, 3, nstart = 20)
km$cluster

plot(dats[, c(2,3)], col = km$cluster, main = "Example k-means")
table(km$cluster,dats_class)

