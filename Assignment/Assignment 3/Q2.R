#Assignment 3 
#Q2
rm(list = ls())
setwd("~/Documents/GitHub/STA546_SDM2/Assignment/Assignment 3")
load("./primate.scpulae.rdata")

library("cluster")
require("class")
summary(primate.scapulae)

primate.scapulae$gamma[is.na(primate.scapulae$gamma)] = 59.11
mean(primate.scapulae$gamma)

d = dist(primate.scapulae[1:9])
dim(as.matrix(d))

# Complete Linkage
hc = hclust(d, method = "complete")

plot(hc, hang = -1)

cutTreeDt =(cutree(hc, k = 5))
cutTreeDt
table(cutTreeDt,primate.scapulae$classdigit)

# Single Linkage

hc = hclust(d, method = "single")

plot(hc, hang = -1)

cutTreeDt =(cutree(hc, k = 5))
cutTreeDt
table(cutTreeDt,primate.scapulae$classdigit)


# Average Linkage

hc = hclust(d, method = "average")

plot(hc, hang = -1)

cutTreeDt =(cutree(hc, k = 5))
cutTreeDt
table(cutTreeDt,primate.scapulae$classdigit)




#K Means

km = kmeans(d,5)
table(km$cluster,primate.scapulae$classdigit)

