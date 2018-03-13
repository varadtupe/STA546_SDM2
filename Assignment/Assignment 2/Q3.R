#Q3
rm(list = ls())
setwd("~/Documents/GitHub/STA546_SDM2/Assignment/Assignment 2")

#Loading Data
dt = read.csv("Ch10Ex11.csv",header = F)
corDt = cor(dt)


#Clustering

corDist = 1-corDt

hcMod <- hclust(as.dist(corDist), method = "complete")
plot(hcMod, main = "Hierarchical Clustering using complete method")

hcMod <- hclust(as.dist(corDist), method = "single")
plot(hcMod, main = "Hierarchical Clustering using single method")

hcMod <- hclust(as.dist(corDist), method = "average")
plot(hcMod, main = "Hierarchical Clustering using average method")

hcMod <- hclust(as.dist(corDist), method = "ward.D")
plot(hcMod , main = "Hierarchical Clustering using ward method")

hcMod <- hclust(as.dist(corDist), method = "ward.D2")
plot(hcMod , main = "Hierarchical Clustering using ward.D2 method")

