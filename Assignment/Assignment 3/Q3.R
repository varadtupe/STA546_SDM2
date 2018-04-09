#Q3

rm(list = ls())
setwd("~/Documents/GitHub/STA546_SDM2/Assignment/Assignment 3")


#Loading Library
#install.packages("kohonen")
#install.packages("phyclust")
library(phyclust)
library(kohonen)
library(ElemStatLearn)

# Load Data
data(nci)
nciScaledData = scale(nci)

nciKm = kmeans(nciScaledData,2)
nciKm$cluster

#SOM

nciSom <- som(X = as.matrix(nciScaledData), grid = somgrid(5,5,"hexagonal"))
somRadius = c(0.1,0.4,0.8,2,4,10)
kMeansK = c(2,5,10,20)

simlarClust = data.frame("kMeansK" = 0,"somRad" = 0,"similarity" = 0)

for(i in c(1:6)){
  for(j in c(1:4)){
    kmens = kmeans(nciScaledData,kMeansK[j])
    nciSom = som(X = as.matrix(nciScaledData), grid = somgrid(2,1,"hexagonal"), radius =  somRadius[i] )
    sim = RRand(kmens$cluster, nciSom$unit.classif)$Rand
    simDf = data.frame("kMeansK" = kMeansK[j],"somRad" = somRadius[i],"similarity" = sim)
    simlarClust = rbind(simlarClust,simDf)
  }
  
}
#Ignore the first record
simlarClust

plot(simlarClust$kMeansK,simlarClust$somRad)
