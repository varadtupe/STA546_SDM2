#Q1
rm(list = ls())
setwd("~/Documents/GitHub/STA546_SDM2/Assignment/Assignment 2")

#Librarys
source("http://bioconductor.org/biocLite.R")
#biocLite()
#biocLite("multtest")
#biocLite("cluster")
#install.packages("fpc")

library("multtest")
library("fpc")
library("cluster")


#Loading Data
usAr = USArrests

summary(usAr)

#idx <- sample(c(1:length(usAr[,1])), 30)
#usAr_red <- usAr[idx, ]

# Calculate the distance, and perform hierarchical clustering.
d <- dist(usAr)
dim(as.matrix(d))
hc <- hclust(d, method = "complete")

#Plotting dendrogram
plot(hc, hang = -1)

#Cutting dendrogram at height of 3
cutTreeDt =(cutree(hc, k = 3))
cutTreeDt

#Scaling the data
scaled_usAr = scale(usAr)


# SCALED : Calculate the distance, and perform hierarchical clustering.
dsc <- dist(scaled_usAr)
dim(as.matrix(dsc))
hc_scaled <- hclust(dsc, method = "complete")

# SCALED: Plotting dendrogram
plot(hc_scaled, hang = -1)

# SCALED: Cutting dendrogram at height of 3
cutTreeDt_scaled =(cutree(hc_scaled, k = 3))
cutTreeDt_scaled

#comparison
hclustDF = as.data.frame(cutTreeDt)
hclustDF$Scaled = cutTreeDt_scaled
colnames(hclustDF) = c("Without_Scaling","With_Scaling")

hclustDFStats =(table(cutTreeDt,cutTreeDt_scaled)) 
