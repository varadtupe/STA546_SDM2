#Question 1
rm(list = ls())
getwd()
setwd("/Users/varadtupe/Documents/GitHub/STA546_SDM2/Assignment/Assignment 1")

mat = matrix(c(4,5,0,5,1,0,3,2, 0,3,4,3,1,2,1,0, 2,0	,	1,	3,	0,	4,	5,	3),3,8,byrow = TRUE)
rownames(mat) = c("A","B","C")
colnames(mat) = c("a","b","c","d","e","f","g","h")
mat1 = mat
#install.packages("proxy")
library(proxy)

for (i in 1:3){
  for (j in 1:8){
    if(mat1[i,j] != 0){
      mat1[i,j]  = 1
    }
  }
}

dist(mat1, method = "Jaccard")

dist(mat1, method = "Cosine")

#Part B
matBin = mat
for (i in 1:3){
  for (j in 1:8){
    if(matBin[i,j] < 3){
      matBin[i,j]  = 0
    }else{
      matBin[i,j]  = 1
    }
  }
}

dist(matBin, method = "Jaccard")

dist(matBin, method = "Cosine")

#Part C

matAvg = mat

for (i in 1:3){
  for (j in 1:8){
    if(matAvg[i,j] != 0){
      matAvg[i,j]  = (sum(matAvg[i,])/6)
    }
  }
}

dist(matAvg, method = "Jaccard")

dist(matAvg, method = "Cosine")
