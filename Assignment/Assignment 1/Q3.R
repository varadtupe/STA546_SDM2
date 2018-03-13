#Q3
rm(list = ls())
setwd("/Users/varadtupe/Documents/GitHub/STA546_SDM2/Assignment/Assignment 1")
dt = read.csv("./Q3Data/1.csv")
dt1 = read.csv("./Q3Data/2.csv")
dt = rbind(dt,dt1)
dt1 = read.csv("./Q3Data/3.csv")
dt = rbind(dt,dt1)
#dt1 = read.csv("./Q3Data/4.csv")
#dt = rbind(dt,dt1)
#dt1 = read.csv("./Q3Data/5.csv")
#dt = rbind(dt,dt1)
#write(dt,file = 'data.csv')


summary(dt)
library(rpart)
library("pROC")
library("rpart.plot")
model.controls <- rpart.control(minbucket = 2, minsplit = 4, xval = 10, cp = 0)
fit <- rpart(Class~., data = dt, control = model.controls)

summary(fit)
plot(fit, branch = .3, compress=T, main = "Non Pruned Tree")
text(fit, cex = .5)


prp(fit , fallen.leaves = FALSE, type=4, extra=1, varlen=0, faclen=0, yesno.yshift=-1,cex =.5)
