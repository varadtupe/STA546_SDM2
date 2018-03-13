#Q2
rm(list = ls())
setwd("/Users/varadtupe/Documents/GitHub/STA546_SDM2/Assignment/Assignment 1")
library(ElemStatLearn)
library(MASS)

library(ggplot2)

bosData = Boston

summary(bosData)
attach(bosData)
varNames = colnames(bosData)

hist(bosData$crim)
bosData$crim <- ordered(cut(bosData$crim, c(0,10,90), labels = c("low",  "high")))
summary(bosData$zn)
hist(bosData$zn)
bosData$zn <- ordered(cut(bosData$zn, c(-1,20,400), labels = c("low",  "high")))

hist(bosData$indus)
bosData$indus <- ordered(cut(bosData$indus, c(0,15,110), labels = c("low",  "high")))

hist(bosData$chas)
summary(bosData$chas)
bosData$chas <- ordered(cut(bosData$chas, c(-1,.5,2), labels = c("low", "high")))

hist(bosData$nox)
bosData$nox <- ordered(cut(bosData$nox, c(0,0.6,2), labels = c("low",  "high")))

hist(bosData$rm)
bosData$rm <- ordered(cut(bosData$rm, c(0,6,110), labels = c("low",  "high")))

hist(bosData$age)
bosData$age <- ordered(cut(bosData$age, c(-1,60,150), labels = c("low",  "high")))

hist(bosData$dis)
bosData$dis <- ordered(cut(bosData$dis, c(0,4,20), labels = c("low",  "high")))

hist(bosData$rad)
bosData$rad <- ordered(cut(bosData$rad, c(-1,6,40), labels = c("low",  "high")))

hist(bosData$tax)
bosData$tax <- ordered(cut(bosData$tax, c(0,400,1110), labels = c("low",  "high")))

hist(bosData$ptratio)
bosData$ptratio <- ordered(cut(bosData$ptratio, c(0,19,40), labels = c("low",  "high")))

hist(bosData$black)
bosData$black <- ordered(cut(bosData$black, c(0,350,660), labels = c("low",  "high")))

hist(bosData$lstat)
bosData$lstat <- ordered(cut(bosData$lstat, c(0,15,60), labels = c("low",  "high")))

hist(bosData$medv)
bosData$medv <- ordered(cut(bosData$medv, c(0,25,70), labels = c("low",  "high")))

library("arules")
bosTran <- as(bosData, "transactions")

#Item Freqncy plot
itemFrequencyPlot(bosTran, support = 0.02, cex.names = 0.8)

#applying apriori algo
rules <- apriori(bosTran, parameter = list(support = 0.02, confidence = 0.7))



#
summary(rules)
partCRules <- subset(rules, subset = lhs %ain% "crim=low" &rhs %in% "dis=low" & lift>1)

partCRules <- subset(rules, subset = lhs %ain% c("crim=low" ,"dis=low") & lift>1)


summary(partCRules)
inspect(head(sort(partCRules, by = "lift",decreasing = TRUE), n = 10))

partDRules <- subset(rules, subset = rhs %in% "ptratio=low" & lift>1.5)
summary(partDRules) 
inspect(head(sort(partDRules, by = "lift",decreasing = TRUE), n = 10))


lm




