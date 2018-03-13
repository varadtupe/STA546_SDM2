#Quiz 1
rm(list = ls())
setwd("/Users/varadtupe/Documents/GitHub/STA546_SDM2/Quiz/Quiz1")
getwd()
###################################################
#Part A: Loading the data
###################################################
library(ISLR)
library(lattice)
library(ggplot2)
colgData = College
attach(colgData)

###################################################
#Part B: Changing rownames
###################################################

#Skipping fix function since rownames already consist of College names
#rownames(colgData)=colgData[,1]
#fix(colgData)

###################################################
#Part C:
###################################################
#i.
summary(colgData)

#ii.
pairs(colgData[,1:10])

#iii.
boxplot(Outstate~Private,
        data = colgData,
        main="Box Plot of Outstate vs Private", 
        xlab="Private",
        ylab="Outstate")
#iv.
elite = rep("No",nrow(colgData))
elite[colgData$Top10perc > 50]="Yes"
elite = as.factor(elite)
colgData$Elite = elite

#Check how many elite universities are there in data set
summary(colgData$Elite)

#Box Plot of Outstate vs Elite
boxplot(Outstate~Elite,
        data = colgData,
        main="Box Plot of Outstate vs Private", 
        xlab="Elite",
        ylab="Outstate")

#v.
par(mfrow=c(2,2))
hist(Room.Board)
hist(Books)
hist(Personal)
hist(PhD)

#vi.
par(mfrow=c(1,1))
cor(colgData[,2:10])

#Correlation graph
panel.cor <- function(x, y, ...)
{
  par(usr = c(0, 1, 0, 1))
  txt <- as.character(format(cor(x, y), digits=2))
  text(0.5, 0.5, txt, cex = 6* abs(cor(x, y)))
}
pairs(colgData, upper.panel=panel.cor)

install.packages("PerformanceAnalytics")
library("PerformanceAnalytics")
#The graph would show predictors with highest correlation 
chart.Correlation(colgData[2:10], histogram=TRUE, pch=19)


#Calculating Acceptance Percent

#Total Acceptance ratio
totAcceptance = sum(colgData$Accept)*100/sum(colgData$Apps)
totAcceptance #67.25%

#Cloning Data
colgDataClone = colgData

#Accept Rate for each colg
colgDataClone$AcceptRate = colgData$Accept*100/colgData$Apps

#Which college has highest Accceptance Rate
colgDataClone[which.max(colgDataClone$AcceptRate),c('Apps','Accept','Enroll','AcceptRate')]

#Which college has Lowest Accceptance Rate
colgDataClone[which.min(colgDataClone$AcceptRate),c('Apps','Accept','Enroll','AcceptRate')]

#Calculating Enrollement Percent

#Total Enrollement ratio
totEnroll = sum(colgData$Enroll)*100/sum(colgData$Accept)
totEnroll #38.63%

#Enroll Rate for each colg
colgDataClone$EnrollRate = colgData$Enroll*100/colgData$Accept

#Which college has highest Enrollment Rate
colgDataClone[which.max(colgDataClone$EnrollRate),c('Apps','Accept','Enroll','EnrollRate')]

#Which college has lowest Enrollment Rate
colgDataClone[which.min(colgDataClone$EnrollRate),c('Apps','Accept','Enroll','EnrollRate')]


