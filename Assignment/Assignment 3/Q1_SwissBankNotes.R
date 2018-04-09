#Assignment 3 
#Q1
rm(list = ls())
setwd("~/Documents/GitHub/STA546_SDM2/Assignment/Assignment 3")
load("./SwissBankNotes.rdata")

summary(SwissBankNotes)

#Adding isFake
#The first 100 are genuine and the second 100 are counterfeit.
SwissBankNotes$is.fake = 1
SwissBankNotes$is.fake[1:100] = 0
SwissBankNotes$is.fake = as.factor(SwissBankNotes$is.fake)

#Checking summary
summary(SwissBankNotes)

# Correlation panel
my_cols = c("#3EBCC0","#F88179")

panel.cor <- function(x, y){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- round(cor(x, y), digits=2)
  txt <- paste0("", r)
  cex.cor <- 0.6/strwidth(txt)
  text(0.5, 0.5, txt, cex = 3)
}
my_cols = c("#3EBCC0","#F88179")
# Customize upper panel
upper.panel<-function(x, y){
  points(x,y, pch = 19, col = my_cols[as.numeric(SwissBankNotes$is.fake)])
}
# Create the plots
pairs(SwissBankNotes[,c(1:6)], 
      lower.panel = panel.cor,
      upper.panel = upper.panel,cex.labels =1.8
      )



#PCA
pcaNotes = prcomp(SwissBankNotes[,1:6])

#PCA Details
pcaNotes

summary(pcaNotes)

#PCA of first 100 notes
currPCAData = pcaNotes$x[1:100,1:2]
isFake = SwissBankNotes$is.fake[1:100]
plot(currPCAData,col=my_cols[as.numeric(isFake)], main="First two principal components of genuine notes")+
  legend("topright", pch=c(1,1), col=my_cols, c("Not Fake","Fake"),  box.col="darkgreen", cex=.8)

#PCA of next 100 notes
currPCAData = pcaNotes$x[101:200,1:2]
isFake = SwissBankNotes$is.fake[101:200]
plot(currPCAData,col=my_cols[as.numeric(isFake)], main="First two principal components of counterfeit notes")+
  legend("topright", pch=c(1,1), col=my_cols, c("Not Fake","Fake"),  box.col="darkgreen", cex=.8)


currPCAData = pcaNotes$x[,1:2]
isFake = SwissBankNotes$is.fake
plot(currPCAData,col=my_cols[as.numeric(isFake)], main="First two principal components of all notes")+
  legend("topright", pch=c(1,1), col=my_cols, c("Not Fake","Fake"),  box.col="darkgreen", cex=.8)
