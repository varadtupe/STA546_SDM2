#Q1
rm(list = ls())
setwd("~/Documents/GitHub/STA546_SDM2/Quiz/Quiz2")
x1 = c(1,1,0,5,6,4)
x2 = c(4,3,4,1,2,0)
df = as.data.frame(x1)
df$x2 = x2
df$label = 1

set.seed(14)
smp = sample(6,3)
df$label[smp[1]] = 2
df$label[smp[2]] = 2
df$label[smp[3]] = 2

#Part A
plot(df$x1, df$x2,col = df$label, pch = 20,cex =3)



#Computing Centroid
c1 =c(0,0)
c2 =c(0,0)

#Centroid of class 1
c1[1] = mean(df[which(df$label == 1),"x1"])
c1[2] = mean(df[which(df$label == 1),"x2"])


#Centroid of class 2
c2[1] = mean(df[which(df$label == 2),"x1"])
c2[2] = mean(df[which(df$label == 2),"x2"])

#Plotting
plot(df$x1, df$x2,col = df$label, pch = 20,cex =3)
points(c1[1], c1[2], col = 1, pch = 4,cex = 4)
points(c2[1], c2[2], col = 2, pch = 4 ,cex = 4)

#Rearrainging lables
df$label = c(1,1,1,2,2,2)


#Repeating

#plot(df$x1, df$x2,col = df$label, pch = 20,cex =3)
c1 =c(0,0)
c2 =c(0,0)

#Centroid of class 1
c1[1] = mean(df[which(df$label == 1),"x1"])
c1[2] = mean(df[which(df$label == 1),"x2"])


#Centroid of class 2
c2[1] = mean(df[which(df$label == 2),"x1"])
c2[2] = mean(df[which(df$label == 2),"x2"])

#Plotting
plot(df$x1, df$x2,col = df$label, pch = 20,cex =3)
points(c1[1], c1[2], col = 1, pch = 4,cex=4)
points(c2[1], c2[2], col = 2, pch = 4,cex = 4)

