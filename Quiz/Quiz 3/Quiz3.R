rm(list = ls())
setwd("~/Documents/GitHub/STA546_SDM2/Quiz/Quiz 3")
data(state)


stData = state.x77


#Checking summary
summary(stData)


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
  points(x,y, pch = 19, col = my_cols[1])
}
# Create the plots
pairs(stData, 
      lower.panel = panel.cor,
      upper.panel = upper.panel,cex.labels =1.8
)



pcaState = prcomp(stData)
plot(pcaState$x[,1:2], main="First two principal components")+
text(pcaState$x[, 1], pcaState$x[, 2], rownames(pcaState$x), 
     cex = 0.6, pos = 4, col = "black")

#hclust
d = dist(stData)
dim(as.matrix(d))

# Complete Linkage
hc = hclust(d, method = "complete")
plot(hc, hang = -1)

cutTreeDt =(cutree(hc, k = 5))
cutTreeDt


km = kmeans(d,5)
km$cluster
