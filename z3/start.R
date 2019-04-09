setwd("E:/RStudio/Rstudio/z3")
edmins <- read.csv(file = "Edmins.csv", header = T, sep=";")

colnames(edmins) <- c("sex","age","channel","profession","press")

fedmins <- edmins
fedmins$sex[fedmins$sex==0] <- 2
fedmins$sex <- factor(fedmins$sex)
fedmins$age <- factor(fedmins$age)
fedmins$channel <- factor(fedmins$channel)
fedmins$profession <- factor(fedmins$profession)
fedmins$press <- factor(fedmins$press)

for (i in 1:5) {
    xlength <- c(1:length(levels(fedmins[,i])))
    for (k in 1:length(levels(fedmins[,i]))) {
    xlength[k] <- length(fedmins[,i][fedmins[,i]==k])
    }
    names(fedmins)[i]
    str(names(fedmins)[i]); str(xlength)
}

edmins$sex=scale(edmins$sex)
edmins$age=scale(edmins$age)
edmins$channel=scale(edmins$channel)
edmins$profession=scale(edmins$profession)
edmins$press=scale(edmins$press)
str(Edmins)

set.seed(1) 
graph <- (nrow(edmins)-1)*sum(apply(edmins,2,var))
for (i in 2:12) graph[i] <- sum(kmeans(edmins, 
                                     centers=i, nstart = 10)$withinss)


plot(1:12, graph, type="b", xlab="Number of clusters",
     ylab="Sum")


clus4 <- kmeans(edmins, 4)
#t(clus4$centers)
clus4$withinss

clus3 <- kmeans(edmins, 3)
#t(clus3$centers)
clus3$withinss

clus2 <- kmeans(edmins, 2)
#t(clus2$centers)
clus2$withinss


distclus <- dist(edmins,method = "euclidean")
lledmins <- cmdscale(distclus)
plot(lledmins, col = clus3$cluster)


distcl=dist(edmins, method = "manhattan", diag = FALSE, upper = FALSE, p = 2)
hcl=hclust(distcl, method = "ward.D2", members = NULL)
plot(hcl, hang = 1)
rect.hclust(hcl,3, border="red")


Edmins_Up=edmins

c=cutree(h,3)
Edmins$clusters=as.factor(c)
Edmins_Up$clusters=as.factor(c)
Edmins1=subset(Edmins_Up,Edmins_Up$clusters=="1")
Edmins2=subset(Edmins_Up,Edmins_Up$clusters=="2")
Edmins3=subset(Edmins_Up,Edmins_Up$clusters=="3")
plot(cmdscale(d_man), col = Edmins$clusters, xlab = "Index", ylab = "Y")

table(edmins$clusters, Edmins_Up$sex)
table(edmins$clusters, Edmins_Up$age)
table(edmins$clusters, Edmins_Up$channe)
table(edmins$clusters, Edmins_Up$profession)
table(edmins$clusters, Edmins_Up$press)




