
setwd("E:/RStudio/Rstudio/z3")
acid.drop <- read.table("Леденцы.csv", header=T, sep=";")



set.seed(1) 
wss <- (nrow(acid.drop)-1)*sum(apply(acid.drop,1,var))
for (i in 1:40) wss[i] <- sum(kmeans(acid.drop, 
                                     centers=i, nstart = 100)$withinss)


plot(1:40, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares", xlim = c(0, 15))


fit1 <- kmeans(acid.drop, 4)

options(digits=3) #округление до трех знаков
t(fit1$centers)

options(digits=2)
fit1$withinss

fit <- kmeans(acid.drop, 3, nstart = 100)

options(digits=3)
t(fit$centers)

options(digits=2)
fit$withinss


lollypop.dist <- dist(acid.drop,method = "euclidean")
acid.drop_cmd <- cmdscale(lollypop.dist)
plot(acid.drop_cmd, col = fit$cluster, xlab = "Index", ylab = "Y", main = "Multidimensional Scaling, k = 3")


lollypop.dist <- dist(acid.drop,method = "euclidean")
acid.drop_cmd <- cmdscale(lollypop.dist)
plot(acid.drop_cmd, col = fit1$cluster, xlab = "Index", ylab = "Y", main = "Multidimensional Scaling, k = 4")
