setwd("E:/RStudio/Rstudio/z2")
prestige <- read.table(file="Albuquerque.txt",header=T)

#Исключаем строки без данных о налоге
prestige <- subset(prestige, TAX>0) 

prestige$TAX <- as.numeric(as.character(prestige$TAX)) 
prestige$NE <- as.character(as.numeric(prestige$NE)) 
prestige$NE <- factor(prestige$NE) 

#Попробуем увидеть на столбчатой диаграмме 
#очевидные выбросы, есть ли они в принципе
ttax <- prestige[order(prestige$TAX),]
ttax$NE <- factor(ttax$NE)
ttax$color[ttax$NE==1] <- 3
ttax$color[ttax$NE==0] <- 1
barplot(ttax$TAX,col=ttax$color,ylim=c(0,max(ttax$TAX)),xlim=c(0,6),width=0.035, space=0.95)
legend("topleft",legend=c("N=1","N=0",paste("min =",min(ttax$TAX)), paste("max =",max(ttax$TAX))),fill=c(3,1,0,0)) 

#Построим гистограмму для визуальной оценки нормальности распределений
hist(prestige$TAX[prestige$NE=="1"],col=5, breaks = 12)
hist(prestige$TAX[prestige$NE=="0"],col=7, breaks = 12)

#Проверка на нормальность Шапиро-тестом 
shapiro.test(prestige$TAX[prestige$NE == "1"])
shapiro.test(prestige$TAX[prestige$NE == "0"])

by((prestige$TAX)^(1/3),INDICES=prestige$NE,shapiro.test) 

exprestige <- prestige
exprestige$TAX <- (prestige$TAX)^(1/3)

fligner.test(TAX ~ NE, exprestige)

summary(aov(exprestige$TAX ~ exprestige$NE, data=exprestige))
