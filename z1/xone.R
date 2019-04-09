setwd("E:/RStudio/Rstudio/z1")
tourist <- read.csv(file = "tourists2.csv", header = T, sep=";")
# tourist <- read.table(file="tourists.txt",header=T)

#Исключаем строки без данных о доходе и ненужные данные
tourist <- subset(tourist, USD!="*") 
tourist <- subset(tourist, Region!="3") 

is.character(tourist$USD) 
tourist$USD <- as.numeric(as.character(tourist$USD)) 
tourist$Region <- as.character(as.numeric(tourist$Region)) 
tourist$Region <- factor(tourist$Region) 

#Попробуем увидеть на столбчатой диаграмме 
#очевидные выбросы, есть ли они в принципе
moneytour <- tourist[order(tourist$USD),]
moneytour$Region <- factor(moneytour$Region)
moneytour$color[moneytour$Region==1] <- 8
moneytour$color[moneytour$Region==2] <- 4
barplot(sort(tourist$USD),col=moneytour$color,legend.text = c(1,2),ylim=c(0,86000))
        
#Построение графика
ggplot(tourist, aes(USD, fill = Region))+ 
  geom_density(alpha=0.5)  

#Проверка на нормальность
by(tourist$USD, INDICES = tourist$Region, shapiro.test) 
#Применяем возведение в степень и снова проверяем на нормальность
by((tourist$USD)^(-1), INDICES = tourist$Region, shapiro.test) 

#Строим график для новой выборки
touristext <- tourist 
touristext$USD <- (tourist$USD)^(-1) 
ggplot(touristext, aes(USD, fill = Region))+ 
  geom_density(alpha=0.5) 

#Проверяем равенство дисперсий
fligner.test(USD ~ Region, touristext) 

#Выполняется t-test
t.test(USD ~ Region, touristext)