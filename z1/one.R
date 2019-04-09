setwd("E:/RStudio/Rstudio/z1")
tourist <- read.csv(file = "tourists2.csv", header = T, sep=";")
# tourist <- read.table(file="tourists.txt",header=T)

tourist <- subset(tourist, USD!="*") 
tourist <- subset(tourist, Region!="3") 

is.character(tourist$USD) 
tourist$USD <- as.numeric(as.character(tourist$USD)) 
tourist$Region <- as.character(as.numeric(tourist$Region)) 
tourist$Region <- factor(tourist$Region) 
ggplot(tourist, aes(USD, fill = Region))+ 
  geom_density(alpha=0.5) 

by(tourist$USD, INDICES = tourist$Region, shapiro.test) 
by(log(tourist$USD), INDICES = tourist$Region, shapiro.test) 

touristlg <- tourist 
touristlg$USD <- log(tourist$USD) 
ggplot(touristlg, aes(USD, fill = Region))+ 
  geom_density(alpha=0.5) 

fligner.test(USD ~ Region, touristlg) 

t.test(USD ~ Region, touristlg)