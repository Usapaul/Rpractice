setwd("E:/RStudio/Rstudio/z1")
tourist <- read.csv(file = "tourists2.csv", header = T, sep=";")
# tourist <- read.table(file="tourists.txt",header=T)

#��������� ������ ��� ������ � ������ � �������� ������
tourist <- subset(tourist, USD!="*") 
tourist <- subset(tourist, Region!="3") 

is.character(tourist$USD) 
tourist$USD <- as.numeric(as.character(tourist$USD)) 
tourist$Region <- as.character(as.numeric(tourist$Region)) 
tourist$Region <- factor(tourist$Region) 

#��������� ������� �� ���������� ��������� 
#��������� �������, ���� �� ��� � ��������
moneytour <- tourist[order(tourist$USD),]
moneytour$Region <- factor(moneytour$Region)
moneytour$color[moneytour$Region==1] <- 8
moneytour$color[moneytour$Region==2] <- 4
barplot(sort(tourist$USD),col=moneytour$color,legend.text = c(1,2),ylim=c(0,86000))
        
#���������� �������
ggplot(tourist, aes(USD, fill = Region))+ 
  geom_density(alpha=0.5)  

#�������� �� ������������
by(tourist$USD, INDICES = tourist$Region, shapiro.test) 
#��������� ���������� � ������� � ����� ��������� �� ������������
by((tourist$USD)^(-1), INDICES = tourist$Region, shapiro.test) 

#������ ������ ��� ����� �������
touristext <- tourist 
touristext$USD <- (tourist$USD)^(-1) 
ggplot(touristext, aes(USD, fill = Region))+ 
  geom_density(alpha=0.5) 

#��������� ��������� ���������
fligner.test(USD ~ Region, touristext) 

#����������� t-test
t.test(USD ~ Region, touristext)