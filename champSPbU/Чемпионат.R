setwd("/home/pavel/general/rstud")
champ <- read.csv(file="Чемпион.csv", header=T, sep=";")
champ <- subset(champ, pass!="") 
colnames(champ) <- c("pass","goal")

#Производится удаление плохих данных и их исправление
champ <- subset(champ, pass!="?") 
champ <- subset(champ, goal!="?") 
champ[champ=="Апетян *"] <- "Апетян"
champ[champ=="Ким *"] <- "Ким"

#Не несущие смысловой нагрузки преобразования
champ$pass <- as.character(champ$pass)
champ$goal <- as.character(champ$goal)
kolvo <- c(champ$pass,champ$goal)
kolvo <- factor(kolvo)
champ$pass <- factor(champ$pass)

#Создается таблица people с двумя столбцами (name=фамилия,number=порядковый номер)
people <- levels(kolvo)
people <- as.character(people)
people <- data.frame(name=people,number=seq(1:length(levels(kolvo))))
people[,1] <- as.character(people[,1])

#Создается таблица, ячейки которой содержат количество отданных человеком пасов,
#чья фамилия указана в имени строки, на человека, чья фамилия -- имя столбца 
people0 <- matrix(rep(0),nrow=length(people$name),ncol=length(people$name))                      
rownames(people0) <- people$name
colnames(people0) <- people$name
for (i in 1:length(people$name)) {
  for (k in 1:length(people$name)) {
  people0[i,k] <- length(champ$pass[champ$pass==people[i,1]&champ$goal==people[k,1]])
  } } 

#Все значения из таблицы записываются в один длинный вектор
allins <- rep(0,(length(people$name)*length(people$name))) 
for (i in 1:length(people$name)) {
  for (k in 1:length(people$name)) {
    allins[length(people$name)*(i-1)+k] <- length(champ$pass[champ$pass==people[i,1]&champ$goal==people[k,1]])
  } } 

#Все значения в длинном векторе сортируются (по убыванию)
allins <- sort(allins,decreasing = T)
#Построение конечной таблицы (чьи пасы, и на кого)
tabl1 <- matrix(rep(0),nrow=length(allins[allins!=0]),ncol=3)
peoplex <- people0
for (i in 1:length(tabl1[,1])) {
   tabl1[i,1] <- allins[i]
   tabl1[i,2] <- people[(match(allins[i],peoplex)-1)%%(length(people$name))+1,1]
   tabl1[i,3] <- people[((match(allins[i],peoplex)-1)%/%(length(people$name))+1),1]
   peoplex[tabl1[i,2],tabl1[i,3]] <- 0
}

#Такие же процедуры производятся для другой таблицы (гол+пас в сумме)
peoplepl <- people0
for (i in 1:length(people$name)) {
  for (k in 1:length(people$name)) {
    peoplepl[i,k] <- people0[i,k]+people0[k,i]
  } } 

for (i in 1:length(people$name)) {
  for (k in 1:length(people$name)) {
    allins[length(people$name)*(i-1)+k] <- people0[i,k]+people0[k,i]
  } } 

allins <- sort(allins,decreasing = T)

#В векторе после сортировки содержатся дублирующие друг друга значения
#Первыми членами вектора становятся числа без дублирующих значений 
allins[1:(length(allins[allins!=0])/2)] <- allins[seq(from=1,to=length(allins[allins!=0]),by=2)]

#Строится конечная таблица
tabl2 <- matrix(rep(0),nrow=(length(allins[allins!=0])/2),ncol=3)
peoplex <- peoplepl
for (i in 1:length(people$name)) {
  peoplex[i,1:(i-1)] <- 0
}
for (i in 1:length(tabl2[,1])) {
  tabl2[i,1] <- allins[i]
  tabl2[i,2] <- people[(match(allins[i],peoplex)-1)%%(length(people$name))+1,1]
  tabl2[i,3] <- people[((match(allins[i],peoplex)-1)%/%(length(people$name))+1),1]
  peoplex[tabl2[i,2],tabl2[i,3]] <- 0
}

#Записывается количество сыгранных в одной команде игр
#games <- matrix(rep(0),nrow=length(people$name),ncol=length(people$name))                      
#rownames(games) <- people$name
#colnames(games) <- people$name

#for (i in 1:length(people$name)) {
#  for (k in 1:length(people$name)) {
#    games[i,k] <- games[i,k]+
#  } } 
#teams <- matrix(rep(0),nrow=nofteams,ncol=max(length(people0)))


#Запись таблиц в файлы
write.table(tabl1,file='tabl1.csv',row.names=F,col.names=F,sep=";") 
write.table(tabl2,file='tabl2.csv',row.names=F,col.names=F,sep=";")
