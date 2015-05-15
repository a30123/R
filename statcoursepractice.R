breastdata=read.table(file="C:/Users/Mary/Desktop/breast cancer data/415530a-s8.csv", sep=",",stringsAsFactors=F,header=T)
stem(breastdata$age,scale=0.1)
qqnorm(breastdata$age)
qqline(breastdata$age,col='red')
require(ggplot2)
h<-ggplot(breastdata,aes(x=factor(ERp))) 
 