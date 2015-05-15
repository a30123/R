breastdata=read.table(file="C:/Users/Mary/Desktop/breast cancer data/415530a-s8.csv", sep=",",stringsAsFactors=F,header=T)
stem(breastdata$age,scale=0.1)
qqnorm(breastdata$age)
qqline(breastdata$age,col='red')
require(ggplot2)
h<-ggplot(breastdata,aes(x=factor(ERp))) 
h+geom_bar()
qplot(factor(ERp),data=breastdata, geom="bar",fill=factor(ERp))
h+geom_histogram(binwidth=0.05)
h+geom_histogram(binwidth=0.05)+geom_density()

b<-ggplot(breastdata,aes(x=factor(ERp),y=age)) 
b+geom_boxplot()
b+geom_boxplot()+coord_flip()


c<-ggplot(breastdata,aes(factor(ERp),factor(PRp))) 
c+geom_point()
c+geom_point()+stat_smooth()

sba<-ggplot(breastdata,aes(x=factor(ERp),fill=factor(metastases)))
sba+geom_bar()
sba+geom_bar(position="fill")+labs(y="proportion")


d3c<-ggplot(breastdata,aes(ERp,PRp))
d3c+geom_point(aes(colour=age))
