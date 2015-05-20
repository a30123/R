### created date:5/5/2015
### last modified date:
### author:A30123
### description:


#########################################################################################################
###      #####  #####        #####       ###############    #   ###  ###       ###       ################
###  #########  ########  ########  ####################  #  #  ###  ###  ###  ###  ###  ################
###  #########  ########  ########  ####################  ####  ###  ###  ###  ###  ###  ################
###      #####  ########  ########       ###############  ####  ###  ###       ###       ################
#########################################################################################################


#########################################################################################################
#######################################   IMPORT LIBRARIES    ###########################################
#########################################################################################################
require(gdata)
require(ggplot2)
#########################################################################################################
#######################################   FUNCTIONS           ###########################################
#########################################################################################################


#########################################################################################################
#######################################   INITIALIZING        ###########################################
#########################################################################################################
file_path="C:/Users/A30123.ITRI/Documents/Python Scripts/New_for_event_mining/Try_20150511_TMAl_mean_reconstructed_error_segments/mean_reconstructed_error_flat_region.csv"

file_path2="C:/Users/A30123.ITRI/Documents/Python Scripts/New_for_event_mining/Try_20150511_TMAl_mean_reconstructed_error_segments/mean_reconstructed_error_oscillate_region.csv"

file_path3="C:/Users/A30123.ITRI/Documents/Python Scripts/New_for_event_mining/Try_20150511_TMAl_mean_reconstructed_error_segments/mean_reconstructed_error_steep_ascend_region.csv"

file_path4="C:/Users/A30123.ITRI/Documents/Python Scripts/New_for_event_mining/Try_20150511_TMAl_mean_reconstructed_error_segments/mean_reconstructed_error_steep_descend_region.csv"

file_path5="C:/Users/A30123.ITRI/Documents/Python Scripts/New_for_event_mining/Try_20150511_TMAl_mean_reconstructed_error_segments/mean_reconstructed_error_slow_ascend_region.csv"

file_path6="C:/Users/A30123.ITRI/Documents/Python Scripts/New_for_event_mining/Try_20150511_TMAl_mean_reconstructed_error_segments/mean_reconstructed_error_slow_descend_region.csv"

#########################################################################################################
#######################################   MAIN PROGRAM        ###########################################
#########################################################################################################
templist<-unlist(read.csv(file_path,header=FALSE))
templist2<-unlist(read.csv(file_path2,header=FALSE))


shortened=c()
count1=0
for(i in (1:length(templist))){
  if(templist[i]==-.5){
  count1=count1+1
  }else{
    if(templist[i]>4){
      shortened=c(shortened,4)
    }else{
      shortened=c(shortened,templist[i])
    }
  }
}


shortened2=c()
count2=0
for(i in (1:length(templist2))){
  if(templist2[i]==-.5){
    count2=count2+1
  }else{
    if(templist2[i]>4){
      shortened2=c(shortened2,4)
    }else{
      shortened2=c(shortened2,templist2[i])
    }
  }
}

ggplot(NULL,aes(x=shortened))+geom_histogram(binwidth=.1,fill='darkturquoise',alpha=0.6)
ggplot(NULL,aes(x=shortened2))+geom_histogram(binwidth=.1,fill='darkseagreen',alpha=0.7)
ggplot(NULL,aes(x=templist2))+geom_histogram(binwidth=.1,fill='green',alpha=0.7)
ggplot(NULL,aes(x=templist))+geom_histogram(fill='green',alpha=0.7)

#########################################################################################
#########################################################################################

templist3<-unlist(read.csv(file_path3,header=FALSE))
templist4<-unlist(read.csv(file_path4,header=FALSE))

shortened3=c()
count3=0
for(i in (1:length(templist3))){
  if(templist3[i]==-.5){
    count3=count3+1
  }else{
    if(templist3[i]>4){
      shortened3=c(shortened3,4)
    }else{
      shortened3=c(shortened3,templist3[i])
    }
  }
}


shortened4=c()
count4=0
for(i in (1:length(templist4))){
  if(templist4[i]==-.5){
    count4=count4+1
  }else{
    if(templist4[i]>4){
      shortened4=c(shortened4,4)
    }else{
      shortened4=c(shortened4,templist4[i])
    }
  }
}


ggplot(NULL,aes(x=shortened3))+geom_histogram(binwidth=.1,fill='gold2',alpha=0.6)
ggplot(NULL,aes(x=shortened4))+geom_histogram(binwidth=.1,fill='firebrick1',alpha=0.5)
#########################################################################################
#########################################################################################

templist5<-unlist(read.csv(file_path5,header=FALSE))
templist6<-unlist(read.csv(file_path6,header=FALSE))

shortened5=c()
count5=0
for(i in (1:length(templist5))){
  if(templist5[i]==-.5){
    count5=count5+1
  }else{
    if(templist5[i]>4){
      shortened5=c(shortened5,4)
    }else{
      shortened5=c(shortened5,templist5[i])
    }
  }
}


shortened6=c()
count6=0
for(i in (1:length(templist6))){
  if(templist6[i]==-.5){
    count6=count6+1
  }else{
    if(templist6[i]>4){
      shortened6=c(shortened6,4)
    }else{
      shortened6=c(shortened6,templist6[i])
    }
  }
}

ggplot(NULL,aes(x=shortened5))+geom_histogram(binwidth=.1,fill='darkorchid1',alpha=0.6)
ggplot(NULL,aes(x=shortened6))+geom_histogram(binwidth=.1,fill='ivory4',alpha=0.5)
hist(templist5,breaks=c(-1,0,0.1,0.2,0.3,0.4,1,10,20),main="slow ascend",col='darkorchid1')


ggplot(NULL,aes(x=shortened5))+geom_histogram(data=shortened3,binwidth=.1,fill='gold2',alpha=0.2)+geom_histogram(data=shortened4,binwidth=.1,fill='firebrick1',alpha=0.2)+geom_histogram(data=shortened5,binwidth=.1,fill='darkorchid1',alpha=0.2)+geom_histogram(data=shortened6,binwidth=.1,fill='ivory4',alpha=0.2)

hist1<-hist(shortened,breaks=c(0,0.05,0.1,0.2,0.3,0.4,0.5,1,2,3,4))
hist2<-hist(shortened2,breaks=c(0,0.05,0.1,0.2,0.3,0.4,0.5,1,2,3,4))
hist3<-hist(shortened3,breaks=c(0,0.05,0.1,0.2,0.3,0.4,0.5,1,2,3,4))
hist4<-hist(shortened4,breaks=c(0,0.05,0.1,0.2,0.3,0.4,0.5,1,2,3,4))
hist5<-hist(shortened5,breaks=c(0,0.05,0.1,0.2,0.3,0.4,0.5,1,2,3,4))
hist6<-hist(shortened6,breaks=c(0,0.05,0.1,0.2,0.3,0.4,0.5,1,2,3,4))
c1<-hist1$count
c2<-hist2$count
c3<-hist3$count
c4<-hist4$count
c5<-hist5$count
c6<-hist6$count

ttt=rbind(c2,c4,c3,c1,c5,c6)
dfr<-data.frame(ttt)
dfr$category<-c("oscillating","steep descend","steep ascend","flat","slow ascend","slow descend")
colnames(dfr)<-c("0~0.05","0.05~0.1","0.1~0.2","0.2~0.3","0.3~0.4","0.4~0.5","0.5~1","1~2","2~3",">3","category")


library(reshape2)
mdfr<-melt(dfr,id.vars="category")
library(scales)
p<-ggplot(mdfr,aes(variable,value,fill=category))+geom_bar(position="fill")

barplot(ttt,col=2:7)


ba<-ggplot(mdfr,aes(x=variable,y=value,fill=category))
ba+geom_bar(stat="identity")+scale_fill_manual(values=c('darkturquoise','darkgreen','darkorchid1','ivory4','firebrick1','gold2'))+xlab("bins")+ylab("counts")
