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
for(i in (1:length(templist))){
  if(templist[i]!=-.5&&templist[i]<3 ){
  shortened=c(shortened,templist[i])
  }
}

shortened2=c()
for(i in (1:length(templist2))){
  if(templist2[i]!=-.5&& templist2[i]<3){
    shortened2=c(shortened2,templist2[i])
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
for(i in (1:length(templist3))){
  if(templist3[i]!=-.5&& templist3[i]<3){
    shortened3=c(shortened3,templist3[i])
  }
}


shortened4=c()
for(i in (1:length(templist4))){
  if(templist4[i]!=-.5&& templist4[i]<3){
    shortened4=c(shortened4,templist4[i])
  }
}


ggplot(NULL,aes(x=shortened3))+geom_histogram(binwidth=.1,fill='gold2',alpha=0.6)
ggplot(NULL,aes(x=shortened4))+geom_histogram(binwidth=.1,fill='firebrick1',alpha=0.5)
#########################################################################################
#########################################################################################

templist5<-unlist(read.csv(file_path5,header=FALSE))
templist6<-unlist(read.csv(file_path6,header=FALSE))


shortened5=c()
for(i in (1:length(templist5))){
  if(templist5[i]!=-.5&& templist5[i]<3){
    shortened5=c(shortened5,templist5[i])
  }
}


shortened6=c()
for(i in (1:length(templist6))){
  if(templist6[i]!=-.5&& templist6[i]<3){
    shortened6=c(shortened6,templist6[i])
  }
}


ggplot(NULL,aes(x=shortened5))+geom_histogram(binwidth=.1,fill='darkorchid1',alpha=0.6)
ggplot(NULL,aes(x=shortened6))+geom_histogram(binwidth=.1,fill='ivory4',alpha=0.5)
hist(templist5,breaks=c(-1,0,0.1,0.2,0.3,0.4,1,10,20),main="slow ascend",col='darkorchid1')
