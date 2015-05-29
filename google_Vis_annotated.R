### created date:5/21/2015
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
library(googleVis)
library(dplyr)

#########################################################################################################
#######################################   FUNCTIONS           ###########################################
#########################################################################################################


#########################################################################################################
#######################################   INITIALIZING        ###########################################
#########################################################################################################
#file_path='C:/Users/A30123.ITRI/Documents/R scripts/New for event mining/Try_20150521_google_Vis/data/run1767_2040-setpoint.csv'
file_path='C:/Users/A30123.ITRI/Documents/R scripts/New for event mining/Try_20150521_google_Vis/data/run1772_RE-HEMT.AlN_SLs Buffer_Si_028 uGaN 01 for 950 um_E140326D_2045-setpoint.csv'
file_path2='C:/Users/A30123.ITRI/Documents/R scripts/New for event mining/Try_20150521_google_Vis/data/run1772_RE-HEMT.AlN_SLs Buffer_Si_028 uGaN 01 for 950 um_E140326D_2045-current.csv'
# file_path= 'C:/Users/Mary/Music/Documents/R/New for Event Mining/Try_20150522_googleVis/setpoint/run1772_RE-HEMT.AlN_SLs Buffer_Si_028 uGaN 01 for 950 um_E140326D_2045-setpoint.csv'
# file_path2='C:/Users/Mary/Music/Documents/R/New for Event Mining/Try_20150522_googleVis/current/run1772_RE-HEMT.AlN_SLs Buffer_Si_028 uGaN 01 for 950 um_E140326D_2045-current.csv'
#########################################################################################################
#######################################   MAIN PROGRAM        ###########################################
#########################################################################################################
templist<-(read.csv(file_path,header=TRUE))
templist2<-(read.csv(file_path2,header=TRUE))
templist$DataTime<-as.POSIXct(templist$DataTime)
templist2$DataTime<-as.POSIXct(templist2$DataTime)


templist$new<-rep("setpoint",nrow(templist))
templist2$new<-rep("current",nrow(templist2))
templist2$TMAl_1<-NULL

values<-templist$TMAl_1.source
values2<-templist2$TMAl_1.source


len_of_run=length(values)
relative<-abs(values-values2)/values
alarm<-(relative>0.01)+0
consecutives<-alarm[2:len_of_run]-alarm[1:(len_of_run-1)]
thth<-which((consecutives!=0))
shsh<-(thth[2:length(thth)]-thth[1:(length(thth)-1)])
jiji<-split(shsh,1:2)
keykey<-jiji$'1'
index1<-which(keykey>5)
index2<-2*index1-1
index3<-thth[index2]
# len_of_run=length(values)
# bool<-((relative[1:(len_of_run-10)])>0.01)
# bool<-bool*(relative[2:(len_of_run-9)]>0.01)
# bool<-bool*(relative[3:(len_of_run-8)]>0.01)
# bool<-bool*(relative[4:(len_of_run-7)]>0.01)
# bool<-bool*(relative[5:(len_of_run-6)]>0.01)
# bool<-bool*(relative[6:(len_of_run-5)]>0.01)
# bool<-bool*(relative[7:(len_of_run-4)]>0.01)
# bool<-bool*(relative[8:(len_of_run-3)]>0.01)
# bool<-bool*(relative[9:(len_of_run-2)]>0.01)
# bool<-bool*(relative[10:(len_of_run-1)]>0.01)
# bool<-bool*(relative[11:(len_of_run)]>0.01)
# 
# alarm<-c(0,0,0,0,0,bool)+0
# replace(alarm,alarm==0,NA)
# replace(alarm,alarm==1,"over 10 consecutive points")

showthis<-rep(NA,nrow(templist))
showthis[(index3+1)]<-rep("over 5 consecutive points beyond accuracy",length(index3))
templist$annotation<-showthis
templist2$annotation<-rep(NA,nrow(templist2))


templist3<-rbind(templist,templist2)

AnnoTimeLine2<-gvisAnnotatedTimeLine(templist3,datevar="DataTime",numvar="TMAl_1.source",idvar="new",annotationvar="annotation",date.format="%Y/%m/%d %H:%M:%S",options=list(displayAnnotations=TRUE,width="1300",height="350px"))
plot(AnnoTimeLine2)
