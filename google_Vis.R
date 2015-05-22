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
#file_path='C:/Users/A30123.ITRI/Documents/R scripts/New for event mining/Try_20150521_google_Vis/data/run1772_RE-HEMT.AlN_SLs Buffer_Si_028 uGaN 01 for 950 um_E140326D_2045-setpoint.csv'
file_path= 'C:/Users/Mary/Music/Documents/R/New for Event Mining/Try_20150522_googleVis/setpoint/run1772_RE-HEMT.AlN_SLs Buffer_Si_028 uGaN 01 for 950 um_E140326D_2045-setpoint.csv'
file_path2='C:/Users/Mary/Music/Documents/R/New for Event Mining/Try_20150522_googleVis/current/run1772_RE-HEMT.AlN_SLs Buffer_Si_028 uGaN 01 for 950 um_E140326D_2045-current.csv'
#########################################################################################################
#######################################   MAIN PROGRAM        ###########################################
#########################################################################################################

templist<-(read.table(file_path,header=TRUE,sep=","))

templist<-(read.csv(file_path,header=TRUE))
templist2<-(read.csv(file_path2,header=TRUE))

mutate(templist,DataTime=as.POSIXct(DataTime))
templist$DataTime<-as.POSIXct(templist$DataTime)
templist2$DataTime<-as.POSIXct(templist2$DataTime)


#AnnoTimeLine<-gvisAnnotatedTimeLine(Stock,datevar="Date",numvar="Value",idvar="Device",titlevar="Title",annotationvar="Annotation",options=list(displayAnnotations=TRUE,width="600px",height="350px"))


AnnoTimeLine<-gvisAnnotatedTimeLine(templist,datevar="DataTime",numvar="TMAl_1.source",date.format="%Y/%m/%d %H:%M:%S",options=list(displayAnnotations=TRUE,width="600px",height="350px"))
plot(AnnoTimeLine)


templist$new<-rep("setpoint",nrow(templist))
templist2$new<-rep("current",nrow(templist2))

templist3<-rbind(templist,templist2)
AnnoTimeLine<-gvisAnnotatedTimeLine(templist3,datevar="DataTime",numvar="TMAl_1.source",idvar="new",date.format="%Y/%m/%d %H:%M:%S",options=list(displayAnnotations=TRUE,width="600px",height="350px"))