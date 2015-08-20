#### created date:Aug 5 2015
#### last modified date:
#### author:A30123
#### description:


#########################################################################################################
###      #####  #####        #####       ###############    #   ###  ###       ###       ################
###  #########  ########  ########  ####################  #  #  ###  ###  ###  ###  ###  ################
###  #########  ########  ########  ####################  ####  ###  ###  ###  ###  ###  ################
###      #####  ########  ########       ###############  ####  ###  ###       ###       ################
#########################################################################################################


#########################################################################################################
#######################################   IMPORT LIBRARIES    ###########################################
#########################################################################################################

#########################################################################################################
########################################   FUNCTIONS           ##########################################
#########################################################################################################
library(ggplot2)
library("gridExtra")#used by grid.arrange
#########################################################################################################
#######################################   INITIALIZING        ###########################################
#########################################################################################################
file_name="桌面/Summer_2015/PHM/PHMtrain/"
output_folder="桌面/PHM plots/"
#########################################################################################################
########################################   MAIN PROGRAM        ##########################################
#########################################################################################################

for (j in 3:40){
  file_path_name=paste(file_name,"plant_",toString(j),"a.csv",sep="")
  if(file.exists(file_path_name)){
    output_folder_path=paste(output_folder,"plant",toString(j),"/",sep="")
    dir.create(output_folder_path, showWarnings = FALSE)

    ###fetch data
    data<-read.csv(file_path_name,header=F)
    names(data)=c("component","time","sensor_1","sensor_2","sensor_3","sensor_4", "control_1","control_2", "control_3", "control_4")
    data$time<-strptime(data$time,format="%Y-%m-%d %H:%M:%S")
    data_length<-length(data[,1])
    plot_number<-floor(data_length/1000)


    for (i in 1:plot_number){
      begin_index<-(1+1000*(i-1))
      end_index<-(1000*i)

      p1<-ggplot(data=data[begin_index:end_index,],aes(x=time,y=sensor_1,group=component,col=factor(component)))+geom_line()+theme(legend.position="none")
      p2<-ggplot(data=data[begin_index:end_index,],aes(x=time,y=sensor_2,group=component,col=factor(component)))+geom_line()+theme(legend.position="none")
      p3<-ggplot(data=data[begin_index:end_index,],aes(x=time,y=sensor_3,group=component,col=factor(component)))+geom_line()+theme(legend.position="none")
      p4<-ggplot(data=data[begin_index:end_index,],aes(x=time,y=sensor_4,group=component,col=factor(component)))+geom_line()+theme(legend.position="bottom")

      dirname1<-output_folder_path
      dirname2<-paste("plant",toString(j),sep="")
      dirname3<-"_"
      dirname4<-toString(begin_index)
      dirname5<-"to"
      dirname6<-toString(end_index)
      dirname7<-".jpg"
      dirname=paste(dirname1,dirname2,dirname3,dirname4,dirname5,dirname6,dirname7,sep="")

      grid.arrange(p1,p2,p3,p4,ncol=1, nrow=4, heights=c(1,1,1,1.3), widths=c(4))
      dev.copy(png,file=dirname, width=12, height=10,units="in",res=1000)
      graphics.off()

    }

  }

}