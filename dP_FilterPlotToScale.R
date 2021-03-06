### created date:6/01/2015
### last modified date:
### author:A30123
### description:Plot Current,dP_Filter to scale


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

#########################################################################################################
#######################################   FUNCTIONS           ###########################################
#########################################################################################################


#########################################################################################################
#######################################   INITIALIZING        ###########################################
#########################################################################################################
setpoint_folder_path="E:./././././MovedFromD/CSV/TS1/dP_Filter_2492runs/setpoint/"
current_folder_path="E:./././././MovedFromD/CSV/TS1/dP_Filter_2492runs/"
output_folder_path="E:/MovedFromD/PLOT/TS1/dP_Filter_2492runs_toscale/"
sensor_variable="dP_Filter"
upper_plot_limit=70
lower_plot_limit=-10
plotlength<-75000  
  
#########################################################################################################
#######################################   MAIN PROGRAM        ###########################################
#########################################################################################################

filenames<-list.files(path=current_folder_path)
filenames<-filenames[grepl("current.csv", filenames[])] 
no_of_runs<-length(filenames)


i<-0
j<-1
previousremainlist<-c()
previousremainlist2<-c()
previousremainlist3<-c()
previousground<-c()
previousground2<-c()
prevlistx<-c()
prevlisty<-c()
prevlistx2<-c()
prevlisty2<-c()
prevlistx3<-c()
prevlisty3<-c()


while(i<no_of_runs){
  listx<-prevlistx
  listy<-prevlisty
  listx2<-prevlistx2
  listy2<-prevlisty2
  listx3<-prevlistx3
  listy3<-prevlisty3
  
  
  longlist<-previousremainlist
  #longlist2<-previousremainlist2
  #longlist3<-previousremainlist3
  l1<-length(previousremainlist)
  
  #groundtruth<-c(previousground,rep(-10,(2*plotlength-l1)))
  groundtruth2<-previousground2
  
  starting<-i
  while (l1 <=plotlength && i<no_of_runs){
    i<-i+1
    single_current_file_path<-paste(current_folder_path,filenames[i],sep="")
    single_setpoint_file_path<-paste(setpoint_folder_path,gsub("current","setpoint",filenames[i]),sep="")
    
    templist<-unlist(read.csv(single_current_file_path)[sensor_variable])
    longlist<-c(longlist,(templist))
    
    #templist2<-unlist(read.csv(single_setpoint_file_path)[sensor_variable])
    #longlist2<-c(longlist2,(templist2))
    
    #longlist3<-c(longlist3,abs(templist-templist2))
    
    l2<-l1
    l1<-length(longlist)
   # groundtruth[l1]<-600
    groundtruth2<-c(groundtruth2,l1)

    
    ##########
    if(grepl("HCl_Bake_HCl_Bake", filenames[i])){
      listx<-c(listx,l2,l2,l1,l1)
      listy<-c(listy,lower_plot_limit,upper_plot_limit,upper_plot_limit,lower_plot_limit)
      listx3<-c(listx3,l2,l2,l1,l1)
      listy3<-c(listy3,rep(lower_plot_limit,4))
      listx2<-c(listx2,l2,l2,l1,l1)
      listy2<-c(listy2,rep(lower_plot_limit,4))
    }else{
      if(grepl("Buffer", filenames[i])){
        listx2<-c(listx2,l2,l2,l1,l1)
        listy2<-c(listy2,lower_plot_limit,upper_plot_limit,upper_plot_limit,lower_plot_limit)
        listx3<-c(listx3,l2,l2,l1,l1)
        listy3<-c(listy3,rep(lower_plot_limit,4))
        listx<-c(listx,l2,l2,l1,l1)
        listy<-c(listy,rep(lower_plot_limit,4))
      }else{
        if(grepl("Bake", filenames[i])){
          listx3<-c(listx3,l2,l2,l1,l1)
          listy3<-c(listy3,lower_plot_limit,upper_plot_limit,upper_plot_limit,lower_plot_limit)
          listx2<-c(listx2,l2,l2,l1,l1)
          listy2<-c(listy2,rep(lower_plot_limit,4))
          listx<-c(listx,l2,l2,l1,l1)
          listy<-c(listy,rep(lower_plot_limit,4))
        }else{
          listx3<-c(listx3,l2,l2,l1,l1)
          listy3<-c(listy3,rep(lower_plot_limit,4))
          listx2<-c(listx2,l2,l2,l1,l1)
          listy2<-c(listy2,rep(lower_plot_limit,4))
          listx<-c(listx,l2,l2,l1,l1)
          listy<-c(listy,rep(lower_plot_limit,4))
        }
      }
    }
    ######
    
    
  }
  ending<-i
  dirname1<-output_folder_path
  dirname2<-toString(j)
  dirname3<-"_"
  dirname4<-toString(starting)
  dirname5<-"to"
  dirname6<-toString(ending)
  dirname7<-".png"
  dirname=paste(dirname1,dirname2,dirname3,dirname4,dirname5,dirname6,dirname7,sep="")
  jpeg(dirname,height=600,width=1800)
  
  #par(mar=c(5,5,5,5))
  #ylim<-range(c(longlist,groundtruth[1:plotlength]))
  plot(longlist[1:plotlength],type="l",lwd=3,col="forestgreen",ylim=c(lower_plot_limit,upper_plot_limit))
  polygon(c(listx[1:(length(listx)-2)],plotlength,plotlength),listy,col="pink")  
  polygon(c(listx2[1:(length(listx2)-2)],plotlength,plotlength),listy2,col="skyblue")  
  polygon(c(listx3[1:(length(listx3)-2)],plotlength,plotlength),listy3,col="mistyrose") 
  
  
  #lines(longlist2[1:plotlength],type="l",lwd=6,col="red")#setpoint
  lines(longlist[1:plotlength],lwd=3,col="forestgreen")#current
  #lines(groundtruth[1:plotlength],lty="dashed",col="gray")
  abline(v=groundtruth2[groundtruth2<=plotlength],col="gray60")
  
  
  #par(new=T)
  #plot(longlist3[1:plotlength],axes=FALSE,type="l",lwd=2,col="blue",ylim=c(0,300))
  #axis(side=4)
  #legend('top', c('current value','setpoint value','error'),lty=1, col=c('forestgreen','red','blue'),bty='n',lwd=2,cex=2)
  
  dev.off()
  
  previousremainlist<-longlist[(plotlength+1):length(longlist)]
  #previousremainlist2<-longlist2[(plotlength+1):length(longlist)]
  #previousremainlist3<-longlist3[(plotlength+1):length(longlist)]
  #previousground<-groundtruth[(plotlength+1):length(longlist)]
  if (sum(groundtruth2>plotlength)!=0){
    previousground2<-groundtruth2[groundtruth2>plotlength]-plotlength}
  else{
    previousground2<-c()
  }

  prevlistx<-c(0,0,(listx[(length(listx))]-(plotlength)),(listx[length(listx)]-(plotlength)))
  prevlisty<-c(-10,rep(listy[(length(listx)-1)],2),-10)
  prevlistx2<-c(0,0,(listx2[(length(listx2))]-(plotlength)),(listx2[length(listx2)]-(plotlength)))
  prevlisty2<-c(-10,rep(listy2[(length(listx2)-1)],2),-10)
  prevlistx3<-c(0,0,(listx3[(length(listx3))]-(plotlength)),(listx3[length(listx3)]-(plotlength)))
  prevlisty3<-c(-10,rep(listy3[(length(listx3)-1)],2),-10)
  
  
  
  j<-j+1
}