#### created date:7/5/2015
#### last modified date:7/5/2015
#### author:A30123
#### description:Plot Current, Setpoint, |Current-Setpoint|, deviation*physmax/100 values of TMAl_1.source to scale


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
########################################   FUNCTIONS          ###########################################
#########################################################################################################


#########################################################################################################
#######################################   INITIALIZING        ###########################################
#########################################################################################################
setpoint_folder_path="E:./././././././MovedFromD/CSV/TS1/MFCsource_2492runs/setpoint/"
current_folder_path="E:./././././././MovedFromD/CSV/TS1/MFCsource_2492runs/current/"
deviation_folder_path="E:./././././MovedFromD/CSV/TS1/MFCsource_2492runs/deviation/"
output_folder_path="C:././././R scripts/New for event mining/Try_20150705_TS1_TMAl_source_real_deviation/Output_New/"
sensor_variable="TMAl_1.source"
upper_plot_limit=700
lower_plot_limit=-10
plotlength<-75000  
physmax<-500
#########################################################################################################
########################################   MAIN PROGRAM        ##########################################
#########################################################################################################
###start timer
ptm<-proc.time()

###get list of files in current folder
filenames<-list.files(path=current_folder_path)
filenames<-filenames[grepl("current.csv", filenames[])] 

no_of_runs<-length(filenames)

i<-0
j<-1

previousremainlist<-c()
previousremainlist2<-c()
previousremainlist3<-c()
previousremainlist4<-c()
previousground<-c()
previousground2<-c()
prevBakelistleft<-c()
prevBakelistright<-c()
prevHClBakelistleft<-c()
prevHClBakelistright<-c()
prevBufferlistleft<-c()
prevBufferlistright<-c()

while(i<no_of_runs){  
  longlist<-previousremainlist
  longlist2<-previousremainlist2
  longlist3<-previousremainlist3
  longlist4<-previousremainlist4
  l1<-length(previousremainlist)
  
  groundtruth2<-previousground2
  Bakelistleft<-prevBakelistleft
  Bakelistright<-prevBakelistright
  HClBakelistleft<-prevHClBakelistleft
  HClBakelistright<-prevHClBakelistright
  Bufferlistleft<-prevBufferlistleft
  Bufferlistright<-prevBufferlistright
  
  starting<-i
  while (l1 <=plotlength && i<no_of_runs){
    i<-i+1
    single_current_file_path<-paste(current_folder_path,filenames[i],sep="")
    single_setpoint_file_path<-paste(setpoint_folder_path,gsub("current","setpoint",filenames[i]),sep="")
    single_deviation_file_path<-paste(deviation_folder_path,gsub("current","deviation",filenames[i]),sep="")
    
    templist<-unlist(read.csv(single_current_file_path)[sensor_variable])
    longlist<-c(longlist,(templist))
    
    templist2<-unlist(read.csv(single_setpoint_file_path)[sensor_variable])
    longlist2<-c(longlist2,(templist2))
    
    longlist3<-c(longlist3,abs(templist-templist2))
    
    templist4<-unlist(read.csv(single_deviation_file_path)[sensor_variable])
    longlist4<-c(longlist4,(abs(templist4)*(physmax/100)))
    
    l2<-l1
    l1<-length(longlist)
    
    #indices for end of runs
    groundtruth2<-c(groundtruth2,l1)
    
    ########## collect starting and ending indices for bake, HCl-bake and buffer runs
    if(grepl("HCl_Bake_HCl_Bake", filenames[i])){
      HClBakelistleft<-c(HClBakelistleft,l2)
      HClBakelistright<-c(HClBakelistright,l1)
    }else{
      if(grepl("Buffer", filenames[i])){
        Bufferlistleft<-c(Bufferlistleft,l2)
        Bufferlistright<-c(Bufferlistright,l1)
      }else{
        if(grepl("Bake", filenames[i])){
          Bakelistleft<-c(Bakelistleft,l2)
          Bakelistright<-c(Bakelistright,l1)
        }
      }
    }
    ##########
  }

  if (sum(groundtruth2>plotlength)!=0){
    previousground2<-groundtruth2[groundtruth2>plotlength]-plotlength}
  else{
    previousground2<-c()
  }

  if (sum(Bakelistright>plotlength)!=0){
    prevBakelistleft<-c(0)
    prevBakelistright<-(Bakelistright[Bakelistright>plotlength]-plotlength)
    Bakelistright[length(Bakelistright)]<-plotlength}
  else{
    prevBakelistleft<-c()
    prevBakelistright<-c()
  }

  if (sum(HClBakelistright>plotlength)!=0){
    prevHClBakelistleft<-c(0)
    prevHClBakelistright<-(HClBakelistright[HClBakelistright>plotlength]-plotlength)
    HClBakelistright[length(HClBakelistright)]<-plotlength}
  else{
    prevHClBakelistleft<-c()
    prevHClBakelistright<-c()
  }

  if (sum(Bufferlistright>plotlength)!=0){
    prevBufferlistleft<-c(0)
    prevBufferlistright<-(Bufferlistright[Bufferlistright>plotlength]-plotlength)
    Bufferlistright[length(Bufferlistright)]<-plotlength}
  else{
    prevBufferlistleft<-c()
    prevBufferlistright<-c()
  }

  #png filename
  ending<-i
  dirname1<-output_folder_path
  dirname2<-toString(j)
  dirname3<-"_"
  dirname4<-toString(starting)
  dirname5<-"to"
  dirname6<-toString(ending)
  dirname7<-".png"
  dirname=paste(dirname1,dirname2,dirname3,dirname4,dirname5,dirname6,dirname7,sep="")
  jpeg(dirname,height=800,width=2000)
  
  #first part of plot (current,setpoint values, buffer, bake, HCl-bake runs)
  par(mar=c(5,5,5,5))
  plot(longlist2[1:plotlength],cex.lab=2,xlab="runs",ylab="sccm",type="l",lwd=5,col="red",ylim=c(lower_plot_limit,upper_plot_limit))
  title(main="Comparison of error and error reconstructed from deviation")
 
  if(length(Bakelistleft)>0){
    rect(Bakelistleft,rep(lower_plot_limit,length(Bakelistleft)),Bakelistright,rep(upper_plot_limit,length(Bakelistleft)),col="mistyrose",border=NA)
  }

  if(length(HClBakelistleft)>0){
    rect(HClBakelistleft,rep(lower_plot_limit,length(HClBakelistleft)),HClBakelistright,rep(upper_plot_limit,length(HClBakelistleft)),col="pink",border=NA)
  }

  if(length(Bufferlistleft)>0){
    rect(Bufferlistleft,rep(lower_plot_limit,length(Bufferlistleft)),Bufferlistright,rep(upper_plot_limit,length(Bufferlistleft)),col="skyblue",border=NA)
  }

  lines(longlist2[1:plotlength],type="l",lwd=6,col="red")#setpoint
  points(longlist[1:plotlength],pch=20,col="forestgreen")#current
  abline(v=groundtruth2[groundtruth2<=plotlength],col="gray60")#run breakpoints


  #second part of plot
  par(new=T)
  plot(longlist3[1:plotlength],axes=FALSE,xlab="",ylab="",type="l",lwd=3,col="blue",ylim=c(0,100))#error
  lines(longlist4[1:plotlength],type="l",lwd=2,col="brown1")# scaled deviation
  axis(side=4)
  mtext(side=4,line=3,"Error scale (sccm)",cex=2)
  legend('top', c('current value (left axis)','setpoint value (left axis)','error (right axis)','deviation*physmax/100 (right axis)'),lty=1, col=c('forestgreen','red','blue','brown1'),bty='n',lwd=2,cex=2)
  
  dev.off()
  
  previousremainlist<-longlist[(plotlength+1):length(longlist)]
  previousremainlist2<-longlist2[(plotlength+1):length(longlist)]
  previousremainlist3<-longlist3[(plotlength+1):length(longlist)]
  previousremainlist4<-longlist4[(plotlength+1):length(longlist)]  
  
  j<-j+1
}
proc.time()-ptm