# Author : A40021 KazeLo
# vent.vac ??? tmal.source å°¾ç«¯??„è·³èµ·é?œè¯??†æ?event
ptm <- proc.time() #ç´€??„ç?‹å?åŸ·è¡Œæ?‚é??
current_path <- "D:/LED/WorkingSpace/TS1/TS1_data/CSV/32.vac_and_tmal.source/"
current_list <- list.files(path=current_path , pattern="*current")

setpoint_path <- "D:/LED/WorkingSpace/TS1/TS1_data/CSV/32.vac_and_tmal.source/"
setpoint_list <- list.files(path=setpoint_path , pattern="*setpoint")


deviation_path <- "D:/LED/WorkingSpace/TS1/TS1_data/CSV/32.vac_and_tmal.source/"
deviation_list <- list.files(path=deviation_path , pattern="*deviation")

dataset <- length(setpoint_list )

event_vac <- c()
event_tmal <- c()
vac_diff<-c()
PhysMax <- 500

for (i in 1:dataset){ #import data
    print(current_list[i]) #?°?‡º?›®??è?•ç?†åˆ°ç¬¬å¹¾?€‹run
    current_data<-read.csv(paste(current_path,current_list [i],sep=""))
    setpoint_data<-read.csv(paste(setpoint_path,setpoint_list [i],sep=""))
    deviation_data<-read.csv(paste(deviation_path,deviation_list [i],sep=""))
    
    data_length<-nrow(setpoint_data) #count data length   
    vac_diff<-c() #clean vac_diff vector
    
    if(data_length>900 & sd(setpoint_data$Vent.vac)!=0){ #if??‚é??>15??†é?˜ä?”vac??‰å?šé?‹é??
        
        for(row_idx in 1:(data_length-1)){ #?…©é»å?æ?›å?Œï?Œç?æ?œå?˜åœ¨vac.diff
            vac_diff_temp <- setpoint_data$Vent.vac[row_idx] - setpoint_data$Vent.vac[(row_idx+1)]
            vac_diff<-c(vac_diff,vac_diff_temp)
        }    
        
        check_point <- which(vac_diff!=0,arr.ind=TRUE) #?‰¾?‡º?›¸æ¸›ä?ç?‰æ–¼0??„indexï¼Œæ?‰å¯?ƒ½?˜¯0->1 or 1->0       
        IDX <- max(check_point) #?ˆ¤?–·tmal.source?œ¨vent.vac ?”¹è®Šé?ç?„error?˜¯?¦>0.2
        error_0 <- abs(deviation_data$TMAl_1.source[IDX]*PhysMax/100) #tmal.source??? current-setpoint
        error_1 <- abs(deviation_data$TMAl_1.source[IDX+1]*PhysMax/100 )
        error_2 <- abs(deviation_data$TMAl_1.source[IDX+2]*PhysMax/100) 
        
        if(max(check_point)>(0.5*data_length)){ #å¦‚æ?œæ”¹è®Šç?„é?åœ¨è³‡æ?™æ?€å¾Œç??50%ç¯„å?å…§ï¼Œå?‡vac event =1          
            event_vac <- c(event_vac,1)
            
            if(error_1 >1 & error_0<0.5){
                event_tmal <- c(event_tmal,1)
            }else{
                event_tmal <- c(event_tmal,0)
            }
            
        }else{ # ??‚é?“æ”¹è®Šé?ä?åœ¨è³‡æ?™å??30%??? no event
            event_vac <- c(event_vac,0)
            event_tmal <- c(event_tmal,0)
        }
        
    }else{ # ??‚é?? < 15??†é?? ??? vent.vacæ²’é?‹é?? = no event
        event_vac <- c(event_vac,0)
        event_tmal <- c(event_tmal,0)
    }
}
write.csv(event_tmal,"event_tmal_50?•ª.csv",row.names=F)
write.csv(event_vac,"event_vac.csv",row.names=F)

proc.time() - ptm
