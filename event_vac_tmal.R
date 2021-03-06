# Author : A40021 KazeLo
# vent.vac ??? tmal.source 尾端??�跳起�?�聯??��?�event
ptm <- proc.time() #紀??��?��?�執行�?��??
current_path <- "E:/MovedFromD/CSV/TS1/VentVac_TMAl_2492runs/current/"
current_list <- list.files(path=current_path , pattern="*current")

setpoint_path <- "E:/MovedFromD/CSV/TS1/VentVac_TMAl_2492runs/setpoint/"
setpoint_list <- list.files(path=setpoint_path , pattern="*setpoint")


deviation_path <- "E:/MovedFromD/CSV/TS1/VentVac_TMAl_2492runs/deviation/"
deviation_list <- list.files(path=deviation_path , pattern="*deviation")

dataset <- length(setpoint_list )

event_vac <- c()
event_tmal <- c()
vac_diff<-c()
PhysMax <- 500

for (i in 1:dataset){ #import data
    print(current_list[i]) #?��?��?��??��?��?�到第幾?��run
    current_data<-read.csv(paste(current_path,current_list [i],sep=""))
    setpoint_data<-read.csv(paste(setpoint_path,setpoint_list [i],sep=""))
    deviation_data<-read.csv(paste(deviation_path,deviation_list [i],sep=""))
    
    data_length<-nrow(setpoint_data) #count data length   
    vac_diff<-c() #clean vac_diff vector
    
    if(data_length>900 & sd(setpoint_data$Vent.vac)!=0){ #if??��??>15??��?��?�vac??��?��?��??
        
        for(row_idx in 1:(data_length-1)){ #?��點�?��?��?��?��?��?��?�在vac.diff
            vac_diff_temp <- setpoint_data$Vent.vac[row_idx] - setpoint_data$Vent.vac[(row_idx+1)]
            vac_diff<-c(vac_diff,vac_diff_temp)
        }    
        
        check_point <- which(vac_diff!=0,arr.ind=TRUE) #?��?��?��減�?��?�於0??�index，�?�可?��?��0->1 or 1->0       
        IDX <- max(check_point) #?��?��tmal.source?��vent.vac ?��變�?��?�error?��?��>0.2
        error_0 <- abs(deviation_data$TMAl_1.source[IDX]*PhysMax/100) #tmal.source??? current-setpoint
        error_1 <- abs(deviation_data$TMAl_1.source[IDX+1]*PhysMax/100 )
        error_2 <- abs(deviation_data$TMAl_1.source[IDX+2]*PhysMax/100) 
        
        if(max(check_point)>(0.5*data_length)){ #如�?�改變�?��?�在資�?��?�後�??50%範�?�內，�?�vac event =1          
            event_vac <- c(event_vac,1)
            
            if(error_1 >0.5 & error_0<0.25){
                event_tmal <- c(event_tmal,1)
            }else{
                event_tmal <- c(event_tmal,0)
            }
            
        }else{ # ??��?�改變�?��?�在資�?��??30%??? no event
            event_vac <- c(event_vac,0)
            event_tmal <- c(event_tmal,0)
        }
        
    }else{ # ??��?? < 15??��?? ??? vent.vac沒�?��?? = no event
        event_vac <- c(event_vac,0)
        event_tmal <- c(event_tmal,0)
    }
}
write.csv(event_tmal,"C:/Users/A30123.ITRI/Documents/R scripts/New for event mining/Try_20150423_PYLo_VentVacTMAlsource_event/event_tmal_50_pointfive_threshold.csv",row.names=F)
write.csv(event_vac,"C:/Users/A30123.ITRI/Documents/R scripts/New for event mining/Try_20150423_PYLo_VentVacTMAlsource_event/event_vac_pointfivethreshold.csv",row.names=F)

proc.time() - ptm
