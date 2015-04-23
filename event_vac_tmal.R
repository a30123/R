# Author : A40021 KazeLo
# vent.vac 與 tmal.source 尾端的跳起關聯分析event
ptm <- proc.time() #紀錄程式執行時間
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
    print(current_list[i]) #印出目前處理到第幾個run
    current_data<-read.csv(paste(current_path,current_list [i],sep=""))
    setpoint_data<-read.csv(paste(setpoint_path,setpoint_list [i],sep=""))
    deviation_data<-read.csv(paste(deviation_path,deviation_list [i],sep=""))
    
    data_length<-nrow(setpoint_data) #count data length   
    vac_diff<-c() #clean vac_diff vector
    
    if(data_length>900 & sd(setpoint_data$Vent.vac)!=0){ #if時間>15分鐘且vac有做開關
        
        for(row_idx in 1:(data_length-1)){ #兩點前減後，結果存在vac.diff
            vac_diff_temp <- setpoint_data$Vent.vac[row_idx] - setpoint_data$Vent.vac[(row_idx+1)]
            vac_diff<-c(vac_diff,vac_diff_temp)
        }    
        
        check_point <- which(vac_diff!=0,arr.ind=TRUE) #找出相減不等於0的index，有可能是0->1 or 1->0       
        IDX <- max(check_point) #判斷tmal.source在vent.vac 改變點的error是否>0.2
        error_0 <- abs(deviation_data$TMAl_1.source[IDX]*PhysMax/100) #tmal.source的 current-setpoint
        error_1 <- abs(deviation_data$TMAl_1.source[IDX+1]*PhysMax/100 )
        error_2 <- abs(deviation_data$TMAl_1.source[IDX+2]*PhysMax/100) 
        
        if(max(check_point)>(0.5*data_length)){ #如果改變的點在資料最後的50%範圍內，則vac event =1          
            event_vac <- c(event_vac,1)
            
            if(error_1 >2 & error_0<0.5){
                event_tmal <- c(event_tmal,1)
            }else{
                event_tmal <- c(event_tmal,0)
            }
            
        }else{ # 時間改變點不在資料後30%則 no event
            event_vac <- c(event_vac,0)
            event_tmal <- c(event_tmal,0)
        }
        
    }else{ # 時間 < 15分鐘 或 vent.vac沒開關 = no event
        event_vac <- c(event_vac,0)
        event_tmal <- c(event_tmal,0)
    }
}
write.csv(event_tmal,"event_tmal_50啪.csv",row.names=F)
write.csv(event_vac,"event_vac.csv",row.names=F)

proc.time() - ptm
