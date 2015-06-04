#setwd("D:/20150602")

#Fisher's criterion function 

Fisher_cri = function(y,feature){
  
  x_1 = feature[ y== unique(y)[1]]
  x_2 = feature[ y== unique(y)[2]]
  
  j = (mean(x_1)-mean(x_2))^2 / (var(x_1)+var(x_2))
  
  return(j)
  
}


#test

y = sample(x = c(0,1),size = 100,replace = T)
feature = rnorm(100)

Fisher_cri(y,feature)

########################
path1<-"C:/Users/A30123.ITRI/Documents/R scripts/New for event mining/Try_20150604_Fishers_criterion/Faulty"
path2<-"C:/Users/A30123.ITRI/Documents/R scripts/New for event mining/Try_20150604_Fishers_criterion/Normal"

filename1<-list.files(path=path1)[1]
filename2<-list.files(path=path2)[1]

dat_y0<-read.csv(paste(path1,"/",filename1,sep=""))
dat_y1<-read.csv(paste(path2,"/",filename2,sep=""))
#colnames(dat_y0)==colnames(dat_y1)


y = c(rep(0,dim(dat_y0)[1]) , rep(1,dim(dat_y1)[1]))

fc = NA

for( i in 1: dim(dat_y0)[2]){
  
  x = c( (dat_y0[,i]) , (dat_y1[,i]) )
  fc[i] = Fisher_cri(y,x)
  
  }

output = data.frame( feature=colnames(dat_y0) ,Fisher_cri=fc )


write.csv(output,"C:/Users/A30123.ITRI/Documents/R scripts/New for event mining/Try_20150604_Fishers_criterion/Fisher's criterion result.csv",row.names = F)
