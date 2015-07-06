### created date: July 2 2015
### last modified date:
### author:A30123
### description:PCA plus t squared q


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
#######################################   FUNCTIONS           ###########################################
#########################################################################################################

#########################################################################################################
#######################################   INITIALIZING        ###########################################
#########################################################################################################
my_data_path<-"C:/Users/A30123.ITRI/Documents/R scripts/New for event mining/Try_Handmade_Tsquared_Q/features/output.csv"

#########################################################################################################
#######################################   MAIN PROGRAM        ###########################################
#########################################################################################################
#Read in the data
all_data<-read.csv(my_data_path)

#extract only first 200 entries
my_data<-all_data[1:200,]
my_data_testing<-all_data[201:length(all_data[,1]),]

#mean and standard deviation of normal data
means<-colMeans(my_data)
stds<-apply(my_data,2,sd)

my_data2<-all_data[1:200,1:10]
pairs(my_data2)
#standardize each column
standardized_my_data<-scale(my_data,center=TRUE,scale=TRUE)
standardized_my_data_testing<-scale(my_data_testing,center=means,scale=stds)


coco<-matrix(standardized_my_data,length(my_data[,1]))
#coco<-as.matrix(standardized_my_data)

cov<-t(coco)%*%coco

eigs<-eigen(cov,symmetric=TRUE,only.values=FALSE)
#eigs$vectors
#eigs$values


############### PCA
### watch this:https://www.youtube.com/watch?v=Heh7Nv4qimU

pca_result<-princomp(my_data,scores=TRUE,cor=TRUE)
plot(pca_result)
biplot(pca_result)
pca_result$loadings
pca_result$scores
############## PCA on non-standardized data is exactly the same
pca_result2<-princomp(standardized_my_data,scores=TRUE,cor=TRUE)
plot(pca_result2)


############## calculate T squared


############# calculate Q statistics