############################################
#DATA#######################################
############################################
#train data
X=array(c(173,155,175,171,166,167,163,155,159,168,166,169,159,154,160,66,49,72,68,63,64,61,52,55,65,61,73,57,49,60),dim=c(15,2))
X
#test data
x_test=array(c(173,54),dim=c(1,2))
x_test
############################################
#PREPROCESS#################################
############################################
#minus mean
X_prime=scale(X,scale=FALSE)
X_prime
#normalized, sample standard deviation
X_prime_prime=scale(X)
X_prime_prime
#normalized, population standard deviation
length=dim(X)[1]
X_population_std=sqrt((length-1)/length)*apply(X, 2, sd, na.rm = TRUE)
X_prime_prime2=scale(X, center = TRUE, scale =X_population_std )
X_prime_prime2
#
x_test_prime2=scale(x_test,center=colMeans(X),scale=X_population_std)
x_test_prime2

#############################################
#eigenvalue method###########################
#############################################
#covariance matrix
B=t(X_prime)%*%X_prime
B
#diagonalization
EIG=eigen(B)
EIG
#loadings
EIG$vectors
#scores
Y=X_prime%*%EIG$vectors
Y


#############################################
#eigenvalue method###########################
#normalized by sample standard deviation#####
#############################################
B_prime=t(X_prime_prime)%*%(X_prime_prime)
B_prime
EIG_prime=eigen(B_prime)
EIG_prime
EIG_prime$vectors[,1]
Y_prime=X_prime_prime%*%EIG_prime$vectors
Y_prime

##############################################
cor(X[,1],Y[,1])
cor(X_prime[,1],Y[,1])


cor(X[,1],Y[,2])
cor(X[,2],Y[,1])
cor(X[,2],Y[,2])
cor(X_prime[,1],Y_prime[,1])
###############################################

#############################################
#princomp method#############################
#############################################
PC_on_X=princomp(X)
PC_on_X
#loadings
PC_on_X$loadings
#scores
PC_on_X$scores



#############################################
#princomp method#############################
#normalized by sample standard deviation#####
#############################################
PC_on_X_prime_prime=princomp(X_prime_prime)
PC_on_X_prime_prime$loadings
PC_on_X_prime_prime$scores


#############################################
#princomp method#############################
#normalized by population standard deviation#
#############################################
PC_on_X_prime_prime2=princomp(X_prime_prime2)
PC_on_X_prime_prime2$loadings
PC_on_X_prime_prime2$scores
#test
tranform_x_test_prime2=x_test_prime2%*%PC_on_X_prime_prime2$loadings
tranform_x_test_prime2



#############################################
#prcomp method###############################
#############################################
PC2_on_X=prcomp(X)
PC2_on_X
#loadings
PC2_on_X$rotation
#scores
PC2_on_X$x


#############################################
#prcomp method###############################
#normalized by sample standard deviation#####
#############################################
PC2_on_X_prime_prime=prcomp(X,scale=T)
PC2_on_X_prime_prime$rotation
PC2_on_X_prime_prime$x

#############################################
#prcomp method###############################
#normalized by population standard deviation#
#############################################
PC2_on_X_prime_prime2=prcomp(X,scale=X_population_std)
PC2_on_X_prime_prime2$rotation
PC2_on_X_prime_prime2$x
#test
tranform_PC2_x_test_prime2=x_test_prime2%*%PC2_on_X_prime_prime2$rotation
tranform_PC2_x_test_prime2


#############################################
#R ADE4 package##############################
#dudi.pca method#############################
#############################################
#install.packages("ade4")
library(ade4)
PC3_on_X=dudi.pca(X,scale=F,scannf=F)
PC3_on_X
#loadings
PC3_on_X$c1
#scores
PC3_on_X$li

PC3_on_X$eig
#############################################
#R ADE4 package##############################
#dudi.pca method#############################
#normalized by sample standard deviation#####
#############################################
PC3_on_X_prime_prime=dudi.pca(X_prime_prime,scale=F,scannf=F)
PC3_on_X_prime_prime$c1
PC3_on_X_prime_prime$li


#############################################
#R ADE4 package##############################
#dudi.pca method#############################
#normalized by population standard deviation#
#############################################
PC3_on_X_prime_prime2=dudi.pca(X,scannf=F)
PC3_on_X_prime_prime2$c1
PC3_on_X_prime_prime2$li

#############################################
#R FactoMineR package########################
#PCA method##################################
#normalized by sample standard deviation#####
#############################################
#install.packages("FactoMineR")
library(FactoMineR)
PC4_on_X=PCA(X,scale=F,graph=F)
PC4_on_X
#loadings
PC4_on_X$svd$V
#scores
PC4_on_X$ind$coord
#############################################
#R FactoMineR package########################
#PCA method##################################
#normalized by sample standard deviation#####
#############################################
PC4_on_X_prime_prime=PCA(X_prime_prime,scale=F,graph=F)
PC4_on_X_prime_prime$svd$V
PC4_on_X_prime_prime$ind$coord
#############################################
#R FactoMineR package########################
#PCA method##################################
#normalized by population standard deviation#
#############################################
PC4_on_X_prime_prime2=PCA(X,graph=F)
PC4_on_X_prime_prime2$svd$V
PC4_on_X_prime_prime2$ind$coord


#############################################
#R amap package##############################
#acp method##################################
#normalized by sample standard deviation#####
#############################################
#install.packages("amap")
library(amap)
PC5_on_X=acp(X,reduce=F)
PC5_on_X
#loadings
PC5_on_X$loadings
#scores
PC5_on_X$scores
#############################################
#R amap package##############################
#acp method##################################
#normalized by sample standard deviation#####
#############################################
PC5_on_X_prime_prime=acp(X)
PC5_on_X_prime_prime$loadings
PC5_on_X_prime_prime$scores
#############################################
#R amap package##############################
#acp method##################################
#normalized by population standard deviation#
#############################################


PC5_on_X_prime_prime2=acp(X_prime_prime2,reduce=F)
PC5_on_X_prime_prime2$loadings
PC5_on_X_prime_prime2$scores

