##################
#DATA#############
##################
X=array(c(173,155,175,171,166,167,163,155,159,168,166,169,159,154,160,66,49,72,68,63,64,61,52,55,65,61,73,57,49,60),dim=c(15,2))
X
X_prime=scale(X,scale=FALSE)
X_prime

##################
#eigenvalue method
##################

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

X_prime_prime=scale(X)
X_prime_prime
B_prime=t(X_prime_prime)%*%(X_prime_prime)
B_prime
EIG_prime=eigen(B_prime)
EIG_prime
EIG_prime$vectors[,1]
Y_prime=X_prime_prime%*%EIG_prime$vectors
Y_prime


cor(X[,1],Y[,1])
cor(X_prime[,1],Y[,1])


cor(X[,1],Y[,2])
cor(X[,2],Y[,1])
cor(X[,2],Y[,2])
cor(X_prime[,1],Y_prime[,1])



PC_on_X=princomp(X)
PC_on_X
PC_on_X$loadings
EIG


PC_on_X$scores
Y


PC_on_X_prime_prime=princomp(X_prime_prime)
PC_on_X_prime_prime$scores
PC_on_X_prime_prime$loadings


###################population standard deviation
length=dim(X)[1]
X_population_std=sqrt((length-1)/length)*apply(X, 2, sd, na.rm = TRUE)
X_prime_prime2=scale(X, center = TRUE, scale =X_population_std )
X_prime_prime2

PC_on_X_prime_prime2=princomp(X_prime_prime2)
PC_on_X_prime_prime2$scores
PC_on_X_prime_prime2$loadings


################test
x_test=array(c(173,54),dim=c(1,2))
x_test_prime2=scale(x_test,center=colMeans(X),scale=X_population_std)
tranform_x_test_prime2=x_test_prime2%*%PC_on_X_prime_prime2$loadings
