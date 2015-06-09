data(iris)
result<-cfs(Species~., iris)
f<-as.simple.formula(result,"Species")

as.simple.formula(c("mary","david"),"jack")
 
#########################################################################################
library(rpart)

evaluator<-function(subset){
  #k-fold cross validation
  k<-5
  splits<-runif(nrow(iris))
  results=sapply(1:k, function(i){
    test.idx<-(splits>=(i-1)/k)&(splits<i/k)
    train.idx<-!test.idx
    test<-iris[test.idx, ,drop=FALSE]
    train<-iris[train.idx, ,drop=FALSE]
    tree<-rpart(as.simple.formula(subset,"Species"),train)
    error.rate=sum(test$Species != predict(tree,test, type="c"))/nrow(test)
    return(1-error.rate)
  })
  print(subset)
  print(mean(results))
  return(mean(results))
  
}

subset<-best.first.search(names(iris)[-5],evaluator)
f<-as.simple.formula(subset,"Species")
print(f)

#################################################################################
my_data_path<-"C:/Users/A30123.ITRI/Documents/R scripts/New for event mining/Try_20150607_FSelector/cfs/InputVSTarget2.csv"
my_data<-read.csv(my_data_path)
subset2<-cfs(Target~., my_data)
f2<-as.simple.formula(subset2,"Target")
print(f2)



################################################################################

library(mlbench)
data(HouseVotes84)

weights<-chi.squared(Class~.,HouseVotes84)
print(weights)
subset<-cutoff.k(weights, 5)
f<-as.simple.formula(subset,"Class")


#####information gain##################################################################
library(FSelector)
#my_data_path<-"C:/Users/Mary/Music/Documents/R/New for Event Mining/Try_20150608_FSelector/cfs/InputVSTarget2.csv"
#my_data_path<-"C:/Users/A30123.ITRI/Documents/R scripts/New for event mining/Try_20150607_FSelector/information gain/toptwelve_faulty_15cycles.csv"
#my_data_path2<-"C:/Users/A30123.ITRI/Documents/R scripts/New for event mining/Try_20150607_FSelector/information gain/toptwelve_normal_15cycles.csv"
my_data_path<-"C:/Users/A30123.ITRI/Documents/R scripts/New for event mining/Try_20150607_FSelector/information gain/faulty_features_15cycles.csv"
my_data_path2<-"C:/Users/A30123.ITRI/Documents/R scripts/New for event mining/Try_20150607_FSelector/information gain/normal_features_15cycles.csv"

my_data2<-read.csv(my_data_path)
my_data3<-read.csv(my_data_path2)

my_data<-rbind(my_data2,my_data3)
weights<-information.gain(Target~., my_data)
print(weights)
write.csv(weights,"C:/Users/A30123.ITRI/Documents/R scripts/New for event mining/Try_20150607_FSelector/information gain/Information Gain criterion result.csv",row.names = T)

####relief###################
weights<-relief(Target~., my_data, neighbours.count=5, sample.size=20)
print(weights)

