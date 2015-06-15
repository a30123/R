library(FSelector)
my_data_path<-"C:/Users/A30123.ITRI/Documents/R scripts/New for event mining/Try_20150607_FSelector/correlation/input_713features_15cycles_new.csv"
my_data_path2<-"C:/Users/A30123.ITRI/Documents/R scripts/New for event mining/Try_20150607_FSelector/correlation/Target_dP_Filter_max_15cycles_new.csv"

my_data2<-read.csv(my_data_path)
my_data3<-read.csv(my_data_path2)

my_data<-cbind(my_data2,my_data3)
weights<-linear.correlation(Target~., my_data)
print(weights)
write.csv(weights,"C:/Users/A30123.ITRI/Documents/R scripts/New for event mining/Try_20150607_FSelector/correlation/Correlation result_713features_new.csv",row.names = T)

weights2<-rank.correlation(Target~., my_data)
write.csv(weights2,"C:/Users/A30123.ITRI/Documents/R scripts/New for event mining/Try_20150607_FSelector/correlation/Spearmans Correlation result_713features_new.csv",row.names = T)
