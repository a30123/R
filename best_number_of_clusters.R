library(mclust)

path_filename<-"C:/Users/A30123.ITRI/Documents/R scripts/New for event mining/Try_20150605_clustering/data/Feature_BusBarsCooling.temp_mean.csv"
values<-read.csv(path_filename)
d_clust<-Mclust(as.matrix(values),G=1:20)
m.best<-dim(d_clust$z)[2]
cat("model-based optimal number of clusters:",m.best,"\n")
plot(d_clust)
