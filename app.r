#Set working directory to file location
setwd("C:\\Users\\Srinandh\\Downloads")
#Open the required file using "read.csv"
USArrests=read.csv("USArrests.csv",row.names=1)
#view details of the arrest numbers
View(USArrests)
#Structure of every crime
str(USArrests)
#Summary of crime statistics
summary(USArrests)
#Plotting graph
plot(USArrests)
#Plotting graph between different crimes
plot(Murder~Assault,USArrests)
plot(Murder~UrbanPop,USArrests)
plot(Murder~Rape,USArrests)
plot(Assault~UrbanPop,USArrests)
plot(Assault~Rape,USArrests)
plot(UrbanPop~Rape,USArrests)
#To find the optimum number of k value
k.max<-10
wss<-rep(NA,k.max)
nClust<-list()
for(i in 1:k.max){
  ArrestCluster<-kmeans(USArrests,i)
  wss[i]<-ArrestCluster$tot.withinss
  nClust[[i]]<-USArrests$size
}
#Plot the results of the k value optimization into a graph
plot(1:k.max,wss,type="b",pch=18,xlab="Number of clusters K",ylab="Total within-clusters sum of squares:Trips")
y_kmeans<-USArrests$cluster
clusplot
#Using the graph find the optimal k value
#cluster all data into required number of clusters
set.seed(123)
ArrestCluster=kmeans(USArrests,centers=3,nstart = 10) #Here the optimum k value is 3
ArrestCluster
#Plotting the silhouette of every cluster
library(cluster)
library(factoextra)
sil<-silhouette(ArrestCluster$cluster,dist(USArrests))
fviz_silhouette(sil)
