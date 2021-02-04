standardize=function(x){
  return((x-mean(x))/sd(x))}
Data=ForestTypes[,2:14]
View(Data)
SDATA=as.data.frame(lapply(Data[,1:12],standardize))  


res.pca <- prcomp(DATA, scale = TRUE)
#eigen values
eig.val <- get_eigenvalue(res.pca)
#Plot of Eigenvalues Lr versus r
fviz_eig(res.pca, choice = "eigenvalue", addlabels=TRUE,ylim = c(0, 5),ncp=35)
#Percentage of Variance explained versus r
fviz_eig(res.pca, ncp = 23,addlabels=TRUE,ylim = c(0, 35))


newdata = res.pca$x[,1:3]






#PCA
res.pca <- prcomp(SDATA, scale = FALSE)

?prcomp

#Eigenvector w1,w2,w3
eigenvec3 <- res.pca$rotation[,1:3]
library(factoextra)
library("FactoMineR")


#eigen values
eig.val <- get_eigenvalue(res.pca)
#Plot of Eigenvalues Lr versus r
fviz_eig(res.pca, choice = "eigenvalue", addlabels=TRUE,ylim = c(0, 5),ncp=35)
#Percentage of Variance explained versus r
fviz_eig(res.pca, ncp = 23,addlabels=TRUE,ylim = c(0, 35))

pca_vals=res.pca$x[,1:3]

newdata1 <- cbind(ForestTypes[,14],newdata)

newdata.df <- as.data.frame(newdata1)
#3 colors representing 3 fonts
treat_col <- c()
for (i in 1:15120) {
  if(newdata.df$Cover_Type[i] == '1') {
    treat_col <- append(treat_col, 'red')
  }
  else if(newdata.df$Cover_Type[i] == '2'){
    treat_col <- append(treat_col, 'blue') }
  else if(newdata.df$Cover_Type[i] == '3') {
    treat_col <- append(treat_col, 'yellow')
  }
  else if(newdata.df$Cover_Type[i] == '4'){
    treat_col <- append(treat_col, 'green') }
  else if(newdata.df$Cover_Type[i] == '5') {
    treat_col <- append(treat_col, 'cyan')
  }
  else if(newdata.df$Cover_Type[i] == '6'){
    treat_col <- append(treat_col, 'purple') }
  else if(newdata.df$Cover_Type[i] == '7') {
    treat_col <- append(treat_col, 'orange')
  }
}
#PCA plot PC1 vs PC2
plot(newdata.df$PC1, newdata.df$PC2, col=treat_col,
     pch=16, xlab="Principal Component 1 (29.7%)",ylab="Principal Component 2 (21.7%)",main="Score Plot")
par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
legend("bottomright",inset=c(-0.2,0), legend=c("1", "2","3","4",'5','6','7'),title="Class",
       fill=c("red","blue","yellow",'green','cyan','purple','orange'))

#PCA plot PC1 vs PC3
plot(newdata.df$PC1, newdata.df$PC3, col=treat_col,
     pch=16, xlab="Principal Component 1 (29.7%)",ylab="Principal Component 3 (15.4%)",main="Score Plot")
par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
legend("bottomright",inset=c(-0.2,0), legend=c("1", "2","3","4",'5','6','7'),title="Class",
       fill=c("red","blue","yellow",'green','cyan','purple','orange'))

#PCA plot PC2 vs PC3
plot(newdata.df$PC2, newdata.df$PC3, col=treat_col,
     pch=16, xlab="Principal Component 2 (21.7%)",ylab="Principal Component 3 (15.4%)",main="Score Plot")
par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
legend("bottomright",inset=c(-0.2,0), legend=c("1", "2","3","4",'5','6','7'),title="Class",
       fill=c("red","blue","yellow",'green','cyan','purple','orange'))

#k means clustering with k=1 to 10
sm= c()
s1=c()
for (i in 1:10){
  km.out=kmeans(SDATA,i, nstart=50)
  sm = c(sm,sum(km.out$tot.withinss)) #Total within-cluster sum of squares
  s1 = c(s1,km.out$totss) #the total variance in the data
  qm = sm/s1
  perfk1 = 1-qm
}
qm = sm/s1
perfk1 = 1-qm
par(mar = c(5, 4, 4, 2) + 0.1)
plot(1:10,perfk1,xlab="K",ylab="Reduction of Variance Perf(k)",main="K Means Clustering: k Performance",type='o',pch=19,col='red')

wssplot(SDATA)


#k means clustering with k=1 to 10
sm= c()
s1=c()
for (i in 1:10){
  km.out=kmeans(SDATA,i, nstart=50)
  sm = c(sm,sum(km.out$tot.withinss)) #Total within-cluster sum of squares
  s1 = c(s1,km.out$totss) #the total variance in the data
  qm = sm/s1
  perfk1 = 1-qm
}
qm = sm/s1
perfk1 = 1-qm
par(mar = c(5, 4, 4, 2) + 0.1)
plot(1:10,sm,xlab="K",ylab="Total WSS",main="K Means Clustering: k Performance",type='o',pch=19,col='red')

#k means with 3 clusters
km.out=kmeans(SDATA,3, nstart=50)
clusters = km.out$cluster

#Scalar product of w1,w2,w3 and the centers
centers = km.out$centers
ck = centers%*%eigenvec3

#plot of centers
#install.packages("scatterplot3d")
library("scatterplot3d")
colors <- c("red", "yellow", "green")
colors <- colors[as.numeric(ck)]
scatterplot3d(ck,color=colors,main="Cluster Centers",pch = 16)
legend("topright",legend=c("Cluster 1","Cluster 2","Cluster 3"), col=c("red", "yellow", "green"),
       pch=c(16,16,16),lty=c(1,2), ncol=1)

#pc scores with cluster assignment
newdata2 <- cbind(newdata,clusters)
colors <- c("red", "yellow", "green")
colors <- colors[as.numeric(newdata2[, "clusters"])]
scatterplot3d(newdata2,color= colors,pch = 16,main="K Means Clustering")
legend("topright",legend=c("Cluster 1","Cluster 2","Cluster 3"), col=c("Black", "Red", "green"),
       pch=c(16,16,16),lty=c(1,2), ncol=1)

#sizes of 3 clusters
km.out$size

Cover_Type=ForestTypes$Cover_Type
#regular data with standardized data, clusters, and fonts
DATA <- cbind(SDATA,clusters,Cover_Type)

DATA$Cover_Type=as.integer(Data$Cover_Type)

#cluster1
clus1 <- subset(DATA, clusters == 1)
#cluster2
clus2 <- subset(DATA, clusters == 2)
#cluster3
clus3 <- subset(DATA, clusters == 3)


library(plyr)
#get a count of each font in cluster 1
count1 <- count(clus1$Cover_Type)

de<-data.frame("4","0")
names(de)<-c("x","freq")
#get a count of each font in cluster 2
count2 <- count(clus2$Cover_Type)

count4=rbind(count2[1:3,],de,count2[4:6,])
rownames(count4) <- 1:nrow(count4)
count2=count4
#get a count of each font in cluster 3
count3 <-count(clus3$Cover_Type)

#put counts together
counts <- cbind(count1,count2$freq,count3$freq)
names(counts) <- c("Cover_Type","Cluster 1", "Cluster 2","Cluster 3")


#data frame with frequencies of Covertype in each cluster
freq_CTs=cbind(count1$freq,count2$freq,count3$freq)
colnames(freq_CTs)=c("Cluster 1","Cluster 2","Cluster 3")


#k means with 4 clusters
km.out=kmeans(SDATA,4, nstart=50)
clusters = km.out$cluster

#Scalar product of w1,w2,w3 and the centers
centers = km.out$centers
ck = centers%*%eigenvec3

#sizes of 4 clusters
km.out$size

Cover_Type=ForestTypes$Cover_Type
#regular data with standardized data, clusters, and fonts
DATA <- cbind(SDATA,clusters,Cover_Type)

#cluster1
clus1 <- subset(DATA, clusters == 1)
#cluster2
clus2 <- subset(DATA, clusters == 2)
#cluster3
clus3 <- subset(DATA, clusters == 3)
#cluster4
clus4 <- subset(DATA, clusters == 4)

library(plyr)
#get a count of each font in cluster 1
count1 <- count(clus1$Cover_Type)
missval=data.frame(x="4",freq="0")
count1=rbind(count1[1:3,],missval,count1[4:6,])
rownames(count1) <- 1:nrow(count1)
#get a count of each font in cluster 2
count2 <- count(clus2$Cover_Type)
#get a count of each font in cluster 3
count3 <-count(clus3$Cover_Type)
#get a count of each font in cluster 4
count4 <-count(clus4$Cover_Type)


#put counts together
counts <- cbind(count1,count2$freq,count3$freq,count4$freq)
names(counts) <- c("Cover_Type","Cluster 1", "Cluster 2","Cluster 3","Cluster4")

#install.packages("writexl")
#library("writexl")
write_xlsx(counts,"C:\\Users\\haroo\\OneDrive\\Documents\\R\\Azencott H.W\\counts.xlsx")


#k means with 4 clusters
km.out=kmeans(SDATA,5, nstart=50)
clusters = km.out$cluster

#Scalar product of w1,w2,w3 and the centers
centers = km.out$centers
ck = centers%*%eigenvec3

#sizes of 5 clusters
km.out$size

Cover_Type=ForestTypes$Cover_Type
#regular data with standardized data, clusters, and fonts
DATA <- cbind(SDATA,clusters,Cover_Type)

#cluster1
clus1 <- subset(DATA, clusters == 1)
#cluster2
clus2 <- subset(DATA, clusters == 2)
#cluster3
clus3 <- subset(DATA, clusters == 3)
#cluster4
clus4 <- subset(DATA, clusters == 4)
#cluster5
clus5 <- subset(DATA, clusters == 5)
#library(plyr)
#get a count of each font in cluster 1
count1 <- count(clus1$Cover_Type)

#get a count of each font in cluster 2
count2 <- count(clus2$Cover_Type)
#get a count of each font in cluster 3
count3 <-count(clus3$Cover_Type)

missval=data.frame(x="4",freq="0")
count3=rbind(count3[1:3,],missval,count3[4:6,])
rownames(count3) <- 1:nrow(count3)
#get a count of each font in cluster 4
count4 <-count(clus4$Cover_Type)
#get a count of each font in cluster 5
count5 <-count(clus5$Cover_Type)

#put counts together
counts <- cbind(count1,count2$freq,count3$freq,count4$freq,count5$freq)
colnames(counts) <- c("Cover_Type","Cluster 1", "Cluster 2","Cluster 3","Cluster 4","Cluster 5")

#install.packages("writexl")
#library("writexl")
write_xlsx(counts,"C:\\Users\\haroo\\OneDrive\\Documents\\R\\Azencott H.W\\counts1.xlsx")


library(dplyr)
DATARF <- as.data.frame(DATA[,1:12])
DATARF  %>% mutate_if(is.character,as.numeric)
cty <- factor(DATA[,14])


##Q4 train test set split for random forests
DATARF=cbind(DATARF,cty)
colnames(DATARF)[13]="Cover_Type"
#seven classes 
class1 <- subset(DATARF, Cover_Type== '1')
class2 <- subset(DATARF, Cover_Type == '2')
class3 <- subset(DATARF, Cover_Type == '3')
class4 <- subset(DATARF, Cover_Type == '4')
class5 <- subset(DATARF, Cover_Type == '5')
class6 <- subset(DATARF, Cover_Type == '6')
class7 <- subset(DATARF, Cover_Type == '7')

#Train Test Split for cl1
set.seed(101)
n = nrow(class1)
trainIndex = sample(1:n, size = round(0.8*n), replace=FALSE)
traincl1 = class1[trainIndex ,]
testcl1 = class1[-trainIndex ,]

#Train Test Split for cl2
set.seed(102)
n = nrow(class2)
trainIndex1 = sample(1:n, size = round(0.8*n), replace=FALSE)
traincl2 = class2[trainIndex1 ,]
testcl2 = class2[-trainIndex1 ,]

#Train Test Split for cl3
set.seed(103)
n = nrow(class3)
trainIndex2 = sample(1:n, size = round(0.8*n), replace=FALSE)
traincl3 = class3[trainIndex2 ,]
testcl3 = class3[-trainIndex2 ,]

#Train Test Split for cl4
set.seed(104)
n = nrow(class4) 
trainIndex = sample(1:n, size = round(0.8*n), replace=FALSE)
traincl4 = class4[trainIndex ,]
testcl4 = class4[-trainIndex ,]

#Train Test Split for cl5
set.seed(105)
n = nrow(class5)
trainIndex1 = sample(1:n, size = round(0.8*n), replace=FALSE)
traincl5 = class5[trainIndex1 ,]
testcl5 = class5[-trainIndex1 ,]

#Train Test Split for cl6
set.seed(106)
n = nrow(class6)
trainIndex2 = sample(1:n, size = round(0.8*n), replace=FALSE)
traincl6 = class6[trainIndex2 ,]
testcl6 = class6[-trainIndex2 ,]

#Train Test Split for cl7
set.seed(107)
n = nrow(class7)
trainIndex2 = sample(1:n, size = round(0.8*n), replace=FALSE)
traincl7 = class7[trainIndex2 ,]
testcl7 = class7[-trainIndex2 ,]

#Full training set and test set
trainset <- rbind(traincl1,traincl2,traincl3,traincl4,traincl5,traincl6,traincl7)
testset <- rbind(testcl1,testcl2,testcl3,testcl4,testcl5,testcl6,testcl7)

##extract font label column of training and test set
test_category <- testset[,13]
train_category <- trainset[,13]

trainset1 <- trainset[,1:12]
testset1 <- testset[,1:12]

##load trees library
library(randomForest)

#Random Forest for trainset (ntree = 10)
rf_train_10 = randomForest(train_category ~., data=trainset1, ntree=10, mtry=3,importance=TRUE)


#Random Forest for testset (ntree = 10)
rf_test_10 = predict(rf_train_10,newdata=testset1)
rf_train_T10=predict(rf_train_10,newdata=trainset1)
#Confusion matrix for Test set 
conf_10 <- table(test_category,rf_test_10)
conf_10

conf_T10=table(train_category,rf_train_T10)
conf_T10
#Random Forest for trainset (ntree = 100)
rf_train_100 = randomForest(train_category ~., data=trainset1, ntree=100, mtry=3,importance=TRUE)


#Random Forest for testset (ntree = 100)
rf_test_100 = predict(rf_train_100,newdata=testset1)
rf_train_T100=predict(rf_train_100,newdata=trainset1)
#Confusion matrix for Test set 
conf_100 <- table(test_category,rf_test_100)
conf_100
#confurion matrix for training set
conf_T100=table(train_category,rf_train_T100)
conf_T100

#Random Forest for trainset (ntree = 200)
rf_train_200 = randomForest(train_category ~., data=trainset1, ntree=200, mtry=3,importance=TRUE)


#Random Forest for testset (ntree = 200)
rf_test_200 = predict(rf_train_200,newdata=testset1)
rf_train_T200=predict(rf_train_200,newdata=trainset1)

#Confusion matrix for Test set 
conf_200 <- table(test_category,rf_test_200)
conf_200

#Confusion matrix for Training set
conf_T200=table(train_category,rf_train_T200)
conf_T200

#Random Forest for trainset (ntree = 300)
rf_train_300 = randomForest(train_category ~., data=trainset1, ntree=300, mtry=3,importance=TRUE)


#Random Forest for testset (ntree = 300)
rf_test_300 = predict(rf_train_300,newdata=testset1)
rf_train_T100=predict(rf_train_300,newdata=trainset1)
#Confusion matrix for Test set 
conf_300 <- table(test_category,rf_test_300)
conf_300
#Confusion matrix for training set
conf_T300=table(train_category,rf_train_T100)
conf_T300

#Random Forest for trainset (ntree = 400)
rf_train_400 = randomForest(train_category ~., data=trainset1, ntree=400, mtry=3,importance=TRUE)


#Random Forest for testset (ntree = 400)
rf_test_400 = predict(rf_train_400,newdata=testset1)
rf_train_T400=predict(rf_train_400,newdata=trainset1)
#Confusion matrix for Test set 
conf_400 <- table(test_category,rf_test_400)
conf_400

conf_T400=table(train_category,rf_train_T400)
conf_T400


#Function to calculate accuracy
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}

##Accuracy of RF ntree = 10, test set
testperf_10 = accuracy(conf_10)
testperf_10

trainperf_10=accuracy(conf_T10)
trainperf_10

##Accuracy of RF ntree = 100, test set
testperf_100 = accuracy(conf_100)
testperf_100

trainperf_100=accuracy(conf_T100)
trainperf_100

##Accuracy of RF ntree = 200, test set
testperf_200 = accuracy(conf_200)
testperf_200

trainperf_200=accuracy(conf_T200)
trainperf_200
##Accuracy of RF ntree = 300, test set
testperf_300 = accuracy(conf_300)
testperf_300

trainperf_300=accuracy(conf_T300)
trainperf_300

##Accuracy of RF ntree = 400, test set
testperf_400 = accuracy(conf_400)
testperf_400

trainperf_400=accuracy(conf_T400)
trainperf_400
##Plot Accuracy v. ntrees
ntrees = c(10,100,200,300,400)
testperfs = c(testperf_10,testperf_100, testperf_200, testperf_300, testperf_400)
trainperfs = c(trainperf_10,trainperf_100, trainperf_200, trainperf_300, trainperf_400)
plot(ntrees, testperfs, xlim = c(0,415),ylim = c(82,87) ,type = "b", col = "blue", xlab = "N Trees", ylab = "Accuracy")

library(ggplot2)
library(dplyr)
testperfs.df=as.data.frame(testperfs)
ntrees.df=as.data.frame(ntrees)
tbl=cbind(ntrees.df,testperfs.df)
tbl2=cbind(testperfs,trainperfs)
tbl2=as.data.frame(tbl2)
tbl2=cbind(ntrees,tbl2)

ggplot(data=tbl2,aes(x=ntrees))+
  geom_line(aes(y = trainperfs, colour = "trainperfs")) +
  geom_line(aes(y = testperfs, colour = "testperfs")) +
  scale_colour_manual("", 
                      breaks = c("trainperfs", "testperfs"),
                      values = c("trainperfs"="blue", "testperfs"="red" 
                                )) +
  xlim(0,415)+scale_y_continuous("Accuracy Percentage", limits = c(82,101))+
  xlab("Ntree Values")+ ylab("Performance")+
  labs(title="N trees vs performance")



tbl$feature1=c(75.23,77.55,78.01,77.78)
tbl$feature2=c(70.60,69.21,69.44,67.59)
tbl$feature3=c(80.79,80.09,80.32,81.02)
tbl$feature4=c(95.14,95.83,95.60,95.37)
tbl$feature5=c(95.83,96.06,95.83,96.53)
tbl$feature6=c(88.66,88.19,88.43,88.89)
tbl$feature7=c(97.22,96.99,96.76,97.22)

ggplot(data=tbl,aes(x=ntrees,y=feature1))+
  geom_line(color="red")+xlim(90,415)+ylim(65,99)+
  geom_line(y=tbl$feature2,color="blue")+
  geom_line(y=tbl$feature3,color="green")+
  geom_line(y=tbl$feature4,color="purple")+
  geom_line(y=tbl$feature5,color="yellow")+
  geom_line(y=tbl$feature6,color="cyan")+
  geom_line(y=tbl$feature7,color="orange")+ ylab("classes")


ggplot(data=tbl,aes(x=ntrees))+
  geom_line(aes(y = feature1, colour = "feature1")) +
  geom_line(aes(y = feature2, colour = "feature2")) +
  geom_line(aes(y = feature3, colour = "feature3")) +
  geom_line(aes(y = feature4, colour = "feature4")) +
  geom_line(aes(y = feature5, colour = "feature5")) +
  geom_line(aes(y = feature6, colour = "feature6")) +
  geom_line(aes(y = feature7, colour = "feature7")) +
  scale_colour_manual("", 
                      breaks = c("feature1", "feature2","feature3","feature4",
                                 "feature5","feature6","feature7"),
                      values = c("feature1"="blue", "feature2"="red","feature3"="yellow",
                                "feature4"="purple","feature5"="cyan","feature6"="orange",
                                "feature7"="green")) +
  xlim(90,415)+scale_y_continuous("Accuracy Percentage", limits = c(65,99))+
  xlab("Ntree Values")+ ylab("Performances")+
  labs(title="N trees vs performance of test set for different classes ")


rf_train_300 = randomForest(train_category ~., data=trainset1, ntree=300, mtry=3,importance=TRUE)

importance(rf_train_300,class="1")

varImpPlot(rf_train_300,pch=16,col="red",cex=1.5)


plot(rf_train_300)


ggplot(class1,aes(x=Elevation))+geom_histogram(color="blue",fill="lightblue")+labs(title="Class 1 Elevation Histogram")

ggplot(class2,aes(x=Elevation))+geom_histogram(color="blue",fill="lightblue")+labs(title="Class 2 Elevation Histogram")

ggplot(class3,aes(x=Elevation))+geom_histogram(color="blue",fill="lightblue")+labs(title="Class 3 Elevation Histogram")

ggplot(class4,aes(x=Elevation))+geom_histogram(color="blue",fill="lightblue")+labs(title="Class 4 Elevation Histogram")

ggplot(class5,aes(x=Elevation))+geom_histogram(color="blue",fill="lightblue")+labs(title="Class 5 Elevation Histogram")

ggplot(class6,aes(x=Elevation))+geom_histogram(color="blue",fill="lightblue")+labs(title="Class 6 Elevation Histogram")

ggplot(class7,aes(x=Elevation))+geom_histogram(color="blue",fill="lightblue")+labs(title="Class 7 Elevation Histogram")

ks.test(class1$Elevation,class2$Elevation)

ks.test(class1$Elevation,class3$Elevation)

ks.test(class6$Elevation,class7$Elevation)

ks.test(class6$Wildnerness_Area,class7$Wildnerness_Area)


clus2$Cover_Type=1

DATARF=clus1
DATARF$Cover_Type <- as.character(DATARF$Cover_Type)
DATARF$Cover_Type <- as.factor(DATARF$Cover_Type)

#Three classes 
class1 <- subset(DATARF, Cover_Type== '1')
class2 <- subset(DATARF, Cover_Type == '2')
class3 <- subset(DATARF, Cover_Type == '3')
class4 <- subset(DATARF, Cover_Type == '4')
class5 <- subset(DATARF, Cover_Type == '5')
class6 <- subset(DATARF, Cover_Type == '6')
class7 <- subset(DATARF, Cover_Type == '7')


#Train Test Split for cl1
set.seed(101)
n = nrow(class1)
trainIndex = sample(1:n, size = round(0.8*n), replace=FALSE)
traincl1 = class1[trainIndex ,]
testcl1 = class1[-trainIndex ,]

#Train Test Split for cl2
set.seed(102)
n = nrow(class2)
trainIndex1 = sample(1:n, size = round(0.8*n), replace=FALSE)
traincl2 = class2[trainIndex1 ,]
testcl2 = class2[-trainIndex1 ,]

#Train Test Split for cl3
set.seed(103)
n = nrow(class3)
trainIndex2 = sample(1:n, size = round(0.8*n), replace=FALSE)
traincl3 = class3[trainIndex2 ,]
testcl3 = class3[-trainIndex2 ,]

#Train Test Split for cl4
set.seed(104)
n = nrow(class4) 
trainIndex = sample(1:n, size = round(0.8*n), replace=FALSE)
traincl4 = class4[trainIndex ,]
testcl4 = class4[-trainIndex ,]

#Train Test Split for cl5
set.seed(105)
n = nrow(class5)
trainIndex1 = sample(1:n, size = round(0.8*n), replace=FALSE)
traincl5 = class5[trainIndex1 ,]
testcl5 = class5[-trainIndex1 ,]

#Train Test Split for cl6
set.seed(106)
n = nrow(class6)
trainIndex2 = sample(1:n, size = round(0.8*n), replace=FALSE)
traincl6 = class6[trainIndex2 ,]
testcl6 = class6[-trainIndex2 ,]

#Train Test Split for cl7
set.seed(107)
n = nrow(class7)
trainIndex2 = sample(1:n, size = round(0.8*n), replace=FALSE)
traincl7 = class7[trainIndex2 ,]
testcl7 = class7[-trainIndex2 ,]

#Full training set and test set
trainset <- rbind(traincl1,traincl2,traincl3,traincl4,traincl5,traincl6,traincl7)
testset <- rbind(testcl1,testcl2,testcl3,testcl4,testcl5,testcl6,testcl7)

##extract font label column of training and test set
test_category <- as.factor(testset[,14])
train_category <-as.factor(trainset[,14])

trainset1 <- trainset[,1:12]
testset1 <- testset[,1:12]

#Random Forest for trainset (ntree = 300)
rf_train_300 = randomForest(train_category ~., data=trainset1, ntree=300, mtry=3,importance=TRUE)
#Random Forest for testset (ntree = 300)
rf_test_300 = predict(rf_train_300,newdata=testset1)
rf_train_T100=predict(rf_train_300,newdata=trainset1)
#Confusion matrix for Test set 
conf_300 <- table(test_category,rf_test_300)
conf_300
#Confusion matrix for training set
conf_T300=table(train_category,rf_train_300)
conf_T300

accuracy(conf_300)


class1 <- subset(Data, Cover_Type== '1')
class2 <- subset(Data, Cover_Type == '2')

newdata=rbind(class1,class2)
str(newdata)
newdata$Cover_Type=as.factor(newdata$Cover_Type)

library(e1071)

?svm
mymodel=svm(Cover_Type~.,data=newdata,kernel="linear")

plot(mymodel,data=newdata)

pred=predict(mymodel,newdata)
tab=table(Predicted=pred,Actual= newdata$Cover_Type)
tab

str(newdata)
newdata$Cover_Type=as.factor(newdata$Cover_Type)
