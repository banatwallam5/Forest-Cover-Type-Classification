

# We begin by declaring variables
summary(cleanDataAuto)
n=392
mpg=cleanDataAuto$mpg
F1=cleanDataAuto$cylinders
F2=cleanDataAuto$displacement
F3=cleanDataAuto$horsepower
F4=cleanDataAuto$weight
F5=cleanDataAuto$acceleration
#Q1 Find the mean and STd of each f
mean(F1)
mean(F2)
mean(F3)
mean(F4)
mean(F5)
sd(F1)
sd(F2)
sd(F3)
sd(F4)
sd(F5)

#Q2 We use r to compute the histograms 
hist(F1,xlab="Cylinders",main="Histogram of Cylinders")
hist(F2,xlab="Displacement",main="Histogram of Displacement")
hist(F3,xlab="Horse Power",main="Histogram of Horse Power")
hist(F4,xlab="Weight",main="Histogram of Weight")
hist(F5,xlab="Acceleration",main="Histogram of Acceleration")
hist(mpg,xlab="MPG",main="Histogram of MPG")

#Q3) We use r to plot the scatter diagrams
plot(F1,mpg,xlab="Cylinders",main="MPG Cylinders Scatterplot")
# From the Plots it seems like cars with more cylinders tend to have Lower MPGs
plot(F2,mpg,xlab="Displacement",main="MPG Displacement Scatterplot")
#Based on the plot, Cars with higher displacement tend to have lower MPGs
plot(F3,mpg,xlab="Horse Power",main="MPG-Horse Power Scatterplot")
#Based on the Plot, Cars with more Horse Power ten to have lower MPGs
plot(F4,mpg,xlab="Weight",main="MPG-Weight Scatterplot")
#Based on the Scatter plot The more a cars weight the lowers MPGs it seem to have
plot(F5,mpg,xlab="Acceleration",main="MPG-Acceleration Scatterplot")
#Some cars with very low acceleration seem to have low MPGs, but other then that,
#there does not seem to be a clear trend.


#Q5)Usinf r to compute correlations
cor(F1,mpg)
#This shows a strong negative correlation
cor(F2,mpg)
#This shows a strong negative correlation
cor(F3,mpg)
#This shows a strong negative correlation
cor(F4,mpg)
#This Shows a strong negative correlation
cor(F5,mpg)
#This shows a weak positive correlation
#Q6) computing the correlations matrix
cor(cleanDataAuto)
#q7Getting the quantile curve
quantile(mpg, probs = seq(0,1,0.01))
plot(quantile(mpg, probs = seq(0,1,0.01)),xlab="Quantiles",ylab = "MPG",main = "Increasing Quantile Curve")
plot((1:length(mpg)-1)/(length(mpg)-1),sort(mpg))


#q8
#We create a new data set that is basically out orinal data set but sorted by the mpg column
cda_sort=cleanDataAuto[order(mpg),]

# we extract two disjoint tables from our sorted data set 
Lowmpg=cda_sort[1:130,1:6]
head(Lowmpg)
Highmpg=cda_sort[261:392,1:6]
head(Highmpg)
#------------------------------------
# we declare some more variables before we continue as we will need them going forward
fl1=Lowmpg$cylinders
fl2=Lowmpg$displacement
fl3=Lowmpg$horsepower
fl4=Lowmpg$weight
fl5=Lowmpg$acceleration
lmpg=Lowmpg$mpg

fh1=Highmpg$cylinders
fh2=Highmpg$displacement
fh3=Highmpg$horsepower
fh4=Highmpg$weight
fh5=Highmpg$acceleration
hmpg=Highmpg$mpg

#we display the 10 histograms of the features in both sets side by side
par(mfrow=c(1,2))
hist(fl1,xlab ="Cylinders", main="Low MPG")
hist(fh1,xlab ="Cylinders", main="High MPG")

par(mfrow=c(1,2))
hist(fl2,xlab ="Displacement", main="Low MPG")
hist(fh2,xlab ="Displacement", main="High MPG")

par(mfrow=c(1,2))
hist(fl3,xlab ="Horse Power", main="Low MPG")
hist(fh3,xlab ="Horse Power", main="High MPG")

par(mfrow=c(1,2))
hist(fl4,xlab ="Weight", main="Low MPG")
hist(fh4,xlab ="Weight", main="High MPG")

par(mfrow=c(1,2))
hist(fl5,xlab ="Acceleration", main="Low MPG")
hist(fh5,xlab ="Acceleration", main="High MPG")

# we compute the means for the feature in lowmpg
mean(fl1)
mean(fl2)
mean(fl3)
mean(fl4)
mean(fl5)

#we compute the standard deviations for the features in Lowmpg
sd(fl1)
sd(fl2)
sd(fl3)
sd(fl4)
sd(fl5)

# we compute the means for the feature in Highmpg
mean(fh1)
mean(fh2)
mean(fh3)
mean(fh4)
mean(fh5)

#we compute the standard deviations for the features in Highmpg

sd(fh1)
sd(fh2)
sd(fh3)
sd(fh4)
sd(fh5)


#q12# we do the required calculations to compute the discriminant of each feature
sf1=(((sd(fl1)^2)+(sd(fh1)^2))/130)^(1/2)
sf1
discrf1=(-(mean(fh1)-mean(fl1)))/sf1
discrf1

sf2=(((sd(fl2)^2)+(sd(fh2)^2))/130)^(1/2)
sf2
discrf2=(-(mean(fh2)-mean(fl2)))/sf2
discrf2
sf3=(((sd(fl3)^2)+(sd(fh3)^2))/130)^(1/2)
sf3
discrf3=(-(mean(fh3)-mean(fl3)))/sf3
discrf3
sf4=(((sd(fl4)^2)+(sd(fh4)^2))/130)^(1/2)
sf4
discrf4=(-(mean(fh4)-mean(fl4)))/sf4
discrf4
sf5=(((sd(fl5)^2)+(sd(fh5)^2))/130)^(1/2)
sf5
discrf5=((mean(fh5)-mean(fl5)))/sf5
discrf5

#q13 we do the required calculations to compute the thresholds
thrf1=((mean(fl1)*sd(fh1))+(mean(fh1)*sd(fl1)))/(sd(fh1)+sd(fl1))
thrf1
thrf2=((mean(fl2)*sd(fh2))+(mean(fh2)*sd(fl2)))/(sd(fh2)+sd(fl2))
thrf2
thrf3=((mean(fl3)*sd(fh3))+(mean(fh3)*sd(fl3)))/(sd(fh3)+sd(fl3))
thrf3
thrf4=((mean(fl4)*sd(fh4))+(mean(fh4)*sd(fl4)))/(sd(fh4)+sd(fl4))
thrf4
thrf5=((mean(fl5)*sd(fh5))+(mean(fh5)*sd(fl5)))/(sd(fh5)+sd(fl5))
thrf5

#Now we will add the new columns to our original data set for each score f(n) and the fullscore(n) column
cleanDataAuto$Scoref1=c(seq(1:392))
Scoref1=cleanDataAuto$Scoref1
cleanDataAuto$Scoref2=c(seq(1:392))
Scoref2=cleanDataAuto$Scoref2
cleanDataAuto$Scoref3=c(seq(1:392))
Scoref3=cleanDataAuto$Scoref3
cleanDataAuto$Scoref4=c(seq(1:392))
Scoref4=cleanDataAuto$Scoref4
cleanDataAuto$Scoref5=c(seq(1:392))
Scoref5=cleanDataAuto$Scoref5
cleanDataAuto$fullscore=c(seq(1:392))
fullscore=cleanDataAuto$fullscore

# Next we use a loop and if statements to fill in the columns based on the respective values in the feature sections as well as the rules of scoring outlined in the question: 
for (i in 1:392){
  #For cylinders Since M(H)<M(L)
  Scoref1[i]=0
  if(F1[i]<thrf1)
  {Scoref1[i]= 1}
  
  
  else 
  {Scoref1[i]= -1}
  
  Scoref1
  
  cleanDataAuto$Scoref1[i]=Scoref1[i]
  #-----------------
  #For Displacement Since M(H)<M(L) we have,
  Scoref2[i]=0
  if(F2[i]<thrf2)
  {Scoref2[i]= 1}
  
  
  else 
  {Scoref2[i]= -1}
  
  
  Scoref2
  
  cleanDataAuto$Scoref2[i]=Scoref2[i]
  #----------------------
 
  #For Horsepower Since M(H)<M(L) we have,
   Scoref3[i]=0
  if(F3[i]<thrf3)
  {Scoref3[i]= 1}
  
  
  else 
  {Scoref3[i]= -1}
  
  
  Scoref3
  
  cleanDataAuto$Scoref3[i]=Scoref3[i]
  #---------------------
  #For Weight Since M(H)<M(L) we have,
  Scoref4[i]=0
  if(F4[i]<thrf4)
  {Scoref4[i]= 1}
  else 
  {Scoref4[i]= -1}
  Scoref4
  cleanDataAuto$Scoref4[i]=Scoref4[i]
  #----------------------------
  ##For Weight Since M(H)>M(L) we have,
  Scoref5[i]=0
  if(F5[i]>thrf5)
  {Scoref5[i]= 1}
  
  
  else 
  {Scoref5[i]= -1}
  
  
  Scoref5
  
  cleanDataAuto$Scoref5[i]=Scoref5[i]
  
  fullscore[i]=Scoref1[i]+Scoref2[i]+Scoref3[i]+Scoref4[i]+Scoref5[i]
  
  cleanDataAuto$fullscore[i]=fullscore[i]
}

# Computing the median and creating the new column true class
med=median(mpg)
cleanDataAuto$trueclass="notset"
trueclass=cleanDataAuto$trueclass
for (i in 1:392){
  if (mpg[i]>med)
  {trueclass[i]="Highmpg"
  cleanDataAuto$trueclass[i]=trueclass[i] 
  }
  else{
    trueclass[i]="Lowmpg"
    cleanDataAuto$trueclass[i]=trueclass[i]
  }
}

#All cases in the lowmpg will be classified as low as it is the bottom third of our sorted data and thus all values will be below the median

#conversely all cases in the highmpg will be classed as high as it is the top third of our data and thus above the median
library(dplyr)
cleanDataAuto=cleanDataAuto[order(mpg),]
Lowmpg=cleanDataAuto[1:130,1:13]
Highmpg=cleanDataAuto[261:392,1:13]
Train=union(Lowmpg,Highmpg)
View(Train)

#install.packages("e1071")
#install.packages("caret")
library("e1071")
library("caret")
lt=nrow(Train)
lt
Train$autoclassifier=c(1:lt)


for(i in 1:261){
  if (Train$fullscore[i] < 1) 
  {Train$autoclassifier[i]="Lowmpg"
  Train$autoclassifier[i]=Train$autoclassifier[i]}
  else
  {Train$autoclassifier[i]="Highmpg"
  Train$autoclassifier[i]=Train$autoclassifier[i]}
  
}

u=union(Train$autoclassifier,Train$trueclass)
cm=confusionMatrix(factor(Train$trueclass,u),factor(Train$autoclassifier,u))
cm
#--------Testing on the Test set

testdata=cleanDataAuto[131:260,1:13]
testdata$autoclassifiertest=c(1:130)

for(i in 1:130){
  if (testdata$fullscore[i] < 1) 
  {testdata$autoclassifiertest[i]="Lowmpg"
  testdata$autoclassifiertest[i]=testdata$autoclassifiertest[i]}
  else
  {testdata$autoclassifiertest[i]="Highmpg"
  testdata$autoclassifiertest[i]=testdata$autoclassifiertest[i]}
  
}

w=union(testdata$autoclassifiertest,testdata$trueclass)
cm=confusionMatrix(factor(testdata$trueclass,w),factor(testdata$autoclassifiertest,w))
cm






##################################################
cleanDataAuto$autoclassifierglobal=c(1:392)


for(i in 1:392){
  if (cleanDataAuto$fullscore[i] < 1) 
  {cleanDataAuto$autoclassifierglobal[i]="Lowmpg"
  cleanDataAuto$autoclassifierglobal[i]=cleanDataAuto$autoclassifierglobal[i]}
  else
  {cleanDataAuto$autoclassifierglobal[i]="Highmpg"
  cleanDataAuto$autoclassifierglobal[i]=cleanDataAuto$autoclassifierglobal[i]}
  
}

v=union(cleanDataAuto$autoclassifierglobal,cleanDataAuto$trueclass)
cm=confusionMatrix(factor(cleanDataAuto$trueclass,v),factor(cleanDataAuto$autoclassifierglobal,v))
cm


##############################################################
#classifier as 2
###################################################

for(i in 1:261){
  if (Train$fullscore[i] < 2) 
  {Train$autoclassifier[i]="Lowmpg"
  Train$autoclassifier[i]=Train$autoclassifier[i]}
  else
  {Train$autoclassifier[i]="Highmpg"
  Train$autoclassifier[i]=Train$autoclassifier[i]}
  
}

u=union(Train$autoclassifier,Train$trueclass)
cm=confusionMatrix(factor(Train$trueclass,u),factor(Train$autoclassifier,u))
cm
#--------Testing on the Test set

testdata=cleanDataAuto[131:260,1:13]
testdata$autoclassifiertest=c(1:130)

for(i in 1:130){
  if (testdata$fullscore[i] < 2) 
  {testdata$autoclassifiertest[i]="Lowmpg"
  testdata$autoclassifiertest[i]=testdata$autoclassifiertest[i]}
  else
  {testdata$autoclassifiertest[i]="Highmpg"
  testdata$autoclassifiertest[i]=testdata$autoclassifiertest[i]}
  
}

w=union(testdata$autoclassifiertest,testdata$trueclass)
cm=confusionMatrix(factor(testdata$trueclass,w),factor(testdata$autoclassifiertest,w))
cm

##################################################
cleanDataAuto$autoclassifierglobal=c(1:392)


for(i in 1:392){
  if (cleanDataAuto$fullscore[i] < 2) 
  {cleanDataAuto$autoclassifierglobal[i]="Lowmpg"
  cleanDataAuto$autoclassifierglobal[i]=cleanDataAuto$autoclassifierglobal[i]}
  else
  {cleanDataAuto$autoclassifierglobal[i]="Highmpg"
  cleanDataAuto$autoclassifierglobal[i]=cleanDataAuto$autoclassifierglobal[i]}
  
}

v=union(cleanDataAuto$autoclassifierglobal,cleanDataAuto$trueclass)
cm=confusionMatrix(factor(cleanDataAuto$trueclass,v),factor(cleanDataAuto$autoclassifierglobal,v))
cm
###########################################################
#auto classifier =3
###########################################################
for(i in 1:261){
  if (Train$fullscore[i] < 3) 
  {Train$autoclassifier[i]="Lowmpg"
  Train$autoclassifier[i]=Train$autoclassifier[i]}
  else
  {Train$autoclassifier[i]="Highmpg"
  Train$autoclassifier[i]=Train$autoclassifier[i]}
  
}

u=union(Train$autoclassifier,Train$trueclass)
cm=confusionMatrix(factor(Train$trueclass,u),factor(Train$autoclassifier,u))
cm
#--------Testing on the Test set

testdata=cleanDataAuto[131:260,1:13]
testdata$autoclassifiertest=c(1:130)

for(i in 1:130){
  if (testdata$fullscore[i] < 3) 
  {testdata$autoclassifiertest[i]="Lowmpg"
  testdata$autoclassifiertest[i]=testdata$autoclassifiertest[i]}
  else
  {testdata$autoclassifiertest[i]="Highmpg"
  testdata$autoclassifiertest[i]=testdata$autoclassifiertest[i]}
  
}

w=union(testdata$autoclassifiertest,testdata$trueclass)
cm=confusionMatrix(factor(testdata$trueclass,w),factor(testdata$autoclassifiertest,w))
cm

##################################################
cleanDataAuto$autoclassifierglobal=c(1:392)


for(i in 1:392){
  if (cleanDataAuto$fullscore[i] < 3) 
  {cleanDataAuto$autoclassifierglobal[i]="Lowmpg"
  cleanDataAuto$autoclassifierglobal[i]=cleanDataAuto$autoclassifierglobal[i]}
  else
  {cleanDataAuto$autoclassifierglobal[i]="Highmpg"
  cleanDataAuto$autoclassifierglobal[i]=cleanDataAuto$autoclassifierglobal[i]}
  
}

v=union(cleanDataAuto$autoclassifierglobal,cleanDataAuto$trueclass)
cm=confusionMatrix(factor(cleanDataAuto$trueclass,v),factor(cleanDataAuto$autoclassifierglobal,v))
cm