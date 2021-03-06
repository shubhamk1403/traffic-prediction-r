##neural network with 1 hidden layer with 10 neurons

library(neuralnet)
library(MASS)
#Loading data
rdata<-read.csv("Test.csv")

colnames(rdata)
str(rdata)

##new dataframe dropping linkref, linkdesc, date because the neural net is net accepting categorical variables an generating following errors
##Error in neurons[[i]] %*% weights[[i]] : requires numeric/complex matrix/vector arguments
rdat<-rdata[c(4:9)]
colnames(rdat)
str(rdat)

#histogram of flow
hist(rdat$Flow) 

#finding missing value
s=0;
for(i in 1:6)
{
	for(j in dat[,i])
	{
		if(is.na(j))
		s=s+1
	}
}
s

#handling missing value
j=0
for(i in dat[ ,"Flow"]){
j=j+1
if(is.na(i)){
y=dat[j,"TimePeriod"]
x=match(y,dat[,"TimePeriod"])
dat[x,"Flow"]=y
}}

#finding missing values again
s=0;
for(i in 1:6)
{
	for(j in dat[,i])
	{
		if(is.na(j))
		s=s+1
	}
}
s


#outlier
#boxplot
boxplot(rdat)
T=boxplot(rdat[,"AverageJT"])
k<-T$out

#outlier Treatment
l=subset(rdat,rdat[,"AverageJT"]>=114.27 & rdat[,"AverageJT"]<=205.37)
T=boxplot(l[,"Flow"])
T$stats
boxplot(p)
boxplot.stats(p$Flow)
p=subset(l,l[,"Flow"]>=8 & l[,"Flow"]<=576)
rdat<-p
boxplot(rdat)
str(rdat)

#Scaling/Normalization ***Normalize only once or the model wil be distorted and error will increase*** 
f<-function(x) (x-min(x))/(max(x)-min(x))
rdat<-as.data.frame(apply(rdat,2,f))
str(rdat)

#dividing into test&train ***80% training data 20% test data***
index<-sample(1:nrow(dat),round(0.80*nrow(dat)))
train<-dat[index,]
test<-dat[-index,]

str(train)
str(test)

#fittng neural net 
allVar<-colnames(d)
predictorVar<-allVar[!allVar%in%"Flow"]
predictorVar<-paste(predictorVar,collapse="+")
formm=as.formula(paste("Flow~",predictorVar,collapse="+"))
nmod<-neuralnet(formula=formm,hidden=10,linear.output=TRUE,data=trn,stepmax=1e6)

#plot
plot(nmod)

##Error: 13.925987   Steps: 203446

#prediction for test data set
prediction<-compute(nmod,test[,1:5])
str(prediction)
prediction<-prediction$net.result*(max(tst$Flow)-min(tst$Flow))+min(tst$Flow)
actualvalue<-(tst$Flow)*(max(tst$Flow)-min(tst$Flow))+min(tst$Flow) 

#calculation of Mean Squared Data, 0 means perfect model
MSE<-sum((prediction-actualvalue)^2)/nrow(tst)
MSE

##Root Mean Squared error
RMSE=sqrt(MSE)
RMSE

##Gives MSE=0.006822681433 & RMSE=0.08259952441

#plot2
par(mfrow=c(1,2))
plot(tst$Flow,prediction,col='red',main='Real vspredicted NN',ch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='NN',pch=18,col='red',bty='n')
