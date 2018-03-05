# neural network for prediction of traffic flow at a particular link 

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
allVars<-colnames(dat)
predictorVars<-allVars[!allVars%in%"Flow"]
predictorVars<-paste(predictorVars,collapse="+")
form=as.formula(paste("Flow~",predictorVars,collapse="+"))
neuralmodel<-neuralnet(formula=form,hidden=c(4,2),linear.output=TRUE,data=train,stepmax=1e6)

#plot
plot(neuralmodel)

##Gives Error: 16.146457   Steps: 130553

#prediction for test data set
predictions<-compute(neuralmodel,test[,1:5])
str(predictions)
predictions<-predictions$net.result*(max(test$Flow)-min(test$Flow))+min(test$Flow)
actualvalues<-(test$Flow)*(max(test$Flow)-min(test$Flow))+min(test$Flow) 

#calculation of Mean Squared Data, 0 means perfect model
MSE<-sum((predictions-actualvalues)^2)/nrow(test)
MSE

##Root Mean Squared error
RMSE=sqrt(MSE)
RMSE

###Gives MSE=0.008050404272 & RMSE=0.08972404511

#plot2
par(mfrow=c(1,2))
plot(test$Flow,predictions,col='red',main='Real vspredicted NN',ch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='NN',pch=18,col='red',bty='n')

