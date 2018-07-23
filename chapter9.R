set.seed(1)
x=matrix(rnorm(20*2),ncol =2)
y=c(rep(-1,10),rep(1,10))
x[y==1,]=x[y==1,]+1
plot(x,col=(3-y))

dat= data.frame(x=x,y=as.factor(y))
library(e1071)
svmfit=svm(y~.,data=dat,kernel ="linear",cost=10,scale =FALSE)
#cost控制了容忍度
plot(svmfit,dat) 
#查看支持向量
svmfit$index

summary(svmfit)

######用tune来进行cross-validation
set.seed(1)
tune.out=tune(svm,y~.,data=dat,kernel ="linear",
ranges =list(cost=c(0.001,0.01,0.1,1,5,10,100)))
summary(tune.out)

bestmod=tune.out$best.model
summary(bestmod)

###测试集
xtest=matrix(rnorm(20*2),ncol =2)
ytest=sample(c(-1,1),20,rep = TRUE)
xtest[ytest==1,]= xtest[ytest==1,] + 1
testdat =data.frame(x=xtest,y=as.factor(ytest))

ypred=predict(bestmod,testdat)
table(predict=ypred,truth=testdat$y)

########非线性
set.seed(1)
x=matrix(rnorm(200*2),ncol =2)
x[1:100,]= x[1:100,]+2
x[101:150,]=x[101:150,]-2
y=c(rep(1,150),rep(2,50))

train= sample(200,100)
svmfit=svm(y~.,data=dat[train,],kernel ="radial",gamma =1,cost =1)
plot(svmfit,dat[train,])

summary(svmfit)

#####cross-validation
set.seed(1)
tune.out=tune(svm , y~., data=dat [train ,], kernel ="radial",
ranges =list(cost=c(0.1 ,1 ,10 ,100 ,1000),gamma =c(0.5 ,1 ,2 ,3 ,4)))
summary(tune.out)

table(true=dat[-train ,"y"], pred= predict(tune.out$best.model ,newdata=dat[-train,]))

####roc曲线
library(ROCR)
rocplot=function(pred,truth,...){
predob=prediction(pred,truth)
perf=performance(predob,"tpr","fpr")
plot(perf,...)}

#得到预测真值
svmfit.opt =svm (y~.,data =dat[train,], kernel ="radial",gamma=2,cost=1,decision.values =T)
fitted = attributes(predict(svmfit.opt ,dat[train,], decision.values =TRUE ))$decision.values
par(mfrow =c(1,2))
rocplot(fitted,dat[train,"y"],main ="Training Data")
???有问题

##########多分类问题
set.seed(1)
x=rbind(x,matrix(rnorm(50*2),ncol =2) )
y=c(y,rep(0,50))
x[y==0,2]= x[y==0,2]+2
dat=data.frame(x=x, y=as.factor (y))
par(mfrow =c(1,1))
plot(x,col =(y+1))

svmfit=svm(y~.,data=dat,kernel ="radial", cost=10, gamma=1)
plot(svmfit,dat)
