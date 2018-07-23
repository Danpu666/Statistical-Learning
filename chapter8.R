##########����
library(tree)
library(ISLR )
attach(Carseats )
High=ifelse(Sales<=8," No"," Yes ")
Carseats=data.frame(Carseats,High)
tree.carseats=tree(High~.-Sales,Carseats)
summary(tree.carseats)
plot(tree.carseats)
text(tree.carseats,pretty =0)
tree.carseats

#����ѵ�����Ͳ��Լ�
set.seed(2)
train=sample(1:nrow(Carseats),200)
Carseats.test=Carseats[-train,]
High.test=High[-train]
tree.carseats=tree(High~.-Sales,Carseats,subset=train)
tree.pred = predict(tree.carseats,Carseats.test,type ="class")
table(tree.pred,High.test)

#cross-validation
set.seed(3)
cv.carseats=cv.tree(tree.carseats,FUN = prune.misclass)
#�Դ�����������cv��Ĭ���Ƿ���
cv.carseats 

par(mfrow=c(1,2))
plot(cv.carseats$size,cv.carseats$dev,type ="b")
plot(cv.carseats$k,cv.carseats$dev,type ="b")

#��֦
prune.carseats=prune.misclass(tree.carseats,best =9)
plot(tree.carseats)
text(tree.carseats,pretty =0)

tree.pred=predict(prune.carseats,Carseats.test,type="class")
table(tree.pred,High.test)

#########regression trees
library(MASS)
set.seed(1)
train=sample(1:nrow(Boston),nrow(Boston)/2)
tree.boston=tree(medv~.,Boston,subset= train )
summary(tree.boston)

plot(tree.boston)
text(tree.boston,pretty=0)

yhat= predict(tree.boston,newdata=Boston[-train,])
boston.test=Boston[-train,"medv"]
plot(yhat,boston.test)
abline(0,1)

#########bagging
library(randomForest)
set.seed(1)
#13��ʾ�ڻ��ֽڵ��ʱ����13������
bag.boston=randomForest(medv~.,data= Boston,subset=train,mtry=13,importance=TRUE)
bag.boston

yhat.bag=predict(bag.boston,newdata=Boston[-train,])
plot(yhat.bag,boston.test)
abline(0,1)

#�ı�ntree���������ɵ�����
bag.boston =randomForest(medv~.,data= Boston,subset =train,mtry=13,ntree=25)
yhat.bag=predict(bag.boston,newdata=Boston[-train,])

######random forest
set.seed(1)
rf.boston=randomForest(medv~.,data=Boston,subset=train,mtry=6,importance =TRUE)
yhat.rf = predict(rf.boston,newdata =Boston[-train ,])
#��ʾ��������Ҫ��
importance(rf.boston)
varImpPlot(rf.boston)

#########boosting
library(gbm)
set.seed(1)
boost.boston =gbm(medv~.,data=Boston[train,],
distribution="gaussian",n.trees=5000,interaction.depth =4)
#gaussian��ʾΪ�ع飬����Ƿ�������bernoulli
summary(boost.boston)
#����һ�����Ӱ��ͼ
#ѡ�������marginal effect
par(mfrow =c(1,2))
plot(boost.boston,i="rm")
plot(boost.boston,i="lstat")

yhat.boost= predict (boost.boston,newdata=Boston[-train,],n.trees=5000)