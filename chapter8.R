##########分类
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

#建立训练集和测试集
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
#以错误率来引导cv，默认是方差
cv.carseats 

par(mfrow=c(1,2))
plot(cv.carseats$size,cv.carseats$dev,type ="b")
plot(cv.carseats$k,cv.carseats$dev,type ="b")

#剪枝
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
#13表示在划分节点的时候考虑13个变量
bag.boston=randomForest(medv~.,data= Boston,subset=train,mtry=13,importance=TRUE)
bag.boston

yhat.bag=predict(bag.boston,newdata=Boston[-train,])
plot(yhat.bag,boston.test)
abline(0,1)

#改变ntree控制树生成的数量
bag.boston =randomForest(medv~.,data= Boston,subset =train,mtry=13,ntree=25)
yhat.bag=predict(bag.boston,newdata=Boston[-train,])

######random forest
set.seed(1)
rf.boston=randomForest(medv~.,data=Boston,subset=train,mtry=6,importance =TRUE)
yhat.rf = predict(rf.boston,newdata =Boston[-train ,])
#显示变量的重要性
importance(rf.boston)
varImpPlot(rf.boston)

#########boosting
library(gbm)
set.seed(1)
boost.boston =gbm(medv~.,data=Boston[train,],
distribution="gaussian",n.trees=5000,interaction.depth =4)
#gaussian表示为回归，如果是分类则是bernoulli
summary(boost.boston)
#产生一个相对影响图
#选择变量的marginal effect
par(mfrow =c(1,2))
plot(boost.boston,i="rm")
plot(boost.boston,i="lstat")

yhat.boost= predict (boost.boston,newdata=Boston[-train,],n.trees=5000)