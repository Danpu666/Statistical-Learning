library(ISLR)
cor(Smarket[,-9])

#运行逻辑回归
glm.fits=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
             data=Smarket,family=binomial)
summary(glm.fits)
#模型的系数
coef(glm.fits)
#返回预测为1的概率
attach(Smarket)
contrasts(Direction)#up为1
glm.probs=predict(glm.fits,type="response")
glm.probs[1:10]
#返回升还是降
glm.pred=rep("Down",1250)
glm.pred[glm.probs>0.5]="Up"
#返回混淆矩阵
table(glm.pred,Direction)

#创造训练集和测试集
train=(Year<2005)
Smarket.2005=Smarket[!train,]
Direction.2005=Direction[!train]

glm.fits=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
             data=Smarket,family=binomial,subset=train)
glm.probs=predict(glm.fits,Smarket.2005,type="response")
#返回升还是降
glm.pred=rep("Down",252)
glm.pred[glm.probs>0.5]="Up"
#返回混淆矩阵
table(glm.pred,Direction.2005)

#移除变量
glm.fits=glm(Direction~Lag1+Lag2,data=Smarket,family=binomial,subset=train)
glm.probs=predict(glm.fits,Smarket.2005,type="response")
#返回升还是降
glm.pred=rep("Down",252)
glm.pred[glm.probs>0.5]="Up"
#返回混淆矩阵
table(glm.pred,Direction.2005)

#########线性判别分析
library(MASS)
lda.fit=lda(Direction~Lag1+Lag2,data=Smarket,subset=train)
lda.fit
plot(lda.fit)
#预测
lda.pred=predict(lda.fit,Smarket.2005)
#predict返回三个值，第一个值为分类结果,第二个值返回概率
lda.class=lda.pred$class
table(lda.class,Direction.2005)

########多次判别分析
qda.fit=qda(Direction~Lag1+Lag2,data=Smarket,subset=train)
qda.fit
qda.class=predict(qda.fit,Smarket.2005)$class
table(qda.class,Direction.2005)

#########knn
library(class)
train.X=cbind(Lag1,Lag2)[train,]
test.X=cbind(Lag1,Lag2)[!train,]
train.Direction=Direction[train]
set.seed(1)
knn.pred=knn(train.X,test.X,train.Direction,k=1)
table(knn.pred,Direction.2005)





























