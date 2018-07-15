#�����
library(MASS)
library(ISLR)

#############simple linear regression
#Boston���ݼ���yΪmedv
fix(Boston)
lm.fit=lm(medv~lstat,data=Boston)

#����
attach(Boston)
lm.fit=lm(medv~lstat)

lm.fit
summary(lm.fit)

#ϵ��
coef(lm.fit)
#��������
confint(lm.fit)
#Ԥ��prediction�����confidence����
predict(lm.fit,data.frame(lstat=c(5,10,15)),
interval='confidence')
predict(lm.fit,data.frame(lstat=c(5,10,15)),
interval='prediction')

#��ͼ
plot(lstat,medv)
abline(lm.fit)

#���ͼ
par(mfrow=c(2,2))
plot(lm.fit)

#�в�ͼ
plot(predict(lm.fit),residuals(lm.fit))
plot(predict(lm.fit),rstudent(lm.fit))

#�߸ܸ˵�
plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit))

##########��Ԫ���Իع�
lm.fit=lm(medv~.,data=Boston)

library(car)
vif(lm.fit)

#���ӽ�����
lm(medv~lstat*age,data=Boston)

#�����Ա任
lm(medv~lstat+I(lstat^2))
lm(medv~poly(lstat,3))

#��������Ƚ��ĸ�ģ�ͱȽϺ�
anova(lm.fit,lm.fit2)

#���Եı�����δ���
#R�������Զ����д���
#contrasts()����dummy variables

























