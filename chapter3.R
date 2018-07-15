#载入包
library(MASS)
library(ISLR)

#############simple linear regression
#Boston数据集，y为medv
fix(Boston)
lm.fit=lm(medv~lstat,data=Boston)

#或者
attach(Boston)
lm.fit=lm(medv~lstat)

lm.fit
summary(lm.fit)

#系数
coef(lm.fit)
#置信区间
confint(lm.fit)
#预测prediction区间和confidence区间
predict(lm.fit,data.frame(lstat=c(5,10,15)),
interval='confidence')
predict(lm.fit,data.frame(lstat=c(5,10,15)),
interval='prediction')

#绘图
plot(lstat,medv)
abline(lm.fit)

#诊断图
par(mfrow=c(2,2))
plot(lm.fit)

#残差图
plot(predict(lm.fit),residuals(lm.fit))
plot(predict(lm.fit),rstudent(lm.fit))

#高杠杆点
plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit))

##########多元线性回归
lm.fit=lm(medv~.,data=Boston)

library(car)
vif(lm.fit)

#增加交互项
lm(medv~lstat*age,data=Boston)

#非线性变换
lm(medv~lstat+I(lstat^2))
lm(medv~poly(lstat,3))

#方差分析比较哪个模型比较好
anova(lm.fit,lm.fit2)

#定性的变量如何处理
#R软件会自动进行处理
#contrasts()返回dummy variables


























