#############leave-one-out cross-validation
set.seed(2)
library(boot)
library(ISLR)
glm.fit=glm(mpg~horsepower,data=Auto)
cv.err=cv.glm(Auto,glm.fit)
#test error
cv.err$delta

###########k-fold cross-validation
set.seed(17)
cv.error=rep(0,10)
for (i in 1:10){
glm.fit=glm(mpg~poly(horsepower,i),data=Auto)
cv.error[i]=cv.glm(Auto,glm.fit,K=10)$delta[1]
}
cv.error

#########bootstrap
#�ȱຯ������ͳ��������ʵ��bootstrap
alpha=function(data,index){
X=data$X[index]
Y=data$Y[index]
return((var(Y)-cov(X,Y))/(var(X)+var(Y)-2*cov(X,Y)))
}
set.seed(1)
alpha(Portfolio,sample(100,100,replace=T))
#���Զ��ظ����Σ��Ӷ����������

#һ������
boot(Portfolio,alpha,R=1000)

######bootstrap�������Իع��ϵ���ķ���
boots=function(data,index)
{
return(coef(lm(mpg~horsepower,data=data,subset=index)))
}

boot(Auto,boots,R=1000)
























