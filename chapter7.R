library(ISLR)
attach(Wage)

#多项式拟合
fit=lm(wage~poly(age ,4),data=Wage)
coef(summary(fit))

agelims=range(age)
age.grid=seq(from=agelims[1],to=agelims[2])
#求预测值与置信区间
preds=predict(fit,newdata=list(age=age.grid),se=TRUE)
se.bands=cbind(preds$fit+2*preds$se.fit,preds$fit-2*preds$se.fit)
#绘图
par(mfrow=c(1,2),mar =c(4.5 ,4.5 ,1 ,1) ,oma =c(0 ,0 ,4 ,0) )
plot(age,wage,xlim =agelims ,cex =.5, col =" darkgrey ")
title("Degree -4 Polynomial ", outer =T)
lines(age.grid, preds$fit ,lwd =2, col =" blue ")
matlines (age.grid,se.bands ,lwd =1, col =" blue",lty =3)

#进行方差分析看是否该模型合适
fit.1= lm( wage~age , data=Wage )
fit.2= lm( wage~poly (age ,2) ,data =Wage)
fit.3= lm( wage~poly (age ,3) ,data =Wage)
fit.4= lm( wage~poly (age ,4) ,data =Wage)
fit.5= lm( wage~poly (age ,5) ,data =Wage)
anova (fit.1, fit.2, fit.3, fit.4, fit.5)
#a cubic or a quartic polynomial appear to provide a reasonable fit to the data

#逻辑回归多项式拟合
####判断这个人是否会earn 超过250000
fit=glm(I(wage >250)~poly(age,4),data=Wage,family = binomial)
preds=predict(fit,newdata =list(age=age.grid),se=T)
#预测的是x*beta的形式
pfit=exp(preds$fit)/(1+exp(preds$fit))
se.bands.logit = cbind(preds$fit+2*preds$se.fit, preds$fit-2*preds$se.fit)
se.bands = exp(se.bands.logit)/(1+exp(se.bands.logit))

#绘图
#jitter对数据增加扰动
plot(age,I(wage >250),xlim=agelims,type ="n", ylim =c(0,.2))
points (jitter(age), I((wage >250)/5),cex =.5, pch ="|" ,col ="darkgrey")
lines(age.grid,pfit,lwd =2, col =" blue ")
matlines(age.grid,se.bands,lwd=1,col="blue",lty=3)
##一种直接的方式
preds =predict(fit,newdata =list(age =age.grid),type ="response",se=T)

###多个水平的回归
fit=lm(wage~cut(age,4),data=Wage)
coef(summary(fit))

############splins
library(splines)
fit =lm(wage~bs(age,knots=c(25,40,60)),data=Wage)
#knot表示点，默认是cubic 
#因为有3个断点，所以总共有6个basis function
pred= predict(fit,newdata =list(age =age.grid),se=T)
plot(age,wage,col =" gray ")
lines (age.grid, pred$fit ,lwd =2)
lines (age.grid, pred$fit +2* pred$se ,lty ="dashed")
lines (age.grid, pred$fit -2* pred$se ,lty ="dashed")

#可以不用指定断点，而指定basis function的个数,断点的个数是按照百分比来确定的
attr(bs(age,df=6),"knots")
fit2=lm(wage~ns(age,df =4),data =Wage)
pred2 = predict(fit2, newdata = list(age = age.grid ),se=T)
lines(age.grid,pred2$fit,col ="red", lwd =2)

########smooth splines
plot(age,wage,xlim =agelims,cex =.5,col ="darkgrey")
title ("Smoothing Spline")
#指定df
fit=smooth.spline(age,wage,df =16)
#应用cv确定df
fit2=smooth.spline(age,wage,cv=TRUE)
fit2$df

lines(fit,col ="red",lwd =2)
lines(fit2,col ="blue",lwd =2)
legend("topright",legend =c("16 DF " ,"6.8 DF "),col=c("red","blue"),lty=1,lwd=2,cex=.8)

#######local regression
plot(age,wage,xlim =agelims,cex =.5, col =" darkgrey ")
title("Local Regression")
fit =loess(wage~age,span =.2,data= Wage)
fit2=loess(wage~age,span =.5,data= Wage)
lines(age.grid,predict(fit,data.frame(age=age.grid)),col="red",lwd=2)
lines(age.grid,predict(fit2,data.frame(age=age.grid)),col ="blue",lwd =2)
legend("topright",legend =c("Span =0.2" ,"Span =0.5"),col=c("red","blue"),lty=1,lwd=2,cex=.8)

########GAMs
gam1=lm(wage~ns(year,4)+ns(age,5)+education,data=Wage)

library(gam)
gam.m3=gam(wage~s(year,4)+s(age,5)+education,data= Wage)
summary(gam.m3)
#smoothing spline
par(mfrow=c(1,3))
plot(gam.m3,se=TRUE ,col ="blue")
#预测
preds=predict(gam.m3,newdata=Wage)

#在gam中嵌入local regression
gam.lo=gam (wage~s(year,df =4)+lo(age,span=0.7)+education,data= Wage)
plot(gam.lo,se=TRUE,col ="green")
#第一项是year和age的交叉项
gam.lo.i=gam(wage~lo(year,age,span=0.5)+education,data= Wage)
#查看交叉项
library(akima)
plot(gam.lo.i)

#####logistic回归
par(mfrow =c(1,3))
gam.lr.s=gam(I(wage >250)~year+s(age,df =5)+education,family =
binomial,data=Wage,subset =(education!="1.< HS Grad"))
plot(gam.lr.s,se=T,col="green")













