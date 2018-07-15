#ls() a list of all of the objects
#rm() delete any object
rm(list=ls())

#contour() produce a contour plot等高线
x=seq(1,10)
y=x
f=outer(x,y,function(x,y)cos(y)/(1+x^2))
contour(x,y,f)
fa=(f-t(f))/2
contour(x,y,fa,nlevels=15)
#image()产生热力图
image(x,y,fa)
#persp()产生三维图
persp(x,y,fa)

#读取数据
#第一种方法
auto$mpg
#第二种方法
attach(auto)
mpg

#pair产生一系列散点图
pairs(iris[1:4], main = "Anderson's Iris Data -- 3 species",
      pch = 21, bg = c("red", "green3", "blue")[unclass(iris$Species)])

#identity()识别某一个点

