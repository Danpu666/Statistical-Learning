###########主成分分析
pr.out=prcomp(USArrests,scale=TRUE)
#loading
pr.out$rotation
#得分principal component score vector
pr.out$x
#前两个主成分
biplot(pr.out,scale=0)

#每个主成分的标准差
pr.out$sdev
#贡献率
pr.var=pr.out$sdev^2
pve=pr.var/sum(pr.var )
pve

#画累计贡献率的图
plot(pve,xlab ="Principal Component",ylab="Proportion of
Variance Explained",ylim=c(0,1),type="b")
plot(cumsum(pve),xlab="Principal Component",ylab="
Cumulative Proportion of Variance Explained",ylim=c(0,1),type="b")

##########聚类分析
####k-means
set.seed(2)
x= matrix(rnorm(50*2),ncol =2)
x[1:25,1]= x[1:25,1]+3
x[1:25,2]= x[1:25,2]-4
km.out=kmeans(x,2,nstart=20)
#nstart表示初始化的次数
km.out$cluster

plot(x,col=(km.out$cluster+1),main ="K- Means Clustering
Results with K=2",xlab ="",ylab ="",pch =20,cex =2)

km.out
#tot.withinss表示total within-cluster sum of squares
#withinss表示单独的within-cluster sum of squares

#####层次图
hc.complete=hclust(dist(x),method ="complete")
#画图
plot(hc.complete,main ="Complete Linkage ",xlab ="",sub ="",cex =.9)
#label
cutree (hc.complete,2)

x= matrix(rnorm(30*3),ncol =3)
dd=as.dist(1- cor(t(x)))
plot(hclust(dd,method ="complete"), main ="Complete Linkage
with Correlation - Based Distance ", xlab ="", sub ="")

#####例子
library(ISLR)
nci.labs= NCI60$labs
nci.data= NCI60$data

pr.out=prcomp(nci.data,scale=TRUE)

Cols=function(vec){
cols=rainbow(length(unique(vec)))
return(cols[as.numeric(as.factor(vec))])
}

plot(pr.out$x[,1:2] ,col=Cols(nci.labs),pch=19,xlab ="Z1",ylab =" Z2")

summary(pr.out)
#方差
plot(pr.out)

#方差贡献率
summary(pr.out)$importance[2,]
#累计贡献率
summary(pr.out)$importance[3,]

########对主成分分数进行聚类
hc.out=hclust(dist(pr.out$x[,1:5]) )
plot(hc.out,labels =nci.labs , main =" Hier.Clust.on First
Five Score Vectors")
table(cutree(hc.out,4),nci.labs)






