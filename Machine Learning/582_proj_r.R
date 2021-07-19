#str=c("C:/Users/bryan79/Documents/FE582/Overall.csv")
str=c("C:/Users/bryan79/Documents/FE582/Overall2.csv")
coviddata= read.csv(file=str,head=T,sep=",")

str2=c("C:/Users/bryan79/Documents/FE582/temp.csv")
pricedata= read.csv(file=str2,head=T,sep=",")

#---- remove %, commas and string 'x'

coviddata$price_2019<-as.numeric(gsub(",","",coviddata$price_2019))
coviddata$price_2020<-as.numeric(gsub(",","",coviddata$price_2020))
coviddata$price_2021<-as.numeric(gsub(",","",coviddata$price_2021))

coviddata$price_change1<-as.numeric(gsub("%","",coviddata$price_change1))
coviddata$price_change2<-as.numeric(gsub("%","",coviddata$price_change2))

coviddata$TEVEBratio_2019 <-as.numeric(gsub("x $","",coviddata$TEVEBratio_2019))
coviddata$TEVEBratio_2020 <-as.numeric(gsub("x $","",coviddata$TEVEBratio_2020))
coviddata$TEVEBratio_2021 <-as.numeric(gsub("x $","",coviddata$TEVEBratio_2021))

coviddata$ratio_change1 <-as.numeric(gsub("x","",coviddata$ratio_change1))
coviddata$ratio_change2 <-as.numeric(gsub("x","",coviddata$ratio_change2))

coviddata$rev_2019 <- as.numeric(gsub(",","",coviddata$rev_2019))
coviddata$rev_2020 <- as.numeric(gsub(",","",coviddata$rev_2020))
coviddata$rev_2021 <- as.numeric(gsub(",","",coviddata$rev_2021))

coviddata$rev_change1<-as.numeric(gsub("%","",coviddata$rev_change1))
coviddata$rev_change2<-as.numeric(gsub("%","",coviddata$rev_change2))

#------- compute percentage change 

coviddata$rev_changep1 <- coviddata$rev_change1/100  #/coviddata$rev_2019
coviddata$rev_changep2 <- coviddata$rev_change2/100  #/coviddata$rev_2020

coviddata$ratio_changep1 <-  coviddata$ratio_change1/coviddata$TEVEBratio_2019
coviddata$ratio_changep2 <-  coviddata$ratio_change2/coviddata$TEVEBratio_2020

coviddata$price_changep1 <-  coviddata$price_change1/100 #/coviddata$price_2019
coviddata$price_changep2 <-  coviddata$price_change2/100  #/coviddata$price_2020

#------- amplify/de-amplify based on previous period change
#--- = delta2 / (1 +/- delta1)
#--- if the value already dropped before previously in 2019-2020
#--- the drop in value 2020-2021 is a weaker indicator due to COVID-19
#
#--- if the value already rised before previously in 2019-2020
#--- the drop in value 2020-2021 is a strong indicator due to COVID-19
amplifyf <- function(p_delta1,p_delta2,v0,v2) {
  storedata=rep(0,length(p_delta1))  # zeros array
  for (i in 1:length(p_delta1)) {
    if (v2[i]<v0[i]) {
      fac=1-p_delta1[i]
    } else {fac=1+p_delta1[i]}
    storedata[i]=p_delta2[i]/fac
  }
  return(storedata)
}

attach(coviddata)
ratio_changep3 <- amplifyf(ratio_changep1,ratio_changep2,TEVEBratio_2019,TEVEBratio_2021)
rev_changep3 <- amplifyf(rev_changep1,rev_changep2,rev_2019,rev_2021)
price_changep3 <- amplifyf(price_changep1,price_changep2,price_2019,price_2021)

# ---- plotting
plot(price_changep3)     # compare plot(price_changep2)
text(price_changep3)
hist(price_changep3,breaks=8)

plot(rev_changep3)
text(rev_changep3)

plot(ratio_changep3)
text(ratio_changep3)

plot(ratio_changep3,rev_changep3)
text(ratio_changep3,rev_changep3,font=1)

#--- K-means clustering (unsupervised learning)
km.out=kmeans(price_changep3,3,nstart=20)  # 3 groups
km.out$cluster
plot(price_changep3,col=(km.out$cluster+1),pch=20)
text(price_changep3,font=1)

#------Hierarchical Clustering-----
hc.average1=hclust(dist(price_changep3),method="average")
par(mfrow=c(1,1))
plot(hc.average1,main="Average Linkage",xlab="",sub="",cex=.9)
grp.avg1=cutree(hc.average1,3)
plot(price_changep3,col=grp.avg1)

hc.average2=hclust(dist(rev_changep3),method="average")
par(mfrow=c(1,1))
plot(hc.average2,main="Average Linkage",xlab="",sub="",cex=.9)
grp.avg2=cutree(hc.average2,3)
plot(rev_changep3,col=grp.avg2)

hc.average3=hclust(dist(ratio_changep3),method="average")
par(mfrow=c(1,1))
plot(hc.average3,main="Average Linkage",xlab="",sub="",cex=.9)
grp.avg3=cutree(hc.average3,3)
plot(ratio_changep3,col=grp.avg3)
text(ratio_changep3)



#------LP-norm---(can't tell positive/negative affected since ABS())
sel_data=data.frame(Company.Name,price_changep3,rev_changep3,ratio_changep3)

lpnorm <- function(x1,x2,p) { # LP norm
  n <- (sum(abs(x2-x1)^p))^(1/p)
  return(n)
}

pair_dist=rep(0,44)  # empty array
x1=c(0,0,0)  # control point
for (i in 1:44) {
  x2=sel_data[i,2:4]
  pair_dist[i]=lpnorm(x1,x2,1) # store lp-norm
}

# top 10 results with max distance from (0,0,0)
y1 <- sort(pair_dist,decreasing=TRUE,index.return=TRUE)
y1$x[1:10] 
y1$ix[1:10]  # which index


#--- Supervised learning (BUT WHAT FOR???? WHY AM I TRAINING DATA????)
plot(price_changep3,rev_changep3)    
text(price_changep3,rev_changep3)
rev_upL=median(rev_changep3[rev_changep3>0])
rev_dwL=median(rev_changep3[rev_changep3<0])
price_upL=median(price_changep3[price_changep3>0])
price_dwL=median(price_changep3[price_changep3<0])
abline(h=rev_upL,col='blue')
abline(h=rev_dwL,col='blue')
abline(v=price_upL,col='red')
abline(v=price_dwL,col='red')
data_group=rep(0,length(price_changep3))
# own define classification
data_group[(price_changep3>price_upL)&(rev_changep3>rev_upL)]=1  # >+ve median classified as 1
data_group[(price_changep3<price_dwL)&(rev_changep3<rev_dwL)]=-1 # <-ve median classified as -1
data_group <- as.factor(data_group)

# KNN
library(class)
train=1:33
train.x=cbind(price_changep3[train],rev_changep3[train]) 
test.x=cbind(price_changep3[-train],rev_changep3[-train])
train.group=cbind(data_group[train])
knn.pred=knn(train.x,test.x,train.group,k=1)  
levels(knn.pred) = c(-1,0,1) 
table(knn.pred,data_group[-train]) # 1 misclassification
knn.pred
data_group[-train]

#LDA
dfr <- data.frame(price_changep3,rev_changep3,data_group)
lda.fit=lda(data_group~price_changep3+rev_changep3,data=dfr,subset=train)
lda.fit
lda.pred=predict(lda.fit,dfr[-train,])
lda.class=lda.pred$class
prediction.lda=cbind(dfr[-train,],lda.class)
table(lda.class,data_group[-train])

#tree
library(tree)
tree.data=tree(data_group~price_changep3+rev_changep3,dfr,subset=train)
summary(tree.data)
plot(tree.data)
text(tree.data)
tree.pred=predict(tree.data,newdata=dfr[-train,],type="class")
data.test=dfr[-train,"data_group"]
table(tree.pred,data.test)
cv1 <- cv.tree(tree.data)
plot(cv1$size,cv1$dev,type="b")
plot(cv1$k,cv1$dev,type="b")
                      
