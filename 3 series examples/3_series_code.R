count=500
for (n in count){
  nam <- paste("simdata_3series_pred_",n, sep = "")
  assign(nam, sim.data.pred.func(n))
}

Y<-as.matrix(simdata_3series_pred_500[,1:3])
X.star<-as.matrix(simdata_3series_pred_500[,4:27])

i.count=10
ntrain=n-i.count
Ytrain<-Y[1:ntrain,]
Xtrain<-X.star[1:ntrain,]
Ytest<-Y[(ntrain+1),]
Xtest<-X.star[(ntrain+1),]

STmodel<-tsc.setting(Ytrain,mu=c(1,1,1), #mu: include trend or not
                     rho=c(0.6,0.3,0.1),
                     S=c(100,0,40), #S: number of seasons
                     vrho=c(0,0,0.5), #vrho: decay value of cycle
                     lambda=c(0,0,pi/100))



ki<- c(8,16,24)
pii<- matrix(rep(0.5,dim(Xtrain)[2]),nrow=dim(Xtrain)[2])

#beta
b<-matrix(0,dim(Xtrain)[2])
kapp<-0.01

#v0 and V0 for obs Sigma
R2<-0.8
v0<-5

#State component Sigma
v<-0.01
ss<-0.01

class(STmodel)

set.seed(1)
#train a mbsts model
mbsts.model<-mbsts_function(Ytrain,Xtrain,STmodel,ki,pii,v0=5,mc=400,burn=100)

