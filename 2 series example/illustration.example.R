#The mbsts package is available on the CRAN at 
#https://cran.r-project.org/web/packages/mbsts/

#please first run sim.data.R file

#Two target series
Y<-as.matrix(data[,1:m])
#Sixteen candidate predictors
X.star.single<-as.matrix(data[,(m+1):dim(data)[2]])
X.star<-cbind(X.star.single, X.star.single) 

#split dataset into training set and test set
n=dim(Y)[1]
ntrain=n-5
Ytrain<-Y[1:ntrain,]
Xtrain<-X.star[1:ntrain,]
Ytest<-Y[(ntrain+1):n,]
Xtest<-X.star[(ntrain+1):n,]

set.seed(1)
#Specify time series components
STmodel<-tsc.setting(Ytrain,mu=c(1,1),rho=c(0.06,0.08),S=c(100,0),vrho=c(0,0.99),lambda=c(0,pi/100))

class(STmodel)

#prior parameters setting 
ki<- c(8,dim(Xtrain)[2])
pii<- matrix(rep(0.5,dim(Xtrain)[2]),nrow=dim(Xtrain)[2])

set.seed(1)
#train a mbsts model
mbsts.model<-mbsts_function(Ytrain,Xtrain,STmodel,ki,pii,v0=5,mc=400,burn=100)

class(mbsts.model)



#title for each plot
title_new<-c("Inclusion Probabilities for y1", "Inclusion Probabilities for y2")

#rename predictors
varnames_new<-c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8")

#plot inclusion probability 
plot_prob(object=mbsts.model,title=title_new,prob.threshold=0.8,varnames=varnames_new)

#Generate feature selection and parameter estimation results
para.est(object=mbsts.model,prob.threshold=0.8)

#Convergence diagnosis
plot_cvg(object=mbsts.model,index=1,main="Predictor #1")

#title vector for each plot
title_new<-c("Posterior State Components of y1", "Posterior State Components of y2")

#plot state components
#all components
plot_comp(object=mbsts.model, slope=c(T,T),local=c(T,T),season=c(100,0),cyc=c(F,T),title=title_new,component_selection="All")
#individual components
title_new<-c("Posterior State Component of y1", "Posterior State Component of y2")
plot_comp(object=mbsts.model, slope=c(T,T),local=c(T,T),season=c(100,0),cyc=c(F,T),title=title_new,component_selection="Cycle")
plot_comp(object=mbsts.model, slope=c(T,T),local=c(T,T),season=c(100,0),cyc=c(F,T),title=title_new,component_selection="Seasonal")

set.seed(1)
#make a 5-steps prediction
output<-mbsts.forecast(mbsts.model,STmodel,newdata=Xtest,steps=5)
output$pred.mean