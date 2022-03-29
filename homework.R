studentnumber = 819511
fulldata = read.table("dataHW.txt",header=T)
set.seed(studentnumber)
rownumbers = sample(1:nrow(fulldata),600,replace=F)
mydata = fulldata[rownumbers,]

attach(mydata)
plot(mydata)
hist(mydata$Y)
library(glmulti)
#AIC
fullmodel=glm(Y~X1+X2+X3+X4+X5,data=mydata,family = poisson(link="log"))
allfits1 <- glmulti(fullmodel,level = 2,crit = "aic")
weightable(allfits1)[1:3,]
#BIC
allfits2 <- glmulti(fullmodel,level = 2,crit = "bic")
weightable(allfits2)[1:3,]

library(fic)
wide.mydata <- glm(Y~.^2-X2:X3-X3:X5,data=mydata,family = poisson(link="log"))
focus1 <- function(par,X) exp((X %*% par))
inds0 <- c(1,rep(0,13))
combs <- all_inds(wide.mydata,inds0)
X.all <- model.matrix(wide.mydata)
X <- colMeans(X.all)
#focus 1
fic1 <- fic(wide=wide.mydata,inds = combs,inds0 = inds0,focus = focus1,X=X)
best1 <- order(fic1$rmse.adj)[1:3]
fic1[best1,]

#focus 2
focus2 = function(par, X) 1-ppois(8,lambda=exp(X %*% par))
fic2 = fic(wide=wide.mydata,inds=combs,inds0=inds0,focus=focus2,X=X.all)
best2 <- order(fic2$rmse.adj)[1:3]
fic2[best2,]


