rm(list=ls())
library(msme)
library(MASS)
library(lattice)
library(sandwich)
library(COUNT)
data(badhealth)
d=badhealth;d
attach(d)

#graph
numvisit
table(numvisit)
plot(table(numvisit),main="GRAPH OF THE DATASET",xlab="NUMBER OF VISIT TO DOCTOR",ylab="COUNTS OF VISITS")

#Poisson Regression
modelp=glm(numvisit~badh+age,family=poisson(link="log"));modelp
summary(modelp)

#predict number of zeros for Poisson
sum(numvisit==0)
preds=predict(modelp,type="response");preds
prop=dpois(0,preds);prop
round(sum(prop))


#Testing for zero inflation
library(vcdExtra)
zero.test(numvisit)

#hurdle at zero poisson model
library(pscl)
hudp=hurdle(numvisit~badh+age,family=poisson(link="log"));hudp
hurdle(numvisit~badh+age)
summary(hudp)

#Over dispersion
mean=apply(d,2,mean);mean
var=apply(d,2,var);var
library(Rfast2)
overdispreg.test(numvisit,matrix(c(badh,age),ncol=2))

#Negative Binomial Regression
modelnb=glm.nb(numvisit~badh+age,data=badhealth);modelnb
summary(modelnb)

#Predict number of zeros for Negative Binomial
pred=predict(modelnb,type="response");pred #estimated means
esttheta=summary(modelnb)$theta;esttheta
prob=esttheta/(esttheta+pred)
prop0=dnbinom(0,esttheta,prob);prop0
round(sum(prop0))

#hurdle at zero Negative Binomial model
hudnb=hurdle(numvisit~badh+age,data=badhealth,dist="negbin");hudnb
summary(hudnb)

#AIC for poisson
extractAIC(modelp)
#AIC for negative binomial
extractAIC(modelnb)
#AIC for hurdle Poisson model
extractAIC(hudp)
#AIC for hurdle Negative Binomial model
extractAIC(hudnb)




