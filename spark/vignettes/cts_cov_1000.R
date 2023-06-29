## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----echo=FALSE---------------------------------------------------------------
## True parameter values
T=50
beta.p=c(0,0)
beta.phi=c(0,1)
mu.z=rep(0,T)
sigma=c(.5,.2)

truth=c(beta.p,beta.phi,mu.z[-1],sigma[-1])

## ----message=FALSE------------------------------------------------------------
## Load packages
library(spark)
library(RMark)
library(rjags)
library(coda)
library(ggmcmc)

## Load data
data("cts_cov_data")

## -----------------------------------------------------------------------------
## Truncate data
k <- 5
truncdata <- spark(cts_cov_data,informat="spark",outformat="spark",k=k)

## Construct drecap
drecap <- ifelse(truncdata$recapture > 0,
                 truncdata$recapture-truncdata$release,
                 pmin(k,T-truncdata$release))

## -----------------------------------------------------------------------------
## Format covariate matrix
Z <- matrix(NA,nrow=truncdata$nrelease,ncol=k+1)

for(i in 1:truncdata$nrelease){
    Z[i,1] <- truncdata$other[truncdata$ind[i],truncdata$release[i]]

    
    if(truncdata$recapture[i] > 0)
        Z[i,drecap[i]+1] <- truncdata$other[truncdata$ind[i],truncdata$recapture[i]]
}

## -----------------------------------------------------------------------------
## Order data so that recaptures come first
tmp <- which(truncdata$recapture > 0)
index <- c(tmp,(1:truncdata$nrelease)[-tmp])
nrecapture <- length(tmp)

## Construct data list
jags.data <- list(nocc=T,
                  nrecapture=nrecapture,
                  release=truncdata$release[index],
                  nrelease=truncdata$nrelease,
                  drecap=drecap[index],
                  Z=Z[index,],
                  dummy=rep(1,nrow(truncdata$chmat)))

## -----------------------------------------------------------------------------
## Set intial values
jags.inits <- list(beta.phi=c(0,0),
                   beta.p=c(0,0),
                   mu.z=rep(0,T-1),
                   tau.z=1)

## -----------------------------------------------------------------------------
## Run model in JAGS

## This is how the model is run via rjags. However, this can take some time and requires JAGS to be installed, so that the vignette will not pass the CRAN check.
# modelFile <- system.file("JAGS/cts_cov_jags.R",package="spark")
#  
# system.time(jagsModel <- jags.model(modelFile,data=jags.data,inits=jags.inits,n.adapt=1000))
#  
# pars <- c("beta.phi","beta.p","mu.z","sigma.z")
# time.trunc <- system.time(coda.trunc <- coda.samples(jagsModel,pars,n.iter=10000))

## Instead, we can load stored output from a previous run.
load(system.file("extdata","cts_cov_1000_output_trunc.RData",package="spark"))

## ----echo=FALSE---------------------------------------------------------------
## Numerical summaries
summ.trunc <- summary(coda.trunc)

round(cbind(data.frame(Truth=c(beta.p,beta.phi,mu.z[-1],sigma[-1]),
                 ESS=effectiveSize(coda.trunc)),
                 summ.trunc[[1]][,1:2],
                 summ.trunc[[2]][,c(1,3,5)]),2)

## ----echo=FALSE---------------------------------------------------------------
## Traceplots
coda.ggs = ggs(coda.trunc)
ggs_traceplot(coda.ggs,family="beta")

## -----------------------------------------------------------------------------
## Format data to use the same code
k <- T-1
fulldata <- spark(cts_cov_data,informat="spark",outformat="spark",k=k)

## Construct drecap
drecap <- ifelse(truncdata$recapture > 0,
                 truncdata$recapture-truncdata$release,
                 pmin(k,T-truncdata$release))

## Format covariate matrix
Z <- matrix(NA,nrow=truncdata$nrelease,ncol=k+1)

for(i in 1:truncdata$nrelease){
    Z[i,1] <- truncdata$other[truncdata$ind[i],truncdata$release[i]]

    
    if(truncdata$recapture[i] > 0)
        Z[i,drecap[i]+1] <- truncdata$other[truncdata$ind[i],truncdata$recapture[i]]
}

## Order data so that recaptures come first
tmp <- which(truncdata$recapture > 0)
index <- c(tmp,(1:truncdata$nrelease)[-tmp])
nrecapture <- length(tmp)

## Construct data list
jags.data <- list(nocc=T,
                  nrecapture=nrecapture,
                  release=truncdata$release[index],
                  nrelease=truncdata$nrelease,
                  drecap=drecap[index],
                  Z=Z[index,],
                  dummy=rep(1,nrow(truncdata$chmat)))

## Set intial values
jags.inits <- list(beta.phi=c(0,0),
                   beta.p=c(0,0),
                   mu.z=rep(0,9),
                   tau.z=1)

## Run model in JAGS

## This is how the model is run via rjags. However, this can take some time and requires JAGS to be installed, so that the vignette will not pass the CRAN check.
# modelFile <- system.file("JAGS/cts_cov_jags.R",package="spark")
# 
# system.time(jagsModel <- jags.model(modelFile,data=jags.data,inits=jags.inits,n.adapt=1000))
# 
# pars <- c("beta.phi","beta.p","mu.z","sigma.z")
# time.full <- system.time(coda.full <- coda.samples(jagsModel,pars,n.iter=10000))
 
## Instead, we can load stored output from a previous run.
load(system.file("extdata","cts_cov_1000_output_full.RData",package="spark"))

## Numerical summaries
summ.full <- summary(coda.full)

round(cbind(data.frame(Truth=c(beta.p,beta.phi,mu.z[-1],sigma[-1]),
                 ESS=effectiveSize(coda.full)),
                 summ.full[[1]][,1:2],
                 summ.full[[2]][,c(1,3,5)]),2)

