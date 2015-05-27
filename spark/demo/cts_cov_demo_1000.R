library(rjags)

## Load data
data("cts_cov_demo_1000")

## Set truncation value
k <- 5

## Run spark
truncdata <- spark(data,informat="spark",outformat="spark",k=k)

## Construct drecap
drecap <- ifelse(truncdata$recapture > 0,
                 truncdata$recapture-truncdata$release,
                 pmin(k,10-truncdata$release))

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
jags.data <- list(nocc=10,
                  nrecapture=nrecapture,
                  release=truncdata$release[index],
                  nrelease=truncdata$nrelease,
                  drecap=drecap[index],
                  Z=Z[index,],
                  dummy=rep(1,nrow(truncdata$chmat)))

jags.inits <- list(beta.phi=c(0,0),
                   beta.p=c(0,0),
                   mu.z=rep(0,9),
                   tau.z=1)

## Run model in JAGS
modelFile <- system.file("JAGS/cts_cov_jags.R",package="spark")

system.time(jagsModel <- jags.model(modelFile,data=jags.data,inits=jags.inits,n.adapt=1000))

pars <- c("beta.phi","beta.p","mu.z","sigma.z")
system.time(coda <- coda.samples(jagsModel,pars,n.iter=1000))

## Summarize output
summ <- summary(coda)
