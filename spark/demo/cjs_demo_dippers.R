## Load RMark
library(RMark)
library(ggplot2)

## Retrieve dipper data from RMark
data(dipper)

## Run spark
k <- 3
dipper.trunc <- spark(dipper,informat="mark",outformat="mark",k=k,ragged=TRUE)

## Process data
dipper.process1 <- process.data(dipper.trunc,
                                model="CJS",
                                begin.time=rep(1:(7-k),2),
                                        #time.intervals=c(1,.5,1,.75,.25,1),
                                groups=c("initial","sex"))

dipper.ddl1 <- make.design.data(dipper.process1)

## Run MARK
timeplussex=list(formula=~time+sex)

system.time(dipper.model1 <- mark(dipper.process1,dipper.ddl1,
                                  model.parameters=list(Phi=timeplussex,
                                      p=timeplussex)))


dipper.model1$results$real

## Original data
dipper.process2 <- process.data(dipper,
                                model="CJS",
                                groups="sex")

dipper.ddl2 <- make.design.data(dipper.process2)
                               
system.time(dipper.model2 <- mark(dipper.process2,dipper.ddl2,
                                  model.parameters=list(Phi=timeplussex,
                                      p=timeplussex)))

dipper.model2$results$real

## Compare point estimates
dipper.results <- data.frame(dipper.model1$results$real[,c(1,3,4)],
                             dipper.model2$results$real[,c(1,3,4)])
colnames(dipper.results) <- c("estimate1","lcl1","ucl1",
                              "estimate2","lcl2","ucl2")
                              
n <- nrow(dipper.results)
jit <- .2

ggplot(dipper.results) +
    geom_point(aes(x=(1:n)-jit,y=estimate1)) +
        geom_point(aes(x=(1:n)+jit,y=estimate2,col="red"))



