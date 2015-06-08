## Load RMark
library(RMark)                          

## Retrieve dipper data from RMark
data(dipper)

## Run spark
dipper.trunc <- spark(dipper,informat="mark",outformat="mark",k=4,ragged=TRUE)

## Process data
dipper.process <- process.data(dipper.trunc,
                               model="CJS",
                               begin.time=rep(1980:1985,2),
                               #time.intervals=c(1,.5,1,.75,.25,1),
                               groups=c("release","sex"))

dipper.ddl <- make.design.data(dipper.process)

## Run MARK
Phi.sexplusage=list(formula=~sex+age)
p.Timeplussex=list(formula=~Time+sex)

system.time(dipper.model1 <- mark(dipper.process,dipper.ddl,
                                  model.parameters=list(Phi=Phi.sexplusage,
                                      p=p.Timeplussex)))

dipper.model1$results$real
