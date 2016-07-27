## Load packages
library(RMark)
library(ggplot2)

## Retrieve dipper data from RMark
data(dipper)

## Run spark
k <- 3
dipper.trunc <- spark(dipper,informat="mark",outformat="mark",k=k,compress=TRUE,ragged = FALSE)

## Process data
dipper.process1 <- process.data(dipper.trunc,
                                model="CJS",
                                begin.time = rep(1:6,2),
                                groups=c("initial","sex"))

dipper.ddl1 <- make.design.data(dipper.process1)

## Run MARK
timebysex=list(formula=~time*sex)

system.time(dipper.model1 <- mark(dipper.process1,dipper.ddl1,
                                  model.parameters=list(Phi=timebysex,
                                      p=timebysex)))


dipper.model1$results$real

## Original data
dipper.process2 <- process.data(dipper,
                                model="CJS",
                                groups="sex")

dipper.ddl2 <- make.design.data(dipper.process2)
                               
system.time(dipper.model2 <- mark(dipper.process2,dipper.ddl2,
                                  model.parameters=list(Phi=timebysex,
                                      p=timebysex)))

dipper.model2$results$real

## Compare point estimates and 95% confidence intervals

## Construct data frame for ggplot2
Occasion=rep(1:6,4)
Sex=rep(rep(c("Female","Male"),c(6,6)),2)

dipper.results <- rbind(data.frame(Data="Truncated",
                                   x=Occasion -.2,
                                   Sex=Sex,
                                   dipper.model1$results$real[-c(7:8,15:16,23:24,31:32),c(1,3,4)]),
                        data.frame(Data="Original",
                                   x=Occasion +.2,
                                   Sex=Sex,
                                   dipper.model2$results$real[,c(1,3,4)]))
                        
# Survival
survindex = grep("^Phi",rownames(dipper.results))

ggplot(dipper.results[survindex,],aes(x,estimate,group=Data,color=Data)) + 
  geom_point() +
  geom_errorbar(aes(ymin=lcl,ymax=ucl)) + 
  facet_grid(~ Sex) +
  ylim(c(0,1)) +
  xlab("Occasion") + ylab("Survival Probability")

# Capture
capindex = grep("^p",rownames(dipper.results))

ggplot(dipper.results[capindex,],aes(x,estimate,group=Data,color=Data)) + 
  geom_point() +
  geom_errorbar(aes(ymin=lcl,ymax=ucl)) + 
  facet_grid(~ Sex) +
  ylim(c(0,1)) +
  xlab("Occasion") + ylab("Capture Probability")


