## Load packages
library(RMark)
library(ggplot2)

## Set path to data set
infile = system.file("extdata", "msdata1000.inp", package = "spark")

## Run spark
msdata = spark(infile = infile,
                informat = "mark",
                outformat = "mark")

## Fit model to truncated data
## 1) Process data
msprocessed.trunc =
  process.data(
    msdata,
    model = "Multistrata"
    )

## 2) Build design data
msddl.trunc = make.design.data(msprocessed.trunc,
                                parameters = list(
                                  Psi = list(pim.type = "constant"),
                                  S = list(pim.type = "time"),
                                  p = list(pim.type = "time")
                                ))

## 3) Fit model
model.parameters = list(
  S = list(formula =  ~ time),
  p = list(formula =  ~ time),
  Psi = list(formula =  ~ stratum:tostratum - 1)
)

time.trunc =
  system.time(
    msmodel.trunc <- mark(
      msprocessed.trunc,
      msddl.trunc,
      model.parameters = model.parameters,
      threads = 4,
      output = FALSE
    )
  )

## Fit model to full data
## 1) Process data
msprocessed.full =
  process.data(RMark::convert.inp(infile), model = "Multistrata")

## 2) Build design data
msddl.full = make.design.data(msprocessed.full,
                               parameters = list(
                                 Psi = list(pim.type = "constant"),
                                 S = list(pim.type = "time"),
                                 p = list(pim.type = "time")
                               ))

## 3) Run model
time.full =
  system.time(
    msmodel.full <- mark(
      msprocessed.full,
      msddl.full,
      model.parameters = model.parameters,
      threads = 4,
      output = FALSE
    )
  )

## Compare parameter estimates

# Survival
ms.survival <- rbind(data.frame(Data="Truncated",
                                   x=1:34 -.2,
                                   msmodel.trunc$results$real[1:34,c(1,3,4)]),
                        data.frame(Data="Original",
                                   x=1:34 +.2,
                                   msmodel.full$results$real[1:34,c(1,3,4)]))

ggplot(ms.survival,aes(x,estimate,group=Data,color=Data)) +
  geom_point() +
  geom_errorbar(aes(ymin=lcl,ymax=ucl)) +
  ylim(c(0,1)) +
  xlab("Occasion") + ylab("Survival Probability")

# Capture
ms.capture <- rbind(data.frame(Data="Truncated",
                                x=1:34 -.2,
                                msmodel.trunc$results$real[35:68,c(1,3,4)]),
                     data.frame(Data="Original",
                                x=1:34 +.2,
                                msmodel.full$results$real[35:68,c(1,3,4)]))

ggplot(ms.capture,aes(x,estimate,group=Data,color=Data)) +
  geom_point() +
  geom_errorbar(aes(ymin=lcl,ymax=ucl)) +
  ylim(c(0,1)) +
  xlab("Occasion") + ylab("Capture Probability")

# Transition
ms.transition <- rbind(data.frame(Data="Truncated",
                               x=1:20 -.2,
                               msmodel.trunc$results$real[69:88,c(1,3,4)]),
                    data.frame(Data="Original",
                               x=1:20 +.2,
                               msmodel.full$results$real[69:88,c(1,3,4)]))

ggplot(ms.transition,aes(x,estimate,group=Data,color=Data)) +
  geom_point() +
  geom_errorbar(aes(ymin=lcl,ymax=ucl)) +
  ylim(c(0,1)) +
  xlab("Parameter") + ylab("Transition Probability")
