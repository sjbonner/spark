## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----.message=FALSE------------------------------------------------------
## Load packages
library(spark)
library(RMark)
library(ggplot2)

## Set path to data set
infile = system.file("extdata", "msdata1000.inp", package = "spark")

## ------------------------------------------------------------------------
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
model.parameters = list(
  S = list(formula =  ~ time),
  p = list(formula =  ~ time),
  Psi = list(formula =  ~ stratum:tostratum - 1)
)

## This is how the model is run via RMark. However, this can take some time and requires Program MARK to be installed, so that the vignette will not pass the CRAN check. 
# time.full =
#   system.time(
#     msmodel.full <- mark(
#       msprocessed.full,
#       msddl.full,
#       model.parameters = model.parameters,
#       threads = 4,
#       output = FALSE
#     )
#   )

## Instead, we can load stored output from a previous run.
load(system.file("extdata","msdata1000_output_full.RData",package="spark"))

## ------------------------------------------------------------------------
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

## This is how the model is run via RMark. However, this can take some time and requires Program MARK to be installed, so that the vignette will not pass the CRAN check. 
# time.trunc =
#   system.time(
#     msmodel.trunc <- mark(
#       msprocessed.trunc,
#       msddl.trunc,
#       model.parameters = model.parameters,
#       threads = 4,
#       output = FALSE
#     )
#   )

## Instead, we can load stored output from a previous run.
load(system.file("extdata","msdata1000_output_trunc.RData",package="spark"))


## ---- echo=FALSE---------------------------------------------------------
ms.survival <- rbind(data.frame(Data="Truncated",
                                   x=1:34 -.2,
                                   msmodel.trunc$results$real[1:34,c(1,3,4)]),
                        data.frame(Data="Full",
                                   x=1:34 +.2,
                                   msmodel.full$results$real[1:34,c(1,3,4)]))

ms.survival$ci.width <- ms.survival$ucl - ms.survival$lcl

ggplot(ms.survival,aes(x,estimate,group=Data,color=Data)) +
  geom_point() +
  geom_errorbar(aes(ymin=lcl,ymax=ucl)) +
  ylim(c(0,1)) +
  xlab("Occasion") + ylab("Survival Probability")

## ---- echo=FALSE---------------------------------------------------------
ms.capture <- rbind(data.frame(Data="Truncated",
                                x=1:34 -.2,
                                msmodel.trunc$results$real[35:68,c(1,3,4)]),
                     data.frame(Data="Full",
                                x=1:34 +.2,
                                msmodel.full$results$real[35:68,c(1,3,4)]))

ms.capture$ci.width <- ms.capture$ucl - ms.capture$lcl

ggplot(ms.capture,aes(x,estimate,group=Data,color=Data)) +
  geom_point() +
  geom_errorbar(aes(ymin=lcl,ymax=ucl)) +
  ylim(c(0,1)) +
  xlab("Occasion") + ylab("Capture Probability")

## ---- echo=FALSE---------------------------------------------------------
ms.transition <- rbind(data.frame(Data="Truncated",
                               x=1:20 -.2,
                               msmodel.trunc$results$real[69:88,c(1,3,4)]),
                    data.frame(Data="Full",
                               x=1:20 +.2,
                               msmodel.full$results$real[69:88,c(1,3,4)]))

ms.transition$ci.width <- ms.transition$ucl - ms.transition$lcl

ggplot(ms.transition,aes(x,estimate,group=Data,color=Data)) +
  geom_point() +
  geom_errorbar(aes(ymin=lcl,ymax=ucl)) +
  ylim(c(0,1)) +
  xlab("Parameter") + ylab("Transition Probability")

