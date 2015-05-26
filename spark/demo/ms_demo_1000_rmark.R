library(RMark)

## Load data set
infile <- system.file("extdata","msdata1000.inp",package="spark")

## Convert data from mark to spark format
indata <- mark2spark(infile=infile)

## Truncate capture histories
truncdata <- truncateCH(indata,k=5)

## Convert truncated data from spark to mark format
msdata <- spark2mark(truncdata)

## Fit model to truncated data
## 1) Process data
msprocessed.trunc <- process.data(msdata,model="Multistrata",groups="release",begin.time=1:34)

## 2) Build design data
msddl.trunc <- make.design.data(msprocessed.trunc,
                                parameter=list(Psi=list(pim.type="constant"),
                                  S=list(pim.type="constant"),
                                  p=list(pim.type="constant")))

## 3) Fit model
time.trunc <- system.time(msmodel.trunc <- mark(msprocessed.trunc,msddl.trunc,threads=-1))

## Fit model to full data
## 1) Process data
msprocessed.full <- process.data(indata,model="Multistrata")

## 2) Build design data
msddl.full <- make.design.data(msprocessed.full,
                                parameter=list(Psi=list(pim.type="constant"),
                                    S=list(pim.type="constant"),
                                    p=list(pim.type="constant")))

## 3) Run model
time.full <- system.time(msmodel.full <- mark(msprocessed.full,msddl.full,threads=-1))

