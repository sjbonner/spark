library(RMark)

## Load data set
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
    model = "Multistrata",
    groups = "release",
    begin.time = c(1:30, rep(30, 4))
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
  S = list(formula =  ~ time + stratum),
  p = list(formula =  ~ time + stratum),
  Psi = list(formula =  ~ stratum:tostratum - 1)
)

time.trunc =
  system.time(
    msmodel.trunc = mark(
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
                               parameter = list(
                                 Psi = list(pim.type = "time"),
                                 S = list(pim.type = "time"),
                                 p = list(pim.type = "time")
                               ))

## 3) Run model
time.full =
  system.time(
    msmodel.full = mark(
      msprocessed.full,
      msddl.full,
      model.parameters = model.parameters,
      threads = -1,
      output = FALSE
    )
  )
