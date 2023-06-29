##' @import dplyr
truncateCHlivedead = function(indata,
                              k = NULL,
                              compress = FALSE,
                              ragged = FALSE,
                              collapse = FALSE) {
  if (is.null(k))
    stop("You must specify a value for the truncation parameter, k.\n")
  
  
  ## Truncate capture histories
  
  chnew.list =
    apply(indata$chmat,
          1,
          truncateCH1livedead,
          k = k)
  
  
  ## Stack new capture histories
  chmat = do.call("rbind", sapply(chnew.list, "[[", "ch"))
  
  ## Extract release and recapture times and initial times
  release = unlist(sapply(chnew.list, "[[", "release"))
  recapture = unlist(sapply(chnew.list, "[[", "recapture"))
  recovery = unlist(sapply(chnew.list, "[[", "recovery"))
  
  if (compress)
    initial = unlist(sapply(chnew.list, "[[", "initial"))
  else
    initial = rep(1, length(release))
  
  ## Create individual mapping vector
  nrelease = sapply(chnew.list, "[[", "nrelease")
  ind = rep(1:nrow(indata$chmat), nrelease)
  
  ## Add useful rownames
  if (is.null(rownames(indata$chmat)))
    rownames(chmat) = paste(ind, release, sep = ".")
  else
    rownames(chmat) =
    paste(rep(rownames(indata$chmat), nrelease), release, sep = ".")
  
  ## Create output object
  if (collapse) {
    ## Aggregate data if requested
    if (!is.null(indata$other))
      stop("Aggregation is not currently supported when extra covariates are supplied.\n")
    
    df = data.frame(
      chmat = chmat,
      release = release,
      initial = initial,
      recapture = recapture,
      recovery = recovery,
      freq = indata$freq[ind],
      stringsAsFactors = FALSE
    )
    
    vars = c(paste0("chmat.", 0:k + 1),
             "release",
             "initial",
             "recapture",
             "recovery")
    
    output.tmp = df %>%
      group_by_(.dots = vars) %>%
      tally(wt = freq)
    
    output = list(
      chmat = output.tmp[, paste0("chmat.", 0:k + 1)],
      release = output.tmp$release,
      initial = output.tmp$initial,
      recapture = output.tmp$recapture,
      recovery = output.tmp$recovery,
      freq = output.tmp$n,
      aggregated = TRUE
    )
  }
  else{
    output = list(
      chmat = chmat,
      nrelease = length(release),
      release = release,
      initial = initial,
      recapture = recapture,
      recovery = recovery,
      ind = ind,
      freq = indata$freq,
      other = indata$other,
      aggregated = FALSE
    )
  }
  
  class(output) = "spark"
  
  ## Return output
  output
}

truncateCH1livedead = function(chin, k, ragged = FALSE) {
  ## Creates truncated records for a single individual
  
  ## Useful constants
  nocc = length(chin)                # Number of occasions
  
  ## Identify encounters of each type
  capture = which(chin %in% c(1, 3))
  ncapture = length(capture)
  
  recovery = which(chin %in% c(2, 3))
  
  if (length(recovery) == 0)
    recovered = FALSE
  else if (length(recovery == 1))
    recovered = TRUE
  else
    stop("Multiple recoveries in history", chin, ".\n")
  
  ## Identify times of release
  if (capture[ncapture] == nocc)
    ## Individual was last captured on final occasion
    release = capture[-ncapture]
  else
    ## Individual was last captured before final occasion
    release = capture
  
  ## Number of releases
  nrelease = length(release)
  
  ## Identify times of recapture (within k occasions)
  if (capture[ncapture] == nocc)
    ## Individual was last captured on final occasion
    recapture = ifelse(capture[-1] - release <= k, capture[-1],-1)
  else
    ## Individual was last captured before final occasion
    recapture =
    c(ifelse(capture[-1] - release[-nrelease] <= k, capture[-1],-1),-1)
  
  ## Initialize output
  chout = matrix(".", nrelease, nocc)
  
  ## Create new records
  for (j in 1:nrelease) {
    ## Release state
    chout[j, release[j]] = chin[release[j]]
    
    ## Complete the history
    if (recovered && j == nrelease) {
      ## Individual was next recovered dead
      d = recovery - release[j]
      if (d > 0) {
        ## Individual was recovered after occasion of release
        if (d > 1)
          chout[j, release[j] + (1:min(d - 1, k))] = 0
        if (d <= k)
          chout[j, recovery] = chin[recovery]
      }
    }
    else if (recapture[j] < 0) {
      ## Individual was not recaptured within k occasions
      chout[j, (release[j] + 1):min(release[j] + k, nocc)] = 0
    }
    else{
      ## Compute difference between release and recapture
      d = recapture[j] - release[j]
      
      if (d > 1)
        ## Individual was not captured on subsequent occasion
        chout[j, release[j] + (1:(d - 1))] = 0
      
      ## Recapture
      chout[j, release[j] + d] = chin[recapture[j]]
    }
  }
  
  ## Pad recovery vector with -1s
  if(recovered)
    recovery <- c(rep(-1, nrelease - 1),recovery)
  else
    recovery <- rep(-1, nrelease)
  
  ## Return new records
  list(
    nrelease = nrelease,
    ch = chout,
    release = release,
    recapture = recapture,
    recovery = recovery
  )
}
