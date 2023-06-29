#' @import dplyr
truncateCHrecaptures = function(indata,
                                k = NULL,
                                compress = FALSE,
                                ragged = FALSE,
                                collapse = FALSE) {
  if (is.null(k))
    stop("You must specify a value for the truncation parameter, k.\n")
  
  
  ## Truncate capture histories
  if (compress)
    chnew.list =
      apply(indata$chmat,
            1,
            truncateCH1recapturescompress,
            k = k,
            ragged = ragged)
  else
    chnew.list =
      apply(indata$chmat,
            1,
            truncateCH1recaptures,
            k = k)
  
  
  ## Stack new capture histories
  chmat = do.call("rbind", sapply(chnew.list, "[[", "ch"))
  
  ## Extract release and recapture times and initial times
  release = unlist(sapply(chnew.list, "[[", "release"))
  recapture = unlist(sapply(chnew.list, "[[", "recapture"))
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
      freq = indata$freq[ind],
      stringsAsFactors = FALSE
    )
    
    vars = c(paste0("chmat.", 0:k + 1), "release", "initial", "recapture")
    
    output.tmp = df %>%
      group_by_(.dots = vars) %>%
      tally(wt = freq)
    
    output = list(
      chmat = output.tmp[, paste0("chmat.", 0:k + 1)],
      release = output.tmp$release,
      initial = output.tmp$initial,
      recapture = output.tmp$recapture,
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

truncateCH1recaptures = function(chin, k, ragged = FALSE) {
  ## Creates truncated records for a single individual
  
  ## Useful constants
  nocc = length(chin)                # Number of occasions
  
  ## Identify captures
  capture = which(chin > 0)
  ncapture = length(capture)
  
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
    if (recapture[j] < 0) {
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
  
  ## Return new records
  list(
    nrelease = nrelease,
    ch = chout,
    release = release,
    recapture = recapture
  )
}


truncateCH1recapturescompress <- function(chin, k, ragged = FALSE) {
  ## Creates truncated records for a single individual
  ## Compresses capture histories to length k and tracks initial occasion
  
  ## Useful constants
  nocc <- length(chin)                # Number of occasions
  
  ## Identify captures
  capture <- which(chin > 0)
  ncapture <- length(capture)
  
  ## Identify times of release
  if (capture[ncapture] == nocc)
    ## Individual was last captured on final occasion
    release <- capture[-ncapture]
  else
    ## Individual was last captured before final occasion
    release <- capture
  nrelease <- length(release)
  
  ## Identify times of recapture (within k occasions)
  if (capture[ncapture] == nocc)
    ## Individual was last captured on final occasion
    recapture <- ifelse(capture[-1] - release <= k, capture[-1],-1)
  else
    ## Individual was last captured before final occasion
    recapture <-
    c(ifelse(capture[-1] - release[-nrelease] <= k, capture[-1],-1),-1)
  
  ## Initialize output
  chout <- matrix(".", nrelease, k + 1)
  
  ## Compute padding values if requested
  if (ragged)
    pad <- pmax(k + release - nocc, 0)
  else
    pad <- rep(0, nrelease)
  
  ## Add release states in first column
  chout[cbind(1:nrelease, pad + 1)] <- chin[release]
  
  ## Create records for all but the final release
  for (j in 1:nrelease) {
    if (ragged)
      ## Add padding
      if (pad[j] > 0)
        chout[j, 1:pad[j]] <- 0
      
      if (recapture[j] < 0) {
        ## Individual was not recaptured within k occasions
        chout[j, (2 + pad[j]):(min(k + 1, nocc - release[j] + 1 + pad[j]))] <-
          0
      }
      else{
        ## Compute difference between release and recapture
        d <- recapture[j] - release[j]
        
        if (d > 1)
          ## Individual was not captured on subsequent occasion
          chout[j, pad[j] + (2:d)] <- 0
        
        ## Recapture
        chout[j, d + 1 + pad[j]] <- chin[recapture[j]]
      }
  }
  
  ## Identify time of initial capture occasion in truncated history
  if (ragged)
    initial <- release - pad
  else
    initial <- release
  
  
  ## Return new records
  list(
    nrelease = nrelease,
    ch = chout,
    release = release,
    initial = initial,
    recapture = recapture
  )
}
