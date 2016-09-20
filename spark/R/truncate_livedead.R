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
    recapture = ifelse(capture[-1] - release <= k, capture[-1], -1)
  else
    ## Individual was last captured before final occasion
    recapture =
    c(ifelse(capture[-1] - release[-nrelease] <= k, capture[-1], -1), -1)
  
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
    recapture <- ifelse(capture[-1] - release <= k, capture[-1], -1)
  else
    ## Individual was last captured before final occasion
    recapture <-
    c(ifelse(capture[-1] - release[-nrelease] <= k, capture[-1], -1), -1)
  
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

