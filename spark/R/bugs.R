## Function to output data in a format that is easily read into BUGS/JAGS.

spark2bugs <- function(truncdata, outfile = NULL) {
  ## Convert data from spark internal format to a nice R list
  
  ## Requires functions from RMark
  if (!requireNamespace("RMark", quietly = TRUE)) {
    stop("RMark needed for this function to work. Please install it.",
         call. = FALSE)
  }
  
  ## Create list by:
  ## 1) Collapsing and formatting thehistories  
  cat("Note: this procedure will generate several warnings regarding introduction of NAs by coercion. These warnings can be ignored.\n")
  ch = apply(as.matrix(truncdata$chmat), 2, as.numeric)
  
  ## 2) Binding the frequencies, release times, and other variables
  
 bugslist <- list(
    n = nrow(ch),
    k = ncol(ch) - 1,
    ch = ch,
    release = truncdata$release,
    initial = truncdata$initial,
    recapture = truncdata$recapture,
    nrelease = truncdata$nrelease
  )
  
  if (truncdata$aggregated)
    bugslist$freq = truncdata$freq
  else{
    bugslist$freq = truncdata$freq[truncdata$ind]
    
    if (is.null(truncdata$other)) {
      bugslist$freq = truncdata$other[truncdata$ind, ]
    }
  }
  
  ## Return list
  bugslist
}
