## Functions for the input and output of data in MARK's INP file
## format.

mark2spark <-
  function(indata = NULL,
           infile = NULL,
           group.df = NULL,
           covariates = NULL,
           datatype = "recaptures",
           use.comments = FALSE) {
    ## This function is simply a wrapper around marked2spard that allows for data to be read directly from a MARK .inp file.
    
    ## Requires functions from RMark
    if (!requireNamespace("RMark", quietly = TRUE)) {
      stop("RMark is needed for this function to work. Please install it.",
           call. = FALSE)
    }
    
    ## Load data from infile if necessary
     if (is.null(infile))
        stop("You must provide the name of an input file.\n")
      
      ## Load data using convert.inp from RMark
      indata <-
        RMark::convert.inp(infile, group.df, covariates, use.comments)
      
      marked2spark(indata,group.df,covariates,data.type,use.comments)
  }

spark2mark <- function(truncdata, outfile = NULL) {
  ## Convert data from spark internal format to RMark data frame
  
  ## Requires functions from RMark
  if (!requireNamespace("RMark", quietly = TRUE)) {
    stop("RMark needed for this function to work. Please install it.",
         call. = FALSE)
  }
  
  ## Create data frame by:
  ## 1) Collapsing histories
  ## 2) Binding the frequencies, release times, and other variables
  
  markdf <-
    data.frame(
      ch = RMark::collapseCH(as.matrix(truncdata$chmat)),
      initial = truncdata$initial,
      release = truncdata$release,
      stringsAsFactors = FALSE
    )
  
  if (truncdata$aggregated) {
    markdf$freq = truncdata$freq
  }
  else{
    markdf$freq = truncdata$freq[truncdata$ind]
    
    if (!is.null(truncdata$other)) {
      markdf$other = truncdata$other[truncdata$ind,]
      names(markdf)[-(1:4)] = colnames(truncdata$other)
    }
  }
  
  ## Return data frame
  markdf
}
