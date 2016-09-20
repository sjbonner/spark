## Functions for the input and output of data in MARK's INP file
## format.

mark2spark <-
  function(indata = NULL,
           infile = NULL,
           group.df = NULL,
           covariates = NULL,
           datatype = "recaptures",
           use.comments = FALSE) {
    ## This function is simply a wrapper around marked2spark that allows for data to be read directly from a MARK .inp file.
    
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
      
      marked2spark(indata,group.df,covariates,datatype,use.comments)
  }

spark2mark <- function(truncdata) {
  ## For now just a wrapper
  
  ## Requires functions from RMark
  if (!requireNamespace("RMark", quietly = TRUE)) {
    stop("RMark is needed for this function to work. Please install it.",
         call. = FALSE)
  }
  
  ## Format data
  spark2marked(truncdata)
}
