## Functions for the input and output of data in MARK's INP file
## format.

mark2spark <-
  function(indata = NULL,
           infile = NULL,
           group.df = NULL,
           covariates = NULL,
           datatype = "recaptures",
           use.comments = FALSE) {
    ## Requires functions from RMark
    if (!requireNamespace("RMark", quietly = TRUE)) {
      stop("RMark is needed for this function to work. Please install it.",
           call. = FALSE)
    }
    
    ## Load data from infile if necessary
    if (is.null(indata)) {
      if (is.null(infile))
        stop("You must provide either the data or the name of an input file.\n")
      
      ## Load data using convert.inp from RMark
      indata <-
        RMark::convert.inp(infile, group.df, covariates, use.comments)
    }
    
    ## Separate input data into (possibly) three components
    ## 1) Matrix of capture histories
    if (datatype == "livedead") {
      chmat <- t(sapply(indata$ch,function(h) {
        matrix(as.numeric(strsplit(h,"")[[1]]),ncol = 2,byrow = TRUE) %*% c(1,2)
      }))
    }
    else if(datatype == "recaptures"){
      # We can use RMark's built-in function to split the capture histories
      chmat <- RMark::splitCH(indata$ch)
    }
    else{
      stop("Unknown data type:",datatype,"\n")
    }
    
    ## 2) Frequencies
    if (is.null(indata$freq))
      freq <- rep(1, nrow(indata))
    else
      freq <- indata$freq
    
    ## 3) Other columns
    othernames <- setdiff(colnames(indata), c("ch", "freq"))
    
    if (length(othernames) > 0)
      other <- indata[, othernames, drop = FALSE]
    else
      other <- NULL
    
    ## Return spark input data list
    return(list(
      chmat = chmat,
      freq = freq,
      other = other,
      datatype = datatype
    ))
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
