## Functions for the input and output of data in marked's file format.

marked2spark <-
  function(indata = NULL,
           group.df = NULL,
           covariates = NULL,
           datatype = "recaptures",
           use.comments = FALSE) {
    ## Requires functions from marked (preferably) or RMark
    if (!requireNamespace("marked", quietly = TRUE)) {
      if (!requireNamespace("RMark", quietly = TRUE)) {
        stop("Either the marked or RMark package is needed for this function to work. Please install one or the other.",
             call. = FALSE)
      }
    }
    
    ## Load data from infile if necessary
    if (is.null(indata)) {
      stop("No input data provided.\n")
    }
    
    ## Separate input data into (possibly) three components
    ## 1) Matrix of capture histories
    if (datatype == "livedead") {
      chmat <- t(sapply(indata$ch, function(h) {
        matrix(as.numeric(strsplit(h, "")[[1]]),
               ncol = 2,
               byrow = TRUE) %*% c(1, 2)
      }))
    }
    else if (datatype == "recaptures") {
      # We can use RMark's built-in function to split the capture histories
      chmat <- RMark::splitCH(indata$ch)
    }
    else{
      stop("Unknown data type:", datatype, "\n")
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

spark2marked <- function(truncdata, outfile = NULL) {
  ## This is actually just an alias for spark2mark.
  spark2mark(truncdata, outfile = NULL)
}
