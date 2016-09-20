## Functions for the input and output of data in marked's file format.

marked2spark <- function(indata=NULL,infile=NULL,group.df=NULL,covariates=NULL,use.comments=FALSE){

  ## Requires functions from RMark
  if (!requireNamespace("RMark", quietly = TRUE)) {
    stop("RMark is needed for this function to work. Please install it.",call. = FALSE)
  }

    ## Load data from infile if necessary
    if(is.null(indata)){
        if(is.null(infile))
            stop("You must provide either the data or the name of an input file.\n")

    }

    ## Separate input data into (possibly) three components
    ## 1) Matrix of capture histories
    chmat <- RMark::splitCH(indata$ch)

    ## 2) Frequencies
    if(is.null(indata$freq))
        freq <- rep(1,nrow(indata))
    else
        freq <- indata$freq

    ## 3) Other columns
    othernames <- setdiff(colnames(indata),c("ch","freq"))

    if(length(othernames) > 0)
        other <- indata[,othernames,drop=FALSE]
    else
        other <- NULL

    ## Return spark input data list
    return(list(chmat=chmat,
                freq=freq,
                other=other))
}

spark2marked <- function(truncdata,outfile=NULL){
  ## This is actually just an alias for spark2mark.
  spark2mark(truncdata,outfile=NULL)
}


