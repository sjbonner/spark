## Functions for the input and output of data in MARK's INP file
## format.

mark2spark <- function(indata=NULL,infile=NULL,group.df=NULL,covariates=NULL,use.comments=FALSE){

    ## Requires functions from RMark
    if (!requireNamespace("RMark", quietly = TRUE)) {
        stop("RMark needed for this function to work. Please install it.",call. = FALSE)
    }

    ## Load data from infile if necessary
    if(is.null(indata)){
        if(is.null(infile))
            stop("You must provide either the data or the name of an input file.\n")

        ## Load data using convert.inp from RMark
        indata <- RMark::convert.inp(infile,group.df,covariates,use.comments)
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

spark2mark <- function(truncdata,outfile=NULL){
    ## Convert data from spark internal format to RMark data frame

    ## Requires functions from RMark
    if (!requireNamespace("RMark", quietly = TRUE)) {
        stop("RMark needed for this function to work. Please install it.",call. = FALSE)
    }

    ## Create data frame by:
    ## 1) Collapsing histories
    ## 2) Binding the frequencies, release times, and other variables

    if(is.null(truncdata$other))
        markdf <- data.frame(ch=RMark::collapseCH(as.matrix(truncdata$chmat)),
                             truncdata$release,
                             truncdata$freq[truncdata$ind],
                             stringsAsFactors=FALSE)
    else
        markdf <- data.frame(ch=RMark::collapseCH(as.matrix(truncdata$chmat)),
                             truncdata$release,
                             truncdata$freq[truncdata$ind],
                             truncdata$other[truncdata$ind,],
                             stringsAsFactors=FALSE)

    ## Add column names
    colnames(markdf) <- c("ch","release","freq",
                          colnames(truncdata$other))

    ## Return data frame
    markdf
}
    
    

    
    
