## Function to output as an R list.

spark2list <- function(truncdata,outfile=NULL){
    ## Convert data from spark internal format to a nice R list

    ## Requires functions from RMark
    if (!requireNamespace("RMark", quietly = TRUE)) {
        stop("RMark is needed for this function to work. Please install it.",call. = FALSE)
    }

    ## Create list by:
    ## 1) Collapsing histories
    ## 2) Binding the frequencies, release times, and other variables

    if(is.null(truncdata$other))
       output <- list(ch=RMark::collapseCH(as.matrix(truncdata$chmat)),
                      release=truncdata$release,
                      initial=truncdata$initial,
                      freq=truncdata$freq[truncdata$ind],
                      other=truncdata$other[truncdata$ind,])

    ## Return list
    output
}





