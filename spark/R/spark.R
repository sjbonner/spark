spark <- function(indata=NULL,infile=NULL,informat="spark",
                  outfile=NULL,outformat="spark"){

    ## Format input data if necessary

    ## Truncate capture histories

    ## Format output data if necessary
    
}

truncateCH1 <- function(chin,k){
    ## Creates truncated records for a single individual

    ## Useful constants
    nocc <- length(chin)                # Number of occasions
    
    ## Identify captures
    capture <- which(chin>0)
    ncapture <- length(capture)

    ## Identify times of release
    if(capture[ncapture] == nocc)
        ## Individual was last captured on final occasion
        release <- capture[-ncapture]
    else
        ## Individual was last captured before final occasion
        release <- capture
    nrelease <- length(release)

    ## Identify times of recapture (within k occasions)
    if(capture[ncapture] == nocc)
        ## Individual was last captured on final occasion
        recapture <- ifelse(capture[-1]-release <= k,capture[-1],-1)
    else
        ## Individual was last captured before final occasion
        recapture <- c(ifelse(capture[-1]-release[-nrelease] <=k,capture[-1],-1),-1)

    ## Initialize output 
    chout <- matrix(".",nrelease,k+1)

    ## Add release states in first column
    chout[,1] <- chin[release]
    
    ## Create records for all but the final release
    for(j in 1:nrelease){
        if(recapture[j] < 0){
            ## Individual was not recaptured within k occasions
            chout[j,2:(min(k+1,nocc-release[j]+1))] <- 0
        }
        else{
            ## Compute difference between release and recapture
            d <- recapture[j] - release[j]

            if(d==0)
                ## Individual was recaptured on subsequent occasion
                chout[j,2] <- chin[recapture[j]]
            else{
                ## Individual was recaptured on occasion t1+2,...,t1+k
                chout[j,2:d] <- 0
                chout[j,d+1] <- chin[recapture[j]]
            }
        }
    }

    ## Return new records
    list(nrelease=nrelease,ch=chout,release=release,recapture=recapture)
}

    
truncateCH <- function(indata,k=NULL){

    if(is.null(k))
        stop("You must specify a value for the truncation parameter, k.\n")

    
    ## Truncate capture histories
    chnew.list <- apply(indata$chmat,1,truncateCH1,k=k)

    ## Stack new capture histories
    chmat <- do.call("rbind",sapply(chnew.list,"[[","ch"))

    ## Extract release and recapture times
    release <- unlist(sapply(chnew.list,"[[","release"))
    recapture <- unlist(sapply(chnew.list,"[[","recapture"))
    
    ## Create individual mapping vector
    nrelease <- sapply(chnew.list,"[[","nrelease")
    ind <- rep(1:nrow(indata$chmat),nrelease)

    ## Add useful rownames
    if(is.null(rownames(indata$chmat)))
        rownames(chmat) <- paste(ind,release,sep=".")
    else
        rownames(chmat) <- paste(rep(rownames(indata$chmat),nrelease),release,sep=".")
        
    ## Create output object
    output <- list(chmat=chmat,
                   nrelease=length(release),
                   release=release,
                   recapture=recapture,
                   ind=ind,
                   freq=indata$freq,
                   other=indata$other)
    class(output) <- "spark"

    ## Return output
    output
}
