spark <- function(indata=NULL,infile=NULL,informat="spark",
                  outfile=NULL,outformat="spark",k=5,ragged=FALSE,...){
    
    ## Format input data if necessary
    if(informat=="mark")
        indata <- mark2spark(indata,infile,...)
    else if(informat!="spark")
        stop("Sorry, I do not recognize that informat.\n")
    
    ## Remove histories that do not contribute to likelihood
    first <- apply(indata$chmat,1,function(w) min(which(w>0)))
    invalid <- which(first==ncol(indata$chmat))

    if(length(invalid) > 0){
        warning("Removing ",length(invalid)," individuals first released on occasion ",ncol(indata$chmat),".\n")
        
        indata$chmat <- indata$chmat[-invalid,]
        indata$freq <- indata$freq[-invalid]

        othernames <- colnames(indata$other)
        indata$other <- as.matrix(indata$other[-invalid,])
        colnames(indata$other) <- othernames
    }

    ## Check value of k
    if(k > (ncol(indata$chmat)-1)){
        k <- ncol(indata$chmat)-1
        warning("The value of k must be less than the number of capture occasions. Setting k=",k,". This will reproduce the full model with no truncation.\n")
    }
    
    ## Truncate capture histories
    outdata <- truncateCH(indata,k=k,ragged=ragged)
    
    ## Format output data if necessary
    if(outformat=="mark")
        return(spark2mark(outdata,outfile))
    else if(outformat!="spark")
        stop("Sorry, I do not recognize that outformat.\n")
    else
        return(outdata)
}

truncateCH1 <- function(chin,k,ragged=FALSE){
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

    ## Compute padding values if requested
    if(ragged)
        pad <- pmax(k+release-nocc,0)
    else
        pad <- rep(0,nrelease)

    ## Add release states in first column
    chout[cbind(1:nrelease,pad+1)] <- chin[release]
    
    ## Create records for all but the final release
    for(j in 1:nrelease){
        if(ragged)
            ## Add padding
            if(pad[j]>0)
                chout[j,1:pad[j]] <- 0
        
        if(recapture[j] < 0){
            ## Individual was not recaptured within k occasions
            chout[j,(2+pad[j]):(min(k+1,nocc-release[j]+1+pad[j]))] <- 0
        }
        else{
            ## Compute difference between release and recapture
            d <- recapture[j] - release[j]

            if(d>1)
                ## Individual was not captured on subsequent occasion
                chout[j,pad[j] + (2:d)] <- 0

            ## Recapture
            chout[j,d+1+pad[j]] <- chin[recapture[j]]
        }
    }

    ## Return new records
    list(nrelease=nrelease,
         ch=chout,
         release=release,
         recapture=recapture)
}

    
truncateCH <- function(indata,k=NULL,ragged=FALSE){

    if(is.null(k))
        stop("You must specify a value for the truncation parameter, k.\n")

    
    ## Truncate capture histories
    chnew.list <- apply(indata$chmat,1,truncateCH1,k=k,ragged=ragged)

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
