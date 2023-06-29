## Define aliases for marked/RMark functions so that they will be in the right environment (Thanks DM!)

mysplitCH <- NULL
mycollapseCH <- NULL

.onLoad = function(libname,packagename) {
  ## Ensure that either RMark or marked is available and link to the appropriate functions.
  
  message("Welcome to spark!\n")
  
  if (require("RMark", quietly = FALSE)) {
    message("Using RMark functions.\n")
    mysplitCH <<- RMark::splitCH
    mycollapseCH <<- RMark::collapseCH
  }
  else if(require("marked",quietly = FALSE)) {
    message("Using marked functions.\n")
    mysplitCH <<- marked::splitCH
    mycollapseCH <<- marked::collapseCH
  }
  else{
    stop(
      "Either the marked or RMark package is needed for this function to work. Please install one or the other.",
      call. = FALSE
    )
  }
  
  message("Enjoy.\n")
}
