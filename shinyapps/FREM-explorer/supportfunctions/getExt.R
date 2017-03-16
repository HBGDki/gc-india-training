getExt <- function(extFile,set=NULL) {
  
  ## Extracts the relevant part of an extFile
  
  # extFile:  The name of the ext file to read
  # set:      The set of ext results to extract. (With multiple $ESTs tehre may be several.) The default is the last set of ext-results
  
  tmp   <- scan(extFile,what="character",sep="\n",quiet=TRUE)
  tabs  <- grep("TABLE",tmp)
  set   <- if(is.null(set)) length(tabs)
  
  if(set==1 & length(tabs)==1) { # Only one set of results
    myext <- read.table(extFile,skip=1,h=T)
  } else if(set== 1 & length(tabs)>1) {
    myext <- read.table(extFile,skip=1,nrows=tabs[2]-3,h=T)
  } else if(set==2 & length(tabs)==2) {
    myext <- read.table(extFile,skip=tabs[2],h=T)
  } else if(set==2 & length(tabs)==3) {
    myext <- read.table(extFile,skip=tabs[2],nrows=length(tmp)-tabs[2]-(length(tmp)-tabs[3])-2,h=T)
  } else if(set==4 & length(tabs)==4) {
    myext <- read.table(extFile,skip=tabs[4],h=T)
  }
  
  return(myext)
}
