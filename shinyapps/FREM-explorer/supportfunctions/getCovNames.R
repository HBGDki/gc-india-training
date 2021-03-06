getCovNames <- function(modFile,keepComment=FALSE) {
  
  ## Figure out the FREM covariate names from a FREM model file. It relies on the covariate names added by PsN in the 
  ## FREM code.
  
  # modFile:     The name of the model file
  # keepComment: If FALSE, remove the leading ; and white space in front of the covariate name in the model file. 
  
  ## Returns a list with two components: 
  ##         covNames = the names of the covariates as given in the model file. These corresponds to the covariate column names in the FREM data set created by PsN.
  ##         polyCatCovs = the name of the dichotomized covariates created by PsN
  ##         orgCovNames = original covariate names (removing the frem specific ones)
  
  mod       <- scan(modFile,what="character",sep="\n",quiet=TRUE)
  fremStart <- grep(";;;FREM CODE BEGIN COMPACT",mod)
  fremEnd   <- grep(";;;FREM CODE END COMPACT",mod)
  
  mod1      <- mod[(fremStart+2):(fremEnd-1)]
  covNames  <- mod1[grep(";",mod1)]
  
  if(!keepComment) {
    covNames  <- str_replace(covNames,";\\s*","")
    covNames  <- str_replace(covNames," ","")
  }
  
  orgCovNames <- sort(unique(str_replace(covNames,"_.*","")))
  
  ## Figure out which ones that are poly-categorical, i.e. those with an '_' in the name
  fremCovs  <- covNames[grep("_",x = covNames)]
  
  return(list(covNames=covNames,
              polyCatCovs=fremCovs,
              orgCovNames=orgCovNames)
  )  
  
}
