calcHAZVector <- calcHAZ <- function(age,basethetas=NULL,covthetas=rep(0,length(basethetas)),etas=rep(0,length(basethetas)),
                          BASE,PLMAX,HLKON,HLKOFF,BASSL,BP,
                          BASECOV=0,PLMAXCOV=0,HLKONCOV=0,HLKOFFCOV=0,BASSLCOV=0,BPCOV=0) {

  ## Computes HAZ based on input.
  
  # age: An age or a vecor of ages at which to compute HAZ.
  # basethetas: A vector with the base thetas 
  # covthetas:  A vector with the ffem, parameter specific covariate effetcs
  # etas: a vecor of individual specific random effects
  ## Will return a vector of HAZ values

  if(!is.null(basethetas)) {
    BASE    <- basethetas[1]          + covthetas[1] + etas[1]
    PLMAX   <- basethetas[2]          + covthetas[2] + etas[2]
    HLKON   <- exp(log(basethetas[3]) + covthetas[3] + etas[3])
    HLKOFF  <- exp(log(basethetas[4]) + covthetas[4] + etas[4])
    BASSL   <- basethetas[5]          + covthetas[5] + etas[5]
    BP      <- exp(log(basethetas[6])      + covthetas[6] + etas[6])
  } else {
    BASE    <- BASE            + BASECOV
    PLMAX   <- PLMAX           + PLMAXCOV
    HLKON   <- exp(log(HLKON)  + HLKONCOV)
    HLKOFF  <- exp(log(HLKOFF) + HLKOFFCOV)
    BASSL   <- BASSL           + BASSLCOV
    BP      <- exp(log(BP)          + BPCOV)
  }
  
  KON  <- log(2)/HLKON
  KOFF <- log(2)/HLKOFF
  
  BASFAC1   <- BASSL*age
  BASFAC2   <- BASSL*BP
  
  PHI      <- age**2.25/(age**2.25 + BP**2.25)
  
  MYBASE   <- BASE + (1-PHI)*BASFAC1 + PHI*BASFAC2
  
  IPRED <-  MYBASE-PLMAX*KON*(exp(-KOFF*age)-exp(-KON*age))/(KON-KOFF)

  return(IPRED)
}
  

