##### Mono-exponential growth model #####
# Y(t) = Linf - (Linf - L0)*exp(-Kgr * t) + error

MonoExpModel <- function(lnLinf, lnL0, lnKgr, TIME) {
  #
  # lnLinf = log asymptote (length at large TIME)
  # lnL0 = log of length at TIME=0
  # lnKgr = log of growth rate
  # TIME = time
  #

  Linf <- exp(lnLinf)
  L0 <- exp(lnL0)
  Kgr <- exp(lnKgr)

  Linf - (Linf - L0) * exp(-Kgr * TIME)
}

##### Non-linear Length Deceleration model #####
# Y(t) = Linf - (Linf - L0) * exp(-Kgr(t) * t) + error
# Kgr(t) = Kgr0 * (1 - Kgrdec + Kgrdec * exp(-lambda * t))

NLLDmodel <- function(lnLinf, lnL0, lnKgr0, logitKgrdec, lnTTP50, TIME) {
  #
  # lnLinf = log asymptote (length at large TIME)
  # lnL0 = log of length at TIME=0
  # lnKgr0 = log of initial growth rate
  # logitKgrdec = logit of asymptotic fractional reduction in growth deceleration
  #   (at large values of TIME)
  # lnTTP50 = log of the half-life for reaching the asymptotic fractional reduction
  # TIME = time
  #

  Linf <- exp(lnLinf)
  L0 <- exp(lnL0)
  Kgr0 <- exp(lnKgr0)
  Kgrdec <- 1 / (1 + exp(-logitKgrdec))
  lambda <- log(2) / exp(lnTTP50)

  Kgr <- Kgr0 * (1 - Kgrdec + Kgrdec * exp(-lambda * TIME))

  Linf - (Linf - L0) * exp(-Kgr * TIME)
}


##### Non-linear Length Deceleration model, with t* parameterization #####
# Y(t) = Linf - (Linf - L0) * exp(-Kgr(t) * t) + error
# Kgr(t) = Kgr0 * (1 - Kgrdec + Kgrdec * exp(-lambda * t))
# Linf = (Lt.star - L0*exp(-Kgr.star*t.star)) / (1 - exp(-Kgr.star*t.star))
# Kgr.star <- Kgr0*(1-Kgrdec + Kgrdec*exp(-t.star*log(2)/TTP50))

NLLDTstarModel <- function(lnLtstar, lnL0, lnKgr0, logitKgrdec, lnTTP50, TIME, t.star = 24) {
  # lnLtstar = Parameter for length at time t.star (log scale),
  # lnL0 = Length at time 0 (log scale),
  # lnKgr0 = Initial rate of deceleration (log scale),
  # logitKgrdec = asymptotic fractional decrease in rate of deceleration (logit scale),
  # lnTTP50 = Time at 50% deceleration (log scale)
  # TIME = Time, the dependent variable
  # t.star = Time at endpoint of observations (fixed)

  # First, constrain parameters
  Lt.star <- exp(lnLtstar)
  Kgr0 <- exp(lnKgr0)
  L0 <- exp(lnL0)
  Kgrdec <- expit(logitKgrdec)
  TTP50 <- exp(lnTTP50)

  # Kgr.star function (used to determine Linf)
  Kgr.star <- Kgr0 * (1 - Kgrdec + Kgrdec * exp(-t.star * log(2) / TTP50))

  # Linf function
  Linf <- (Lt.star - L0 * exp(-Kgr.star * t.star)) / (1 - exp(-Kgr.star * t.star))

  # Non-linear deceleration rate function
  Kgr <- Kgr0 * (1 - Kgrdec + Kgrdec * exp(-TIME * log(2) / TTP50))

  # Finally, the model
  Linf - (Linf - L0) * exp(-Kgr * TIME)
}
