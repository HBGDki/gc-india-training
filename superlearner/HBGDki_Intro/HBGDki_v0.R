#;;;;;;;;;;;;;;;;;;;;;;;;;MODEL NOTES;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
# HBGD Knowledge Integration Group  (BMGF)                                                  ;
# Shasha Jumbe                                                                             ;
# script created: 20140615                                                                  ;
# script last modified: 20140731                                                            ;
#------------------*************************------------------------------------------------;
#
#
#########################################################
## Clean up workspace, set flags and set ROOT directory
#########################################################
#
# This logical flag (TRUE or FALSE) removes all data from the workspace
REMOVE=TRUE

# Clear workspace if requested
if (REMOVE) {remove(list=ls())}

#
# This logical flag (TRUE or FALSE) indicates if the user would like 
# to exports a PDF of plots in this script
PDF = FALSE

# This logical flag (TRUE or FALSE) indicates if the user would like to
# export the merged datafile
EXPORT = TRUE

# Define a description of the project directory location
shortname <- "~Projects/HBGDki/"

# Create a time stamp for when the script is executed
TimeStamp=format(Sys.time(),"%Y%m%d %H%M%S")

# Define the working directory (that is, the location of this script
# and the data)
dir="C:\\Projects\\HBGDki\\"
#dir = "~/Projects/svn-proj-BMG100100/script/"

# Set the working directory to 'dir'
setwd(dir)

# Run script to create functions to define the models
source('ModelFunctions.r')

# Define the logit and inverse-logit (aka expit) functions
logit = function(x) { log(x/(1-x)) }
expit = function(x) { 1/(1+exp(-x)) }

# Open a PDF file for saving plots if requested
if (PDF) {pdf(paste(TimeStamp,"HBGDkiExample.test.pdf",sep=""))}

#
#    													 
#------------------INSTALL PACKAGES------------------------------------------------;
#
# (1) The following R packages are required:
#    foreign, ggplot2, Hmisc, MASS, metrumrg, nlme, plyr, reshape, tools, mgcv
#
#Run this the first time only- and comment out after all packages installed
install.packages("metrumrg")
install.packages("Hmisc")
install.packages("lattice")
install.packages("ggplot2")
install.packages("foreign")
install.packages("MASS")
install.packages("nlme")
install.packages("plyr")
install.packages("reshape")
install.packages("mgcv")
#      												 
#------------------END INSTALL PACKAGES------------------------------------------------;
#comment out above after first time to avoid errors and R re-start requirement
#
library(metrumrg)
library(Hmisc)
library(lattice)
library(ggplot2) 
library(foreign)
library(MASS)
library(nlme)
library(reshape)
library(plyr)
library(mgcv)
#
#      												 
#------------------END LIBRARY CALLS------------------------------------------------;
#
#
#      												 
#------------------IMPORT & PLOT DATA------------------------------------------------;
#

# Read the data
mydata <- sasxport.get("sampleHBGDki.xpt")

# Subset to keep the data that we'll use and re-name variables
d <- mydata[,c(4:7,3)]
names(d) = c("ID","SEX","AGE","HEIGHT","SITE")
d$SEXF <- factor(d$SEX,levels=1:2, labels=c('Male','Female'))

# Look at the first few rows of the data
head(d)

# Output the data if requested
if (EXPORT) {
  write.table(d, file=paste("HBGDki_samp.csv", sep=""),row.names=FALSE, append=FALSE,sep=",",quote=F)  
}
#

# Plot the height vs. age data by child.  
# - overlay a loess smooth as a heavy red line
g <- ggplot(d, aes(x = AGE, y = HEIGHT)) 
g + geom_line(aes(group = ID),alpha=0.5) + 
  geom_smooth(aes(group = 1), colour = 'red', size = 1,method='gam', formula=y~s(x)) 


# Plot height vs. age by child and panelled by sex
ggplot(data=d,aes(x=AGE,y=HEIGHT,group=ID,col=SEXF)) + geom_line() +
  facet_grid(.~SEXF)

#
#------------------END IMPORT & PLOT DATA------------------------------------------------;
#

#
#------------------FIT MODELS TO THE DATA------------------------------------------------;
#

#
##----- Fit base model with no declining Kgr -----##
#
#

# Fit exponential decay model to the data
#  Y = Linf - (Linf - L0)*exp(-Kgr * AGE) + error
#
# The parameters Linf, L0 and Kgr are subject-specific.
#  - Linf is the estimated asymptotic length (at AGE = Infinity)
#  - L0 is the estimated length at AGE=0
#  - Kgr is the rate at which length reaches the asymptote (Linf)
#
# These parameters are estimated on the natural log scale to
# (1) force them to be positive
# (2) incorporate log-normal subject-specific random effects. 
#     for example: lnLinf ~ N(mu_lnLinf, omega_lnLinf)

fit1 <- nlme(HEIGHT ~ MonoExpModel(lnLinf, lnL0, lnKgr, AGE),
             random=list(ID=lnLinf+lnL0+lnKgr~1),
             fixed=lnLinf+lnL0+lnKgr~1,
             start=c(lnLinf=log(130),lnL0=log(50),lnKgr=log(0.01)),
             data=d,
             na.action=na.omit)

# Extract the estimated fixed effects (on the original scale)
fit1.params = exp(fixed.effects(fit1))
print(fit1.params)
#     lnLinf        lnL0       lnKgr 
#127.2269258  54.6459108   0.0278215

# Print the AIC for this model
AIC(fit1)
#1083.4

# Show the model information and parameter estimates for all parameters
print(summary(fit1))

#
# Nonlinear mixed-effects model fit by maximum likelihood
# Model: HEIGHT ~ MonoExpModel(lnLinf, lnL0, lnKgr, AGE) 
#Data: d 
#AIC      BIC    logLik
#1083.4 1116.966 -531.7001
#
#Random effects:
#  Formula: list(lnLinf ~ 1, lnL0 ~ 1, lnKgr ~ 1)
#Level: ID
#Structure: General positive-definite, Log-Cholesky parametrization
#StdDev     Corr         
#lnLinf   0.04487245 lnLinf lnL0  
#lnL0     0.03629248  0.739       
#lnKgr    0.19157252 -0.714 -0.903
#Residual 2.57612680              
#
#Fixed effects: lnLinf + lnL0 + lnKgr ~ 1 
#Value  Std.Error  DF  t-value p-value
#lnLinf  4.845972 0.01436015 190 337.4597       0
#lnL0    4.000874 0.01011746 190 395.4425       0
#lnKgr  -3.581946 0.05810190 190 -61.6494       0
#Correlation: 
#  lnLinf lnL0  
#lnL0   0.575       
#lnKgr -0.788 -0.774
#
#Standardized Within-Group Residuals:
#  Min          Q1         Med          Q3         Max 
#-2.48914463 -0.52740444  0.00836269  0.55448492  2.72773668 
#
#Number of Observations: 212
#Number of Groups: 20 


### Model diagnostic plots

# Plot the individual level (aka level 1) residuals vs. fitted values
plot(fit1)

# Make a similar plot but overlay a smoothing spline to look for trends
plot(fitted(fit1,level=1), residuals(fit1,level=1,type='pearson'),
     xlab = "Individual-level Fitted Values", ylab = "Individual-level standardized residuals")
abline(h=0, lty=2)
lines(smooth.spline(fitted(fit1,level=1), 
                    residuals(fit1,level=1,type='pearson'))
      )
# Clearly some issues with the residuals and correlations between parameters

# Plot the population level (aka level 0) residuals vs. fitted values
plot(fitted(fit1,level=0), residuals(fit1,level=0,type='pearson'),
     xlab = "Population-level Fitted Values", ylab = "Population-level Standardized Residuals")
abline(h=0, lty=2)
lines(smooth.spline(fitted(fit1,level=0), 
                    residuals(fit1,level=0,type='pearson'))
      ) 
# Clearly some issues with the residuals and correlations between parameters


# Plot Individual-level residuals vs. Age
plot(d$AGE, residuals(fit1,level=1,type='pearson'),
     xlab = "Age (months)", ylab = "Individual-level Standardized Residuals")
abline(h=0, lty=2)
lines(smooth.spline(d$AGE, residuals(fit1,level=1,type='pearson'))) 



#
# Check for residual pattern within groups and difference between groups      
xyplot(residuals(fit1) ~ fitted(fit1) | d$SEXF, main = "Fixed Kgr Model Residuals - by SEX",
       panel=function(x, y){ 
         panel.xyplot(x, y) 
         panel.loess(x, y, span = 0.75) 
         panel.lmline(x, y, lty = 2)  # Least squares broken line
       } 
)
# We see similar patterns of residuals


# Calculate empirical derivatives (dldt = change in length / change in time)
deltas <- lapply(split(d,d$ID), function(dd) {
  p = nrow(dd)
  res <- data.frame(ID=dd$ID[1:(p-1)],
                    dldt=diff(dd$HEIGHT)/diff(dd$AGE), 
                    tmean = (dd$AGE[1:(p-1)]+dd$AGE[2:p])/2,
                    obsmean=(dd$HEIGHT[1:(p-1)]+dd$HEIGHT[2:p])/2)
  return(res)
})
deltas <- do.call(rbind,deltas)

# Calculate model-predicted derivative (dldt)
# For the mono-exponential model this is:
# dldt = Kgr * (Linf - L0) * exp(-Kgr * AGE)

pred.fit1 = data.frame(AGE=sort(unique(d$AGE)))
pred.fit1$pred = predict(fit1, newdata=pred.fit1,level=0)
pred.fit1$dldt = with(as.list(fixef(fit1)), 
                      exp(lnKgr)*(exp(lnLinf)-exp(lnL0))*exp(-exp(lnKgr)*pred.fit1$AGE))

# Plot empirical derivative (with scatterplot smooth)
# and overlay model-predicted derivative (red line)
plot1 <- qplot(x=obsmean,y=dldt,data=deltas, 
               xlab="Length (cm)", ylab="Derivative of length (dL/dt)") + 
  stat_smooth(method='gam',formula=y~s(x)) +
  geom_line(data=pred.fit1, aes(x=pred,y=dldt),col='red')
plot1

# Extract Empirical Bayes parameter estimates
fit1.coef = coef(fit1)
names(fit1.coef) = glue(names(fit1.coef),'.fit1')
fit1.coef$ID = row.names(fit1.coef)

#
#
#----- Fit model with non-linear length deceleration -----##
#
#

# Fit non-linear deceleration exponential decay model to the data
#  Y = Linf - (Linf - L0)*exp(-Kgr(t) * AGE) + error
# where
#  Kgr(t) = Kgr0 * (1 - Kgrdec + Kgrdec*exp(-lambda*AGE)
#
# The parameters Linf, L0 and Kgr are subject-specific.
#  - Linf is the estimated asymptotic length (at AGE = Infinity)
#  - L0 is the estimated length at AGE=0
#  - Kgr0 is the initial rate at which length reaches the asymptote (Linf)
#
# These parameters are estimated on the natural log scale to
# (1) force them to be positive
# (2) incorporate log-normal subject-specific random effects. 
#     for example: lnLinf ~ N(mu_lnLinf, omega_lnLinf)
#
# The remaining parameters are:
#  - Kgrdec is the fractional reduction in K0 at large ages
#  - lambda is the rate constant at which the rate K declines from Kgr0 to Kgr0*(1-Kgrdec)
#    lambda is parameterized as lambda = log(2)/TTP50, where TTP50 is the time to reach
#    50% of the reduction in the rate deceleration

fit2 <- nlme(HEIGHT ~ NLLDmodel(lnLinf,lnL0,lnKgr0,logitKgrdec,lnTTP50,AGE),
             random=list(ID=lnLinf+lnKgr0+lnL0~1),
             fixed=lnLinf+lnL0+lnKgr0+logitKgrdec+lnTTP50~1,
             start=c(lnLinf=log(100),lnL0=log(50),lnKgr0=log(0.01),logitKgrdec=0,
                     lnTTP50=log(4)),
             data=d,
             na.action=na.omit)

# Extract the estimated fixed effects (on the original scale)
fit2.params = fixed.effects(fit2)
print(fit2.params)
# lnLinf         lnL0        lnKgr       Kgrdec      lnTTP50 
# 139.72519796  52.16483347   0.04290083   1.75255260   7.43172646 

# Print the AIC for this model
AIC(fit2)
#973.6251 

# Show the model information and parameter estimates for all parameters
print(summary(fit2))

# Nonlinear mixed-effects model fit by maximum likelihood
# Model: HEIGHT ~ NLLDmodel(lnLinf, lnL0, lnKgr0, logitKgrdec, lnTTP50,      AGE) 
# Data: d 
# AIC      BIC    logLik
# 973.6253 1013.904 -474.8127
# 
# Random effects:
#   Formula: list(lnLinf ~ 1, lnKgr0 ~ 1, lnL0 ~ 1)
# Level: ID
# Structure: General positive-definite, Log-Cholesky parametrization
# StdDev     Corr         
# lnLinf   0.05815829 lnLinf lnKgr0
# lnKgr0   0.14798545 -0.766       
# lnL0     0.03121442  0.542 -0.661
# Residual 1.87851992              
# 
# Fixed effects: lnLinf + lnL0 + lnKgr0 + logitKgrdec + lnTTP50 ~ 1 
# Value  Std.Error  DF  t-value p-value
# lnLinf       4.939673 0.02783396 188 177.4693   0.000
# lnL0         3.954408 0.00973121 188 406.3635   0.000
# lnKgr0      -3.148849 0.09538493 188 -33.0120   0.000
# logitKgrdec  0.245520 0.09790476 188   2.5077   0.013
# lnTTP50      2.005738 0.12466029 188  16.0896   0.000
# Correlation: 
#   lnLinf lnL0   lnKgr0 lgtKgr
# lnL0         0.337                     
# lnKgr0      -0.761 -0.584              
# logitKgrdec  0.183 -0.414  0.414       
# lnTTP50      0.619  0.295 -0.848 -0.349
# 
# Standardized Within-Group Residuals:
#   Min           Q1          Med           Q3          Max 
# -3.272244056 -0.517019335  0.002136115  0.536174634  2.757109547 
# 
# Number of Observations: 212
# Number of Groups: 20 


### Model diagnostic plots

# Plot the individual level (aka level 1) residuals vs. fitted values
plot(fit2)

# Make a similar plot but overlay a smoothing spline to look for trends
plot(fitted(fit2,level=1), residuals(fit2,level=1,type='pearson'),
     xlab = "Individual-level Fitted Values", ylab = "Individual-level Standardized Residuals")
abline(h=0, lty=2)
lines(smooth.spline(fitted(fit2,level=1), 
                    residuals(fit2,level=1,type='pearson'))
)
# Clearly better residuals and correlations between parameters

# Plot the population level (aka level 0) residuals vs. fitted values
plot(fitted(fit2,level=0), residuals(fit2,level=0,type='pearson'),
     xlab = "Population-level Fitted Values", ylab = "Population-level Standardized Residuals")
abline(h=0, lty=2)
lines(smooth.spline(fitted(fit2,level=0), 
                    residuals(fit2,level=0,type='pearson'))
) 
# Some possible trend, but clearly better residuals that with fit1


# Plot Individual-level residuals vs. Age
plot(d$AGE, residuals(fit2,level=1,type='pearson'),
     xlab = "Age (months)", ylab = "Individual-level Standardized Residuals")
abline(h=0, lty=2)
lines(smooth.spline(d$AGE, residuals(fit2, level=1,type='pearson'))) 

#
# Check for residual pattern within groups and difference between groups      
xyplot(residuals(fit2) ~ fitted(fit2) | d$SEXF, main = "Fixed Kgr Model Residuals - by SEX",
       panel=function(x, y){ 
         panel.xyplot(x, y) 
         panel.loess(x, y, span = 0.75) 
         panel.lmline(x, y, lty = 2)  # Least squares broken line
       } 
)


# Calculate model-predicted derivative (dldt)
# For the mono-exponential model this is:
# dldt = Kgr * (Linf - L0) * exp(-Kgr * AGE)

pred.fit2 = data.frame(AGE=sort(unique(d$AGE)))
pred.fit2$pred = predict(fit2, newdata=pred.fit2,level=0)
pred.fit2$lambda = log(2)/exp(fixef(fit2)['lnTTP50'])
pred.fit2$Kgrdec = expit(fixef(fit2)['logitKgrdec'])
pred.fit2$kgr = with(as.list(fixef(fit2)),
                     exp(lnKgr0)*(1-pred.fit2$Kgrdec + pred.fit2$Kgrdec*exp(-pred.fit2$lambda * pred.fit2$AGE))
)

pred.fit2$dldt = with(as.list(c(fixef(fit2),pred.fit2)), 
                      (exp(lnLinf)-exp(lnL0))*exp(-kgr*AGE)*(kgr - AGE*lambda*exp(lnKgr0)*Kgrdec*exp(-lambda*AGE))
)


# Plot empirical derivative (with scatterplot smooth)
# and overlay model-predicted derivatives from mono-exponential model (red line) 
# and non-linear length deceleration model (black line)
plot2 <- qplot(x=obsmean,y=dldt,data=deltas, 
               xlab="Length (cm)", ylab="Derivative of length (dL/dt)") + 
  stat_smooth(method='gam',formula=y~s(x)) +
  geom_line(data=pred.fit1, aes(x=pred,y=dldt),col='red') +
  geom_line(data=pred.fit2, aes(x=pred,y=dldt),col='black')
plot2

# Extract the Empirical Bayes parameter estimates
fit2.coef = coef(fit2)
names(fit2.coef) = glue(names(fit2.coef),'.fit2')
fit2.coef$ID = row.names(fit2.coef)


###
### Model comparison
###

anova(fit1, fit2, test = "F")

#Model df       AIC      BIC    logLik
#fit1     1 10 1083.4001 1116.966 -531.7001
#fit2     2 12  973.6251 1013.904 -474.8126


#
#
#----- Fit model with non-linear length deceleration and SEX and SITE effects 
#----- on Linf, L0 and Kgr
#
#

# In this model, we update the non-linear deceleration model by adding effects 
# of SEX on Linf, L0 and Kgr0

fit3 <- nlme(HEIGHT ~ NLLDmodel(lnLinf,lnL0,lnKgr0,logitKgrdec,lnTTP50,AGE),
             random=list(ID=lnLinf+lnKgr0+lnL0~1),
             fixed=list(lnLinf+lnL0+lnKgr0~SEXF, logitKgrdec+lnTTP50~1),
             start=c(lnLinf=c(log(100),0),
                     lnL0=c(log(50),0),
                     lnKgr0=c(log(0.01),0),
                     logitKgrdec=0,
                     lnTTP50=log(4)),
             data=d,
             na.action=na.omit)

# Extract Empirical Bayes parameter estimates
coef3 = coef(fit3)

# Derive individual-specific parameters
model.mat = model.matrix(~SEXF,data=subset(d,!duplicated(ID)))
fit3.coef = data.frame(lnLinf.fit3 = coef3[['lnLinf.(Intercept)']] + coef3[['lnLinf.SEXFFemale']]*model.mat[,2])
fit3.coef$lnL0.fit3 = coef3[['lnL0.(Intercept)']] + coef3[['lnL0.SEXFFemale']] * model.mat[,2]
fit3.coef$lnKgr0.fit3 = coef3[['lnKgr0.(Intercept)']] + coef3[['lnKgr0.SEXFFemale']] * model.mat[,2]
fit3.coeflogitKgrdec.fit3 = coef3$logitKgrdec
fit3.coef$lnTTP50.fit3 = coef3$lnTTP50

fit3.coef$ID = row.names(coef3)

#
#
##----- Fit final model with non-linear length deceleration, using
##----- an alternative parameterization.
#
#

# Instead of estimating the asymptotic length (Linf), we estimate length
# at a specified age (tStar=24 months).  With this re-parameterization,
#   - Ltstar = lengh at t.star
#   - Kgr.tstar = Kgr0*(1 - Kgrdec + Kgrdec*exp(-t.star*log(2)/TTP50)) 
#   - Linf = (Ltstar - L0*exp(-Kgr.tstar*t.star)) / (1 - exp(-Kgr.tstar*t.star))
#
# The remainder of the model remains the same. Thus, we estimate the following parameters:
#   - Ltstar, L0, Kgr0, Kgrdec, and TTP50
#

# Fit the re-parameterized model
fit2.tstar <- nlme(HEIGHT ~ NLLDTstarModel(lnLtstar,lnL0, lnKgr0,logitKgrdec,lnTTP50,AGE),
             random=list(ID=lnLtstar+lnKgr0+lnL0~1),
             fixed=lnLtstar+lnL0+lnKgr0+logitKgrdec+lnTTP50~1,
             start=c(lnLtstar=log(100),lnL0=log(50),lnKgr0=log(0.01),logitKgrdec=0,
                     lnTTP50=log(4)),
             data=d,
             na.action=na.omit)

##
## Compare model fits for the t.star and standard implementation of the NLLD model
##
AIC(fit2,fit2.tstar)

#
if (PDF) {dev.off()}
#
####----------------------------------------#



