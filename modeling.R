# Run script to create functions to define the models
source("ModelFunctions.T")

# Define the logit and inverse-logit (aka expit) functions
logit <- function(x) log(x / (1 - x))
expit <- function(x) 1 / (1 + exp(-x))

require(metrumrg)
require(Hmisc)
require(lattice)
require(ggplot2)
require(foreign)
require(MASS)
require(nlme)
require(reshape)
require(plyr)
require(mgcv)
require(sitar)
require(quantreg)
require(gamlss)
library(dplyr)

# Read the data
mydata <- read.csv("cmc_synthetic.csv")

# make a "working copy" of the dataset that we loaded
d <- mydata

# Look at the first few rows of the data
head(d)
d$agemonths <- hbgd::days2months(d$agedays)

## KEEP
# Plot the height vs. age data by child.
# - overlay a loess smooth as a heavy red line
g <- ggplot(d, aes(x = agemonths, y = htcm))
g + geom_line(aes(group = subjid), alpha = 0.5) +
  geom_smooth(aes(group = 1), colour = "red", size = 1,
    method = "gam", formula = y ~ s(x))

## KEEP
# Plot height vs. age by child and panelled by sex
ggplot(data = d, aes(x = agemonths, y = htcm, group = subjid, col = sex)) +
  geom_line(alpha = 0.2) +
  facet_grid(. ~ sex)

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

fit1 <- nlme(htcm ~ MonoExpModel(lnLinf, lnL0, lnKgr, agemonths),
  random = list(subjid = lnLinf + lnL0 + lnKgr ~ 1),
  fixed = lnLinf + lnL0 + lnKgr ~ 1,
  start = c(lnLinf = log(130), lnL0 = log(50), lnKgr = log(0.01)),
  data = d,
  na.action = na.omit)

# Extract the estimated fixed effects (on the original scale)
fit1.params <- exp(fixed.effects(fit1))
print(fit1.params)
#     lnLinf        lnL0       lnKgr
# 102.15899362  52.52712713   0.03432225

# Print the AIC for this model
AIC(fit1)
# 45949.74

# Show the model information and parameter estimates for all parameters
print(summary(fit1))

### Model diagnostic plots

# Plot the individual level (aka level 1) residuals vs. fitted values
plot(fit1)

# Make a similar plot but overlay a smoothing spline to look for trends
plot(fitted(fit1, level = 1), residuals(fit1, level = 1, type = "pearson"),
  xlab = "Individual-level Fitted Values",
  ylab = "Individual-level standardized residuals")
abline(h = 0, lty = 2)
lines(smooth.spline(fitted(fit1, level = 1),
  residuals(fit1, level = 1, type = "pearson")), col = "red")

# Clearly some issues with the residuals and correlations between parameters

# Plot the population level (aka level 0) residuals vs. fitted values
plot(fitted(fit1, level = 0), residuals(fit1, level = 0, type = "pearson"),
  xlab = "Population-level Fitted Values",
  ylab = "Population-level Standardized Residuals")
abline(h = 0, lty = 2)
lines(smooth.spline(fitted(fit1, level = 0),
  residuals(fit1, level = 0, type = "pearson")), col = "red")

# Clearly some issues with the residuals and correlations between parameters
AA
# Plot Individual-level residuals vs. Age
plot(d$agemonths, residuals(fit1, level = 1, type = "pearson"),
     xlab = "Age (months)", ylab = "Individual-level Standardized Residuals")
abline(h = 0, lty = 2)
lines(smooth.spline(d$agemonths,
  residuals(fit1, level = 1, type = "pearson")), col = "red")

# Check for residual pattern within groups and difference between groups
xyplot(residuals(fit1) ~ fitted(fit1) | d$sex, groups = d$sex,
  main = "Fixed Kgr Model Residuals - by sex",
  panel = function(x, y, groups = groups){
    panel.xyplot(x, y)
    panel.loess(x, y, span = 0.75, col = "black")
    panel.lmline(x, y, lty = 2)  # Least squares broken line
  }
)

# We see similar patterns of residuals

# Calculate empirical derivatives (dldt = change in length / change in time)
deltas <- lapply(split(d, d$subjid), function(dd) {
  p <- nrow(dd)
  res <- data.frame(ID = dd$subjid[1:(p - 1)],
    dldt = diff(dd$htcm) / diff(dd$agedays),
    tmean = (dd$agemonths[1:(p - 1)] + dd$agemonths[2:p]) / 2,
    obsmean = (dd$htcm[1:(p - 1)] + dd$htcm[2:p]) / 2)
  return(res)
})
deltas <- do.call(rbind, deltas)
head(deltas)
deltadplyr <- d %>%
  select(subjid, agemonths, htcm) %>%
  group_by(subjid) %>%
  mutate(
    obsmean = htcm - lag(htcm),
    tmean = agemonths - lag(agemonths),
    dldt = obsmean / tmean)

# Calculate model-predicted derivative (dldt)
# For the mono-exponential model this is:
# dldt = Kgr * (Linf - L0) * exp(-Kgr * AGE)

pred.fit1 <- data.frame(agemonths = sort(unique(d$agemonths)))
pred.fit1$pred <- predict(fit1, newdata = pred.fit1, level = 0)
pred.fit1$dldt <- with(as.list(fixef(fit1)),
  exp(lnKgr) * (exp(lnLinf) - exp(lnL0)) * exp(-exp(lnKgr) * pred.fit1$agemonths))

# Plot empirical derivative (with scatterplot smooth)
# and overlay model-predicted derivative (red line)
plot1 <- qplot(x = obsmean, y = dldt, data = deltas,
  xlab = "Length (cm)", ylab = "Derivative of length (dL/dt)") +
  stat_smooth(method = "gam", formula = y ~ s(x)) +
  geom_line(data = pred.fit1, aes(x = pred, y = dldt), col = "red")
plot1

# Extract Empirical Bayes parameter estimates
fit1.coef <- coef(fit1)
names(fit1.coef) <- glue(names(fit1.coef), ".fit1")
fit1.coef$ID <- row.names(fit1.coef)

fit2 <- nlme(htcm ~ NLLDmodel(lnLinf, lnL0, lnKgr0, logitKgrdec, lnTTP50, agemonths),
  random = list(subjid = pdDiag(lnLinf + lnKgr0 + lnL0 ~ 1)),
  fixed = lnLinf + lnL0 + lnKgr0 + logitKgrdec + lnTTP50 ~ 1,
  start = c(lnLinf = log(121), lnL0 = log(44), lnKgr0 = log(0.05),
    logitKgrdec = 1.7, lnTTP50 = log(6)),
  data = d, na.action = na.omit,
  control = nlmeControl(maxIter = 100, returnObject = TRUE), verbose = TRUE)

# Extract the estimated fixed effects (on the original scale)
fit2.params <- fixed.effects(fit2)
print(fit2.params)

AIC(fit2)# 40017.06 fit 2 has much lower AIC
AIC(fit1) #45949.74


print(summary(fit2))



### Model diagnostic plots

# Plot the individual level (aka level 1) residuals vs. fitted values
plot(fit2)

# Make a similar plot but overlay a smoothing spline to look for trends
plot(fitted(fit2, level = 1), residuals(fit2, level = 1, type = "pearson"),
  xlab = "Individual-level Fitted Values",
  ylab = "Individual-level Standardized Residuals")
abline(h = 0, lty = 2)
lines(smooth.spline(fitted(fit2, level = 1),
  residuals(fit2, level = 1, type = "pearson")), col = "red")
# Clearly better residuals and correlations between parameters

# Plot the population level (aka level 0) residuals vs. fitted values
plot(fitted(fit2, level = 0), residuals(fit2, level = 0, type = "pearson"),
  xlab = "Population-level Fitted Values",
  ylab = "Population-level Standardized Residuals")
abline(h = 0, lty = 2)
lines(smooth.spline(fitted(fit2, level = 0),
  residuals(fit2, level = 0, type = "pearson")), col = "red")
# Some possible trend, but clearly better residuals that with fit1
# we might need a proportional variance model

# Plot Individual-level residuals vs. Age
plot(d$agemonths, residuals(fit2, level = 1, type = "pearson"),
  xlab = "Age (months)",
  ylab = "Individual-level Standardized Residuals")
abline(h = 0, lty = 2)
lines(smooth.spline(d$agemonths,
  residuals(fit2, level = 1, type = "pearson")), col = "red")

# Check for residual pattern within groups and difference between groups
xyplot(residuals(fit2) ~ fitted(fit2) | d$sex,
  main = "Fixed Kgr Model Residuals - by sex",
  panel = function(x, y){
    panel.xyplot(x, y)
    panel.loess(x, y, span = 0.75, col = "black")
    panel.lmline(x, y, lty = 2, col = "black")  # Least squares broken line
  }
)

# Calculate model-predicted derivative (dldt)

pred.fit2 <- data.frame(agemonths = sort(unique(d$agemonths)))
pred.fit2$pred <- predict(fit2, newdata = pred.fit2, level = 0)
pred.fit2$lambda <- log(2) / exp(fixef(fit2)["lnTTP50"])
pred.fit2$Kgrdec <- expit(fixef(fit2)["logitKgrdec"])
pred.fit2$kgr <- with(as.list(fixef(fit2)),
  exp(lnKgr0) * (1 - pred.fit2$Kgrdec + pred.fit2$Kgrdec *
    exp(-pred.fit2$lambda * pred.fit2$AGE)))

pred.fit2$dldt <- with(as.list(c(fixef(fit2), pred.fit2)),
  (exp(lnLinf) - exp(lnL0)) * exp(-kgr * agemonths) *
    (kgr - agemonths * lambda * exp(lnKgr0) * Kgrdec * exp(-lambda * agemonths)))

# Plot empirical derivative (with scatterplot smooth)
# and overlay model-predicted derivatives from mono-exponential model (red line)
# and non-linear length deceleration model (black line)
plot2legend <-
  ggplot(deltas, aes(obsmean, dldt)) +
  geom_point(alpha = 0.05) +
  stat_smooth(method = "gam", formula = y~s(x), aes(col = "Empirical")) +
  geom_line(data = pred.fit1, aes(x = pred, y = dldt, col = "MonoExpModel")) +
  geom_line(data = pred.fit2, aes(x = pred, y = dldt, col = "NLLDmodel"), size = 1.2) +
  xlab("Length (cm)") +
  ylab("Derivative of length (dL/dt)") +
  scale_colour_manual(breaks = c("Empirical", "MonoExpModel", "NLLDmodel"),
    values = c("blue", "red", "black")) +
  theme_bw() +
  theme(legend.title = element_blank(), legend.position = "bottom",
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12, face = "bold")) +
  theme(panel.grid.minor  =  element_line(colour = "gray", linetype = "dotted")) +
  theme(panel.grid.major =  element_line(colour = "gray", linetype = "solid")) +
  coord_cartesian(ylim = c(-5, 10))

plot2legend

# Extract the Empirical Bayes parameter estimates
fit2.coef <- coef(fit2)
names(fit2.coef) <- glue(names(fit2.coef), ".fit2")
fit2.coef$ID <- row.names(fit2.coef)


###
### Model comparison
###

anova(fit1, fit2, test = "F")

# 2 is better

fit.nldgompertz <- nlme(htcm ~ Linf * exp(-Alpha * exp(-Beta *
  (1 - Theta + Theta * exp(-Lambda * agemonths)) * agemonths)),
  data = d,
  fixed = Linf + Alpha + Beta + Theta + Lambda ~ 1,
  random = list(subjid = pdDiag(Linf + Alpha + Lambda ~ 1)),
  start = c(Linf = 49.96386835, Alpha = 5.62118906,
    Beta = 0.10451279, Theta = 0.75939675, Lambda = 0.01557118),
  control = nlmeControl(returnObject = TRUE, maxIter = 100),
  verbose = TRUE)

anova(fit.nldgompertz, fit2) # fit 2 is better

# we now add the effect of sex on Linf, L0 and Kgr0

fit3 <- update(fit2,
  fixed = list(lnLinf + lnL0 + lnKgr0 ~ sex, logitKgrdec + lnTTP50 ~ 1),
  start = c(lnLinf = c(log(100), 0),
    lnL0 = c(log(50), 0),
    lnKgr0 = c(log(0.01), 0),
    logitKgrdec = 0,
    lnTTP50 = log(4)
  ))

anova(fit2, fit3, test = "F")

# Extract Empirical Bayes parameter estimates
coef3 <- coef(fit3)

# Derive individual-specific parameters
model.mat <- model.matrix(~sex, data = subset(d, !duplicated(subjid)))
fit3.coef <- data.frame(
  lnLinf.fit3 = coef3[["lnLinf.(Intercept)"]] +
    coef3[["lnLinf.sexMale"]] * model.mat[, 2],
  lnL0.fit3 = coef3[["lnL0.(Intercept)"]] +
    coef3[["lnL0.sexMale"]] * model.mat[, 2],
  lnKgr0.fit3 = coef3[["lnKgr0.(Intercept)"]] +
    coef3[["lnKgr0.sexMale"]] * model.mat[, 2],
  logitKgrdec.fit3 = coef3$logitKgrdec,
  lnTTP50.fit3 =  coef3$lnTTP50
)

fit3.coef$ID <- row.names(coef3)
head(fit3.coef)

fit2RE <- ranef(fit2, augFrame = TRUE, data = d)
fit3RE <- ranef(fit3, augFrame = TRUE, data = d)

ggplot(fit2RE, aes(sex, lnKgr0)) +
  geom_boxplot(aes(col = "No Covariate"))

ggplot(fit3RE, aes(sex, `lnKgr0.(Intercept)`)) +
  geom_boxplot(aes(col = "With  Covariate"))

# this to show that we can fit a model using the pakcage and using hbgd

require(sitar)
fit.sitar <- sitar(x = agemonths, y = htcm, id = subjid, data = d,
  df = 4,
  fixed = "a", random = "a+b+c",
  a.formula = ~sex, b.formula = ~1, c.formula = ~1)
print(summary(fit.sitar))
# Sometimes SITAR can be sensitive to the choice of knots / df
quantile(d$agemonths, probs = c(1:3) / 4)
fit.sitar2 <- update(fit.sitar, knots = c(10, 21, 33), verbose = TRUE)
quantile(d$agemonths, probs = c(1:4) / 5)
fit.sitar3 <- update(fit.sitar, knots = c(8, 17, 26, 35), verbose = TRUE)

# Print the AIC for the first model
AIC(fit.sitar)
# 983.826

# What if we try adding additional fixed effects?
fit.sitar4 <- update(fit.sitar, fixed = "a+b+c", verbose = TRUE)
AIC(fit.sitar4)
# 988.3587

# Show the model information and parameter estimates for all parameters
print(summary(fit.sitar))

# The SITAR class has a plotting method that shows the model fit and derivative
plot(fit.sitar)
plot(fit.sitar, y2par = list(col = "blue"))
plot(fit.sitar, opt = "a", col = 1 + as.integer(subjid) %% 5, xlim = xaxsd())

outliers <- velout(agemonths, htcm, subjid, d, limit = 5)
#codeplot(outliers, icode=c(1,6))

# velicitysitar <- data.frame( x =  unlist(
#   plot.sitar2(fit.sitar, opt='v' ) ["ss"] $ss$x ),
#
#   v=unlist(
#     plot.sitar2(fit.sitar, opt='v' ) ["ss1"] [[1]][2] )
# )


### Model diagnostic plots

# Plot the individual level (aka level 1) residuals vs. fitted values
df <- data.frame(resid1 = residuals(fit.sitar, level = 1, type = "pearson"),
  resid0 = residuals(fit.sitar, level = 0, type = "pearson"),
  ipred = predict(fit.sitar, level = 1),
  pred = predict(fit.sitar, level = 0),
  age = d$agemonths,
  sex = d$sex,
  htcm = d$htcm)

sitar.plot <- ggplot( data = df, aes(x = ipred, y = resid1)) +
  geom_point() +
  labs(x = "Fitted values", y = "Standardized residuals") +
  geom_hline(yintercept = 0, col = "red", alpha = 0.5)
print(sitar.plot)

# Make a similar plot but overlay a smoothing spline to look for trends
sitar.plot + stat_smooth()

# Plot the population level (aka level 0) residuals vs. fitted values
ggplot(data = df, aes(x = pred, y = resid0)) +
  geom_point() +
  labs(x = "Fitted values", y = "Standardized residuals") +
  geom_hline(yintercept = 0, col = "red", alpha = 0.5) +
  stat_smooth(method = "loess")
# Some  trend

# Plot Individual-level residuals vs. Age
ggplot(data = df, aes(x = age, y = resid1))
  geom_point() +
  labs(x = "Age (m)", y = "Standardized residuals") +
  geom_hline(yintercept = 0, col = "red", alpha = 0.5) +
  stat_smooth(method = "loess")

# Check for residual pattern within groups and difference between groups
ggplot(data = df, aes(x = ipred, y = resid1)) +
  geom_point() +
  labs(x = "Fitted values)", y = "Standardized residuals") +
  geom_hline(yintercept = 0, col = "red", alpha = 0.5) +
  stat_smooth(method = "loess") +
  facet_wrap(~sex)

#--- Fit Piecewise linear (PL) model (with knots at 3, 6, 12, 24, 36, and 48 months)
#
# The PL mdoel is
#    y = a_i + b_i*AGE + \sum_j beta_j * K(AGE-knot_j) + error
# where K(x) = 0 if x<0 and K(x) = x if x>0.
# This particular model has random effects on the intercept and the overall slope, but not
# on age-specific deviations from the initial slope (although they could be incorporated).

# Define the knots and create variables K(Age-knot)
knots <- c(3, 6, 12, 24, 36)
for (knotj in knots) {
  newvar <- paste0("aggt", knotj)
  d[, newvar] <- ifelse(d$agemonths > knotj, d$agemonths - knotj, 0)
}

# Define a simple model as a function of age, only
rhs <- paste(grep("ag", names(d), value = TRUE), collapse = " + ")
pl.model <- formula(paste("htcm ~", rhs))
print(pl.model)

# Fit model with random intercept and slope (overall effect of age).
fit.pl <- lme(pl.model, data = d, random = ~ agemonths | subjid, method = "ML")

# Print AIC
AIC(fit.pl)
# Update the model to include sex effects on intercept and slope
fit.pl2 <- update(fit.pl, .~. + sex - agemonths + agemonths * sex)
AIC(fit.pl2)
# 984.6664

# Show the model information and parameter estimates for all parameters
print(summary(fit.pl))
### Model diagnostic plots

# Plot the individual level (aka level 1) residuals vs. fitted values
df <- data.frame(resid1 = residuals(fit.pl, level = 1, type = "pearson"),
  resid0 = residuals(fit.pl, level = 0, type = "pearson"),
  ipred = predict(fit.pl, level = 1),
  pred = predict(fit.pl, level = 0),
  agemonths = d$agemonths,
  sex = d$sex,
  htcm = d$htcm)

pl.plot <- ggplot( data = df, aes(x = ipred, y = resid1)) +
  geom_point() +
  labs(x = "Fitted values", y = "Standardized residuals") +
  geom_hline(yintercept = 0, col = "red", alpha = 0.5)
print(pl.plot)

# Make a similar plot but overlay a smoothing spline to look for trends
pl.plot + stat_smooth()

# Plot Individual-level residuals vs. Age
ggplot(data = df, aes(x = age, y = resid1)) +
  geom_point() +
  labs(x = "Age (m)", y = "Standardized residuals") +
  geom_hline(yintercept = 0, col = "red", alpha = 0.5) +
  stat_smooth(method = "loess")

# Check for residual pattern within groups and difference between groups
ggplot(data = df, aes(x = ipred, y = resid1)) +
  geom_point() +
  labs(x = "Fitted values)", y = "Standardized residuals") +
  geom_hline(yintercept = 0, col = "red", alpha = 0.5) +
  stat_smooth(method = "loess") +
  facet_wrap(~sexF)

######################## quantreg gamlss section starts
###
f_N <- rqss(htcm ~ qss(agemonths, constraint = "N"), tau = 0.5, lambda = NULL, data = d)
f_I <- rqss(htcm ~ qss(agemonths, constraint = "I"), tau = 0.5, lambda = NULL, data = d)
f_CI <- rqss(htcm ~ qss(agemonths, constraint = "CI"), tau = 0.5, lambda = NULL, data = d)
plot(f_N)
plot(f_I)
plot(f_CI)

f_CI_sex <- rqss(htcm ~ qss(agemonths, constraint = "CI") + sex,
  tau = 0.5, lambda = NULL, data = d)
AIC(f_CI_sex)
AIC(f_CI)
f_CI_0.05 <- rqss(htcm ~ qss(agemonths, constraint = "CI"),
  tau = 0.05, lambda = NULL, data = d)
f_CI_sex_0.05 <- rqss(htcm ~ qss(agemonths, constraint = "CI") + sex,
  tau = 0.5, lambda = NULL, data = d)
AIC(f_CI_sex_0.05)
AIC(f_CI_0.05)

################################
library(gamlss)
library(AGD)

a <- gamlss(htcm ~ cs(agemonths, df = 5), sigma.fo = ~cs(agemonths, df = 2),
  family = BCT, data = na.omit(d[, c("agemonths", "htcm")]))
b <- gamlss(htcm ~ cs(agemonths, df = 5), sigma.fo = ~cs(agemonths, 2),
  family = BCCG, data = na.omit(d[, c("agemonths", "htcm")]))
c <- gamlss(htcm ~ cs(agemonths, df = 5) + sex + re(random = ~1 | subjid ),
  sigma.fo = ~cs(agemonths, 2), family = BCCG,
  data = na.omit(d[, c("subjid", "agemonths", "htcm", "sex")]))

GAIC(a, b, k = 3)
GAIC(b, c, k = 3)

plot(c)
centiles(b, xvar = d$agemonths)
centiles.com(a, b, xvar = d$agemonths)
centiles.split(b, xvar = d$agemonths)
centiles.split(a, xvar = d$agemonths)
centiles.split(c, xvar = d$agemonths)
centiles.com(a, c, xvar = d$agemonths)

coeff1 <- wp.twin(a, b, line = TRUE)
coeff2 <- wp.twin(c, b, line = TRUE)
