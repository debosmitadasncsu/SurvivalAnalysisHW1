
# packages
library(survival)
library(survminer)
library(flexsurv)
library(dplyr)
library(muhaz)

# load data
katrina <- read.csv("C:/Users/dell/Documents/R code fall/Survival/katrina.csv", header = TRUE)

### checking 4 different distributions

# weibull distribution
fit_wb <- flexsurvreg(Surv(hour, reason == 1) ~ backup + bridgecrane + servo + trashrack + elevation + slope +
                        age, data = katrina, dist = "weibull")

plot(fit_wb, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, bty = "n",
     xlab = "Hour", ylab = "cumulative hazard", main = "weibull distribution")

# exponential distribution
fit_exp <- flexsurvreg(Surv(hour, reason == 1) ~ backup + bridgecrane + servo + trashrack + elevation + slope +
                         age, data = katrina, dist = "exponential")

plot(fit_exp, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, bty = "n", 
     xlab = "Hour", ylab = "cumulative hazard",
     main = "exponential distribution")

# lognormal distribution
fit_lnorm <- flexsurvreg(Surv(hour, reason == 1) ~ backup + bridgecrane + servo + trashrack + elevation + slope +
                           age, data = katrina, dist = "lognormal")
# plot shows lack of fit in very early time periods
plot(fit_lnorm, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, bty = "n",
     xlab = "Hour", ylab = "cumulative hazard", main = "lognormal distribution")

# log-logistic distribution
fit_llogis <- flexsurvreg(Surv(hour, reason == 1) ~ backup + bridgecrane + servo + trashrack + elevation + slope +
                            age, data = katrina, dist = "llogis")
plot(fit_llogis, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, bty = "n",
     xlab = "Hour", ylab = "cumulative hazard",
     main = "log-logistic distribution")

#choose weibull
aft <- survreg(Surv(hour, reason == 1) ~ backup + bridgecrane + servo + trashrack + elevation + slope +
                 age, data = katrina, dist = "weibull")

#get coefficients
summary(aft)