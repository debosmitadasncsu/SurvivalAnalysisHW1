library(survival)
library(survminer)
library(muhaz)

katrina <- read.csv("/Users/adamtobias/Documents/MSA 19/Survival Analysis/survivalcsv/katrina.csv", header = TRUE)

# Katrina Survival Curve
katrina_fit <- survfit(Surv(hour, survive == 0) ~ 1, data = katrina)
katrina_fit
katrina_fit <- survfit(Surv(hour, survive == 0) ~ reason, data = katrina)
katrina_fit

ggsurvplot(katrina_fit, data = katrina, conf.int = FALSE, palette = "RdBu")

survminer::pairwise_survdiff(Surv(hour, survive == 0) ~ reason, rho = 0, data = katrina[katrina$reason != 0,])
survminer::pairwise_survdiff(Surv(hour, survive == 0) ~ reason, rho = 1, data = katrina[katrina$reason != 0,])

katrina$hour2 <- ifelse(katrina$hour == 48 & katrina$survive == 1, 49, katrina$hour)
katrina$fail <- ifelse(katrina$survive == 1, 0, 1)
# kphaz.fit() has the same arguments as Surv()
katrina_haz <- with(katrina, kphaz.fit(hour2, fail))
# and we plot it with kphaz.plot()
kphaz.plot(katrina_haz, main = "hazard function")
# to see why i needed to restructure this, look at the plot using week instead
# of week2

### cumulative hazard ###
ggsurvplot(katrina_fit, fun = "cumhaz", palette = "grey")
