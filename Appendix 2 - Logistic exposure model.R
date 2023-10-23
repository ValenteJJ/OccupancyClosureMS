
library(RCurl)
library(tidyverse)


rm(list=ls())


woth.locs <- read.csv(text = getURL("https://raw.githubusercontent.com/ValenteJJ/OccupancyClosureMS/main/woth_locs20.csv"), header=T, as.is=T)


tmp = woth.locs %>% 
  mutate(date = date(dt)) %>% 
  select(bird_id, date, territory) %>% 
  unique()










## logistic exposure for WOTH telemetry data
## v2: using updaded fate data, 28Jun20
## v3: final tweaks
## v4: cleaned script

# references:
# https://www.researchgate.net/post/Does_anybody_have_code_for_running_a_logistic_exposure_nest_survival_model_in_R
# https://rpubs.com/bbolker/logregexp

library(MASS)
library(DHARMa)

setwd("C:/CLOUD/MS_Thesis/WOTH_movement/analysis/survival_model")

woth.fate <- read.csv("woth_fate_v2.csv", header = T, as.is = T)
str(woth.fate)

## metadata (not all variables are utilized):
# check =	the date of telemetry check - the first one is capture date
# bird = bird ID (GT5, COLW2, etc)
# int	= interval: the number of the current telemetry check, labeled consecutively starting with 2 (1 is capture)
# t	= the number of days since last telemetry check
# age	= the number of days since trapping of that individual
# sday = season day (days trapping of first bird that year) - accounts for differences between the two sampling years
# fate = shift (1 = bird present in home range, 0 = bird absent (territory shift)

###code adapted from the references above: Hollenbeck, Herzog, and Bolker

# build logexp link function

logexp <- function(exposure = 1) {
  linkfun <- function(mu) qlogis(mu^(1/exposure))
  linkinv <- function(eta) plogis(eta)^exposure
  logit_mu_eta <- function(eta) {
    ifelse(abs(eta)>30,.Machine$double.eps,
           exp(eta)/(1+exp(eta))^2)
  }
  mu.eta <- function(eta) {
    exposure * plogis(eta)^(exposure-1) *
      logit_mu_eta(eta)
  }
  valideta <- function(eta) TRUE
  link <- paste("logexp(", deparse(substitute(exposure)), ")",
                sep="")
  structure(list(linkfun = linkfun, linkinv = linkinv,
                 mu.eta = mu.eta, valideta = valideta,
                 name = link),
            class = "link-glm")
}


# "mean" model should give the overall odds of daily territory shift

mfit.mean <- glm(fate ~ 1, family=binomial(link=logexp(woth.fate$t)), data=woth.fate)
simulateResiduals(mfit.mean, plot = T)
summary(mfit.mean)
CI.95 <- confint(mfit.mean)
CI.95

# convert to probabilities

# low CI
l.p = 1-(exp(CI.95[1])/(1+exp(CI.95[1]))); l.p
# estimate
e.p = 1-(exp(4.6032)/(1+exp(4.6032))); e.p
# high CI
h.p = 1-(exp(CI.95[2])/(1+exp(CI.95[2]))); h.p
