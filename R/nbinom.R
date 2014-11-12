library(ggplot2)
library(plyr)
library(lubridate)
library(dplyr)
library(simcf)                          # needs github/pschmied fork
library(MASS)
library(pscl)


## Load the saved dataset
wb <- read.csv("data/weatherbike2yr.csv",
               header=T, stringsAsFactors=FALSE)

##
## Useful recodings of variables
##
wb$dow <- wday(wb$Date)
## wb$dow <- as.factor(as.character(wday(
##     wb$Date, label=TRUE, abbr=TRUE)))         # Day of Week
wb$daylight <- wb$sunsetTime - wb$sunriseTime # Min of daylight

## Data type of all cats
lapply(subset(wb, select=all.vars(mod1_f)), class)

##
## Helper functions
##

se <- function(nbmodel) {
    ## Takes estimated model; returns std err for coefs
    sqrt(diag(vcov(nbmodel)))
}

simbetas <- function(nbmodel, nsim=10000) {
    ## Takes estimated model and (optionally) number of simulations;
    ## returns matrix of simulated betas drawn from multivariate
    ## normal distribution
    mvrnorm(nsim, coef(nbmodel), vcov(nbmodel))
}



##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~ MODELS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 

##
## Model 1:
##

mod1_f <- count ~ dow + daylight + holiday + uw +
    temperatureMax + precipIntensityMax +
    cloudCover + X

mod1_m <- glm.nb(mod1_f, data=wb)       # estimate the model
set.seed(123456)                        # To reproduce results
mod1_sb <- simbetas(mod1_m)             # Simulate our betas

## Counterfactual simulations of model 1
mod1_cf1 <- cfMake2(mod1_f, wb, temperatureMax=29:85, uw=c(TRUE,FALSE))

mod1_res1 <- loglinsimev(mod1_cf1, mod1_sb) # Run the sim
mod1_tidy1 <- fortify.longsim(mod1_res1, mod1_cf1)

m1c1p1 <- ggplot(mod1_tidy1, aes(x=temperatureMax, y=pe, ymin=lower,
                                ymax=upper, color=uw)) +
          geom_point() + geom_errorbar()

ggsave(file="m1c1p1.pdf", path="fig")

## cf2
mod1_cf2 <- cfMake2(mod1_f, wb, precipIntensityMax=seq(0, 0.3275, by=.001),
                    uw=c(TRUE,FALSE))
mod1_res2 <- loglinsimev(mod1_cf2, mod1_sb) # Run the sim
mod1_tidy2 <- fortify.longsim(mod1_res2, mod1_cf2)

ggplot(mod1_tidy2, aes(x=precipIntensityMax, y=pe, ymin=lower,
                       ymax=upper, fill=uw)) +
geom_line() + geom_ribbon(alpha=0.3)

