## install.packages(c("ggplot2", "plyr", "reshape2", "lubridate", "dplyr", "MASS", "pscl"))
## install_github("pschmied/tile-simcf/simcf") # Requires devtools loaded
library(ggplot2)
library(plyr)
library(reshape2)
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
wb$dow <- as.factor(as.character(wday(
    wb$Date, label=TRUE, abbr=TRUE)))         # Day of Week

wb$daylight <- wb$sunsetTime - wb$sunriseTime # Secs of daylight
wb$daylighth <- wb$daylight / 60 / 60

## Days of the week, recoded as dummy vars
wb <- cbind(wb, dcast(data=wb, X ~ dow, length)[,-1])
wb$Wknd <- (wb$dow == "Sat" | wb$dow == "Sun")
wb$TTh <- (wb$dow == "Tues" | wb$dow == "Wed" | wb$dow == "Thurs")


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

mutex_dummy <- function(cf, dummies) {
    ## Filters out mutually exclusive counterfactual scenarios, e.g.
    ## it can't be Mon AND Tue at the same time. Takes a cf object and
    ## any number of mutually exclusive variables; returns pruned cf
    ## object.
    dumsum <- apply(subset(cf$x, select=dummies), 1, sum)
    cf$x <- subset(cf$x, dumsum <= 1)   # only 0,1
    cf
}

##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~ MODELS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 

##
## Model 1: 
##

mod1_f <- count ~ daylighth + holiday + uw +
    temperatureMax + precipIntensityMax +
    cloudCover + X +
    Sat + Mon + Tues + Wed + Thurs + Fri # Sunday is reference cat

mod1_m <- glm.nb(mod1_f, data=wb)       # estimate the model
set.seed(123456)                        # To reproduce results
mod1_sb <- simbetas(mod1_m)             # Simulate our betas

## Counterfactual simulations of model 1 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## cf1 - Effect of temperature
mod1_cf1 <- cfMake2(mod1_f, wb, temperatureMax=29:85, uw=c(TRUE,FALSE))

mod1_res1 <- loglinsimev(mod1_cf1, mod1_sb) # Run the sim
mod1_tidy1 <- fortify.longsim(mod1_res1, mod1_cf1)

m1c1 <- ggplot(mod1_tidy1, aes(x=temperatureMax, y=pe, ymin=lower,
                               ymax=upper, fill=uw)) +
        geom_line() + geom_ribbon(alpha=0.3) +
        xlab("Daily Max Temp") + ylab("Bicycles") + theme_bw()

ggsave(file="m1c1.pdf", path="fig")


## cf2 - Effect of max precipitation
mod1_cf2 <- cfMake2(mod1_f, wb, precipIntensityMax=seq(0, 0.3275, by=.001),
                    uw=c(TRUE,FALSE))
mod1_res2 <- loglinsimev(mod1_cf2, mod1_sb) # Run the sim
mod1_tidy2 <- fortify.longsim(mod1_res2, mod1_cf2)

m1c2 <- ggplot(mod1_tidy2, aes(x=precipIntensityMax, y=pe, ymin=lower,
                               ymax=upper, fill=uw)) +
        geom_line() + geom_ribbon(alpha=0.3) +
        xlab("Daily Max Precip (inches)") + ylab("Bicycles") + theme_bw()

ggsave(file="m1c2.pdf", path="fig")


## cf3 - Effect of day of the week
nonrefdays <- c("Mon", "Tues", "Wed", "Thurs", "Fri", "Sat")
mod1_cf3 <- cfMake2(mod1_f, wb, Mon = c(0,1), Tues = c(0,1),
                    Wed = c(0,1), Thurs = c(0,1), Fri = c(0,1),
                    Sat = c(0,1), uw=c(T,F))
mod1_cf3 <- mutex_dummy(mod1_cf3, nonrefdays)

mod1_res3 <- loglinsimev(mod1_cf3, mod1_sb) # Run the sim

## Need to convert dummy vars to long format manually.
mod1_tidy3 <- fortify.longsim(mod1_res3, mod1_cf3)
mod1_tidy3$Sun <- apply(subset(mod1_tidy3, select=nonrefdays), 1, sum) == 0
mod1_tidy3 <-
    melt(mod1_tidy3,
         id.vars = setdiff(names(mod1_tidy3), c(nonrefdays, "Sun")),
         variable.name = "Day")
mod1_tidy3 <- subset(mod1_tidy3, value==1)
mod1_tidy3$Day <- factor(mod1_tidy3$Day, levels=c("Sun", nonrefdays))

m1c3 <- ggplot(mod1_tidy3, aes(x=Day, y=pe, ymin=lower,
                               ymax=upper, color=uw)) +
        geom_point() + geom_errorbar(width=0.2) +
        xlab("Day of the week") + ylab("Bicycles") + theme_bw()

ggsave(file="m1c3.pdf", path="fig")


## cf4 - Effect of season (measured continuously via daylight hours)
mod1_cf4 <- cfMake2(mod1_f, wb, daylighth=seq(8, 16, by=0.01),
                    uw=c(TRUE,FALSE))
mod1_res4 <- loglinsimev(mod1_cf4, mod1_sb) # Run the sim
mod1_tidy4 <- fortify.longsim(mod1_res4, mod1_cf4)

m1c4 <- ggplot(mod1_tidy4, aes(x=daylighth, y=pe, ymin=lower,
                               ymax=upper, fill=uw)) +
        geom_line() + geom_ribbon(alpha=0.3) +
        xlab("Daylight (hours)") + ylab("Bicycles") + theme_bw()

ggsave(file="m1c4.pdf", path="fig")
