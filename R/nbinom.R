## install.packages(c("ggplot2", "plyr", "reshape2", "lubridate",
## "dplyr", "MASS", "pscl"))
## install_github("pschmied/tile-simcf/simcf") # Requires devtools
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

## temperatureMax squared. Transforming here rather than inside
## formula, because of MLE / SimCF.
wb$temperatureMaxSq <- wb$temperatureMax^2

## Data type of all cats
# lapply(subset(wb, select=all.vars(mod1_f)), class)

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

getyhat <- function(model) {
  # Takes a model object as created by glm, generates yhats
  x <- model.matrix(model)[,-1]
  pe <- model$coefficients
  exp(cbind(rep(1,nrow(x)),x)%*%pe) 
}

binys <- function(y, yhat, by=0.05) {
    ## Bins y according to yhat into bins of some desired size
    binned <- group_by(
        data.frame(y=y,
                   yhat=yhat,
                   bin=cut(yhat, seq(min(y),max(y),by=by))),
        bin)
    summarize(binned, y=mean(y), yhat=mean(yhat), n=n())
}

mknb_llk <- function(formula, data) {
    ## Takes a formula specifying the model and a dataframe to extract
    ## variables from. Returns a closure to compute the log-likelihood
    ## of a negative binomial based on the supplied data
    
    av <- all.vars(formula)                 # Get variables to be used
    sdf <- na.omit(subset(data, select=av)) # subset our data, omit NA
    ys <- sdf[,1]                           # extract dependent var
    xmat <- cbind(1, as.matrix(sdf[,-1]))   # creat design mat

    function(betaalpha) {
        expxb <- exp(xmat %*% head(betaalpha, length(betaalpha) - 1))
        alpha <- tail(betaalpha, 1)
        theta <- 1/alpha
        sum(dnbinom(ys, mu=expxb, size=theta, log=TRUE))
    }
}

## Influence plots. This function adapted from:
## https://gist.github.com/DoktorMike/6278065
plotInfluence <- function (model, size=10) {
  if(!inherits(model, "lm")) 
    stop("You need to supply an lm object.")
  df<-data.frame(Residual=rstudent(model), 
                 Leverage=hatvalues(model), 
                 Cooks=cooks.distance(model), 
                 Observation=names(hatvalues(model)), 
                 stringsAsFactors=FALSE)
  myxint<-c(2*mean(df$Leverage), 3*mean(df$Leverage))
  inds<-intersect(which(abs(df$Residual) < 2), 
                  which( df$Leverage < myxint[1]))
  if(length(inds) > 0) df$Observation[inds]<-""
  ggplot(df, aes_string(x='Leverage', y='Residual', 
                        size='Cooks', label='Observation'), 
         legend=FALSE) +
    geom_point(colour="black", alpha=0.2) + 
    scale_size_area(max_size=size) + 
    theme_bw() + 
    geom_hline(yintercept=c(2,-2), linetype="dashed") + 
    geom_vline(xintercept=myxint, linetype="dashed") + 
    ylab("Studentized Residuals") + 
    xlab("Hat-Values") + labs(size="Cook's distance")
}

##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~ Descriptives ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 

dp1 <- ggplot(data=wb, aes(x=count)) + geom_histogram() + theme_bw() +
    xlab("# bicycles") + ylab("days")

ggsave(file="dp1.pdf", path="fig", width=7, height=2.5, units=("in"))
ggsave(file="dp1-sm.pdf", path="fig", width=5, height=2, units=("in"))

dp2 <- ggplot(data=wb, aes(x=as.Date(Date), y=count)) +
    geom_line(size=.1) + theme_bw() + xlab("date") + ylab("# bicycles")

ggsave(file="dp2.pdf", path="fig", width=7, height=2.5, units=("in"))
ggsave(file="dp2-sm.pdf", path="fig", width=5, height=2, units=("in"))

##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~ MODELS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 

##
## Model 1: 
##

mod1_f <- count ~ daylighth + holiday + uw +
  temperatureMax + temperatureMaxSq +
  precipIntensityMax + X +
  Sat + Mon + Tues + Wed + Thurs + Fri # Sunday is reference cat

mod1_m_pois <- glm(mod1_f, data=wb, family=poisson) # poisson for fun
mod1_m <- glm.nb(mod1_f, data=wb)       # estimate the real model
set.seed(123456)                        # To reproduce results
mod1_sb <- simbetas(mod1_m)             # Simulate our betas

odTest(mod1_m)                          # Test for overdispersion
                                        # (prints results)

llk <- mknb_llk(mod1_f, wb)
mod1_m_nboptim <- optim(c(coef(mod1_m), .5), llk, # reestimate with optim
                        method="BFGS", hessian=TRUE,
                        control = list(fnscale = -1))
mod1_dispersion_alpha <- tail(mod1_m_nboptim$par, 1)

## Model fit ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Actual versus predicted plot - negative binomial
avpp1 <- ggplot(data.frame(actual=wb$count, predicted=getyhat(mod1_m)),
                aes(x=actual, y=predicted)) +
         geom_abline(intercept=0, slope=1, color="red") +
         geom_point() + theme_bw()

ggsave(file="avpp1.pdf", path="fig", width=7, height=7, units=("in"))
ggsave(file="avpp1-sm.pdf", path="fig", width=3.5, height=3.5, units=("in"))

## Actual vs predicted plot - nb, binned
avp_bin_m1 <- binys(y=wb$count, yhat=getyhat(mod1_m), by=400)
avpp2 <- ggplot(avp_bin_m1, aes(x=yhat, y=y, size=n)) +
         geom_abline(intercept=0, slope=1, color="red") +
         geom_point(alpha=0.5) + theme_bw() +
         xlab("predicted") + ylab("actual") +
         scale_size_continuous(name="# values")

ggsave(file="avpp2.pdf", path="fig", width=7, height=5, units=("in"))
ggsave(file="avpp2-sm.pdf", path="fig", width=5, height=3.5, units=("in"))

## Actual vs predicted plot - nb using "predict"

predictedy <- predict(mod1_m,newdata=wb, type="response")

avpp4 <- ggplot(data.frame(actual=wb$count, predictedy),
                 aes(x=actual, y=predictedy)) +
       geom_abline(intercept=0, slope=1, color="red") +
       geom_point() + theme_bw()

ggsave(file="avpp4.pdf", path="fig", width=7, height=5, units=("in"))
ggsave(file="avpp4-sm.pdf", path="fig", width=5, height=3.5, units=("in"))

## Actual vs predicted plot - poisson
avpp3 <- ggplot(data.frame(actual=wb$count, predicted=getyhat(mod1_m_pois)),
                aes(x=actual, y=predicted)) +
         geom_abline(intercept=0, slope=1, color="red") +
         geom_point() + theme_bw()

ggsave(file="avpp3.pdf", path="fig", width=7, height=7, units=("in"))
ggsave(file="avpp3-sm.pdf", path="fig", width=3.5, height=3.5, units=("in"))

## AIC
AIC(mod1_m)                             # Negative Binomial
AIC(mod1_m_pois)                        # Poisson

## BIC
BIC(mod1_m)                             # Negative Binomial
BIC(mod1_m_pois)                        # Poisson

##Influence Plot: Residuals
residuals <- plotInfluence(mod1_m) + scale_y_continuous(limits=c(-50, 40))
ggsave(file="residuals.pdf", path="fig", width=7, height=3.5, units=("in"))
ggsave(file="residuals-small.pdf", path="fig", width=3.5, height=3.5, units=("in"))

residuals_pois <- plotInfluence(mod1_m_pois)
ggsave(file="residuals_pois.pdf", path="fig", width=7, height=3.5, units=("in"))
ggsave(file="residuals_pois-small.pdf", path="fig", width=3.5, height=3.5, units=("in"))

## Counterfactual simulations of model 1 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## cf1 - Effect of temperature
mod1_cf1 <- cfMake2(mod1_f, wb, temperatureMax=29:85)

mod1_res1 <- loglinsimev(mod1_cf1, mod1_sb) # Run the sim
mod1_tidy1 <- fortify.longsim(mod1_res1, mod1_cf1)

m1c1 <- ggplot(mod1_tidy1, aes(x=temperatureMax, y=pe, ymin=lower,
                               ymax=upper)) +
        geom_line() + geom_ribbon(alpha=0.3) +
        xlab("Daily Max Temp") + ylab("Bicycles") + theme_bw()

ggsave(file="m1c1.pdf", path="fig", width=7, height=4, units=("in"))
ggsave(file="m1c1-sm.pdf", path="fig", width=3.5, height=2, units=("in"))


## cf2 - Effect of max precipitation
mod1_cf2 <- cfMake2(mod1_f, wb, precipIntensityMax=seq(0, 0.3275, by=.001))
mod1_res2 <- loglinsimev(mod1_cf2, mod1_sb) # Run the sim
mod1_tidy2 <- fortify.longsim(mod1_res2, mod1_cf2)

m1c2 <- ggplot(mod1_tidy2, aes(x=precipIntensityMax, y=pe, ymin=lower,
                               ymax=upper)) +
        geom_line() + geom_ribbon(alpha=0.3) +
        xlab("Daily Max Precip (inches)") + ylab("Bicycles") + theme_bw()

ggsave(file="m1c2.pdf", path="fig", width=7, height=4, units=("in"))
ggsave(file="m1c2-sm.pdf", path="fig", width=3.5, height=2, units=("in"))


## cf3 - Effect of day of the week
nonrefdays <- c("Mon", "Tues", "Wed", "Thurs", "Fri", "Sat")
mod1_cf3 <- cfMake2(mod1_f, wb, Mon = c(0,1), Tues = c(0,1),
                    Wed = c(0,1), Thurs = c(0,1), Fri = c(0,1),
                    Sat = c(0,1))
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
                               ymax=upper)) +
        geom_point() + geom_errorbar(width=0.2) +
        xlab("Day of the week") + ylab("Bicycles") + theme_bw()

ggsave(file="m1c3.pdf", path="fig", width=7, height=4, units=("in"))
ggsave(file="m1c3-sm.pdf", path="fig", width=3.5, height=2, units=("in"))


## cf4 - Effect of season (measured continuously via daylight hours)
mod1_cf4 <- cfMake2(mod1_f, wb, daylighth=seq(8, 16, by=0.01),
                    uw=c(TRUE,FALSE))
mod1_res4 <- loglinsimev(mod1_cf4, mod1_sb) # Run the sim
mod1_tidy4 <- fortify.longsim(mod1_res4, mod1_cf4)

m1c4 <- ggplot(mod1_tidy4, aes(x=daylighth, y=pe, ymin=lower,
                               ymax=upper, fill=uw)) +
        geom_line() + geom_ribbon(alpha=0.3) +
        xlab("Daylight (hours)") + ylab("Bicycles") + theme_bw()

ggsave(file="m1c4.pdf", path="fig", width=7, height=4, units=("in"))
ggsave(file="m1c4-sm.pdf", path="fig", width=3.5, height=2, units=("in"))


## cf5 - General trend over time
mod1_cf5 <- cfMake2(mod1_f, wb, X=1:730)
mod1_res5 <- loglinsimev(mod1_cf5, mod1_sb) # Run the sim
mod1_tidy5 <- fortify.longsim(mod1_res5, mod1_cf5)

m1c5 <- ggplot(mod1_tidy5, aes(x=X, y=pe, ymin=lower,
                               ymax=upper)) +
        geom_line() + geom_ribbon(alpha=0.3) +
        xlab("Day of study period") + ylab("Bicycles") + theme_bw()

ggsave(file="m1c5.pdf", path="fig", width=7, height=3.5, units=("in"))
ggsave(file="m1c5-sm.pdf", path="fig", width=3.5, height=1.75, units=("in"))
