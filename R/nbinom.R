library(ggplot2)
library(plyr)
library(lubridate)
library(dplyr)
library(simcf)                          # needs github/pschmied fork
library(pscl)


## Load the saved dataset
wb <- read.csv("data/weatherbike1yr.csv",
               header=T, stringsAsFactors=FALSE)

##
## Useful recodings
##
wb$dow <- wday(wb$Date, label=TRUE, abbr=TRUE) # Day of Week
wb$daylight <- wb$sunsetTime - wb$sunriseTime  # Min of daylight


##
## Estimate model 1
##
mod1_f <- count ~ dow + holiday + UW + daylight + 
    temperatureMax + precip + cloudCover + x
mod1_m <- glm.nb(mod1_f, data=wb)
