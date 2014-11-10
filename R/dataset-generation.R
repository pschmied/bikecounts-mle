# Requires several libraries: lubridate, rjson, plyr, RCurl
# install.packages(c("lubridate", "rjson", "plyr", "RCurl"))
library(lubridate)
library(rjson)
library(plyr)
library(RCurl)

# Forecast.io formatted datestrings for the period of interest
fdates <- strftime(dmy("31 October 2013") + ddays(1:730),
                   format="%Y-%m-%dT%H:%M:%S")

apikey <- "[key pasted here]" # See developer.forecast.io

dfremont <- function(datetimestr, apikey) {
  # Takes a specially formatted string YYYY-MM-DDTHH:MM:SS,
  # and an API key. Fetches historical data for Fremont Bridge.
  # Hackishly returns a one-observation data.frame.
  fmturl <- paste("https://api.forecast.io/forecast/",
                  apikey,
                  "/47.65,-122.35,",
                  datetimestr,
                  "?exclude=currently,hourly,minutely,flags,alerts",
                  sep="")
  fmturl
  res <- fromJSON(getURL(fmturl)) # make query
  do.call(cbind.data.frame, res$daily$data[[1]])
}

weatherres <- ldply(fdates, dfremont, apikey)

# read counter data from Seattle.gov
bikeurl <- "https://data.seattle.gov/api/views/65db-xm6k/rows.csv"
bikecounts <- read.csv(text = getURL(bikeurl), header=TRUE)

# Convert / truncate datetimes. Don't fret timezone, aggregating daily
bikecounts$Date <- gsub("\\s.+", "",
                        as.character(mdy_hms(bikecounts$Date)))

# Aggregate daily bike counts, add NB and SB data, divide by 2
# (rationale being that most of these are 2-way commutes)
bikedaily <- ddply(bikecounts, .(Date), summarize,
                   count=sum(Fremont.Bridge.NB,
                             Fremont.Bridge.SB,
                             na.rm = TRUE) / 2)

# Convert weatherdata's dates to a joinable date format
weatherres$Date <- gsub("T.+", "", fdates)

# Join the weather and bike counts data.frames
weatherbike <- merge(bikedaily, weatherres, by="Date")