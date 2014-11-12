## Requires several libraries: lubridate, rjson, plyr, RCurl
## install.packages(c("lubridate", "rjson", "plyr", "RCurl"))
library(lubridate)
library(rjson)
library(plyr)
library(RCurl)

## Forecast.io formatted datestrings for the period of interest
fdates <- strftime(dmy("31 October 2012") + ddays(1:730),
                   format="%Y-%m-%dT%H:%M:%S")

## apikey <- "[key pasted here]" # See developer.forecast.io

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

## read counter data from Seattle.gov
bikeurl <- "https://data.seattle.gov/api/views/65db-xm6k/rows.csv"
bikecounts <- read.csv(text = getURL(bikeurl), header=TRUE)

## Convert / truncate datetimes. Don't fret timezone, aggregating daily
bikecounts$Date <- gsub("\\s.+", "",
                        as.character(mdy_hms(bikecounts$Date)))

## Aggregate daily bike counts, add NB and SB data
bikedaily <- ddply(bikecounts, .(Date), summarize,
                   count=sum(Fremont.Bridge.NB,
                             Fremont.Bridge.SB,
                             na.rm = TRUE))

## Convert weatherdata's dates to a joinable date format
weatherres$Date <- gsub("T.+", "", fdates)

## Join the weather and bike counts data.frames
weatherbike <- merge(bikedaily, weatherres, by="Date")

## Beyond here lies jankiness and inefficient date manipulations
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## List of holidays (as observed by UW). Manually scraped from:
## http://www.washington.edu/admin/hr/holidays/holidays.html and
## http://www.washington.edu/admin/hr/holidays/holidays-archive.html
weatherbike$holiday <- ymd(weatherbike$Date) %in% mdy(c("1/2/12", "1/16/12", "2/20/12", "5/28/12", "7/4/12", "9/3/12", "11/12/12", "11/22/12", "11/23/12", "12/25/12", "1/1/13", "1/21/13", "2/18/13", "5/27/13", "7/4/13", "9/2/13", "11/11/13", "11/28/13", "11/29/13", "12/25/13", "1/1/14", "1/20/14", "2/17/14", "5/26/14", "7/4/14", "9/1/14", "11/11/14", "11/27/14", "11/28/14", "12/25/14"))


## UW in regular (non-summer) session? Manually scraped from UW
## academic calendar "Dates of Instruction" archives. Includes finals
## week, though that may be unjustified.
uwstartdates <- mdy(c("1/3/2012", "3/26/2012", "9/24/2012", "1/7/2013", "4/1/2013", "9/25/2013", "1/6/2014", "3/31/2014", "9/24/2014"))
uwstopdates <- mdy(c("3/16/2012", "6/8/2012", "12/14/2012", "3/22/2013", "6/14/2013", "12/13/2013", "3/21/2014", "6/13/2014", "12/12/2014"))
uwdur <- uwstopdates - uwstartdates
uwinst <- unlist(mapply(function(s,d) as.character(s + ddays(1:d)),
                        uwstartdates, uwdur))
weatherbike$uw <- ymd(weatherbike$Date) %in% ymd(uwinst)

