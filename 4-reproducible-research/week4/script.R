## rm(list = ls(pattern = "[^stormData | ^eventTypes]"))

library(dplyr)
library(R.utils)
library(stringr)
library(stringdist)
library(lubridate)
library(ggplot2)

setwd("4-reproducible-research/week4/")

if (!dir.exists("data")) {
        dir.create("data")
}

# download files ----
# data Files
if (!file.exists("data/repdata-data-StormData.csv.bz2")) {
        download.file(url = "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2",
                      destfile = "data/repdata-data-StormData.csv.bz2")
}
# document Files
if (!file.exists("StormData-Documentation.pdf")) {
        download.file(url = "https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf",
                      destfile = "StormData-Documentation.pdf")
}

bunzip2(
        filename = "data/repdata-data-StormData.csv.bz2",
        destname = "data/repdata-data-StormData.csv",
        skip = TRUE,
        remove = FALSE
)

# read in data ----
stormData <- read.csv("data/repdata-data-StormData.csv")
eventTypes <- read.delim("data/events.txt")

names(stormData)

# remove unnecessary columns
stormData2 <- select(stormData, STATE__, BGN_DATE, BGN_TIME, EVTYPE, FATALITIES, 
                     INJURIES, PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP)

# filter dates ----
fixed.BGN_DATE <- stormData2$BGN_DATE %>%
        str_split(pattern = " ", simplify = TRUE)

# segment data ----
# pre/post 1993
stormData.pre1993 <- stormData2 %>%
        mutate(BGN_DATE = mdy(fixed.BGN_DATE[, 1])) %>%
        filter (BGN_DATE <= "1992-12-31")

stormData.post1993 <- stormData2 %>%
        mutate(BGN_DATE = mdy(fixed.BGN_DATE[, 1])) %>%
        filter(BGN_DATE >= "1993-01-01")

rm(fixed.BGN_DATE, stormData, stormData2)

# remove leading/lagging spaces in EVTYPE & Event.Name ----
stormData.pre1993$EVTYPE <- stormData.pre1993$EVTYPE %>%
        str_to_upper() %>%
        str_trim() %>%
        str_squish()
stormData.pre1993 <- arrange(stormData.pre1993, BGN_DATE, BGN_TIME)

stormData.post1993$EVTYPE <- stormData.post1993$EVTYPE %>%
        str_to_upper() %>%
        str_trim() %>%
        str_squish()
stormData.post1993 <- arrange(stormData.post1993, BGN_DATE, BGN_TIME)

eventTypes$Event.Name <- eventTypes$Event.Name %>%
        str_to_upper() %>%
        str_trim() %>%
        str_squish()

# pre-1993 data ----
# how many match this format? ----
table(stormData.pre1993$EVTYPE %in% eventTypes$Event.Name)
# what are the unique EVTYPEs? ----
unique(stormData.pre1993$EVTYPE)
# [1] "TORNADO"   "HAIL"      "TSTM WIND"

# according to NWS (where "marine" is classified), whether or not an area is "marine" is designated
# by the state code (https://www.weather.gov/marine/nws_dissemination)
marineCodes <- c(73, 75, 77, 57, 58, 59, 65, 61, 91, 92, 93, 94, 96, 97, 98)
table(stormData.pre1993$STATE__ %in% marineCodes) # FALSE 187559
table(stormData.post1993$STATE__ %in% marineCodes) # FALSE 712974 TRUE 1688
# none match, so all TSTM can be "THUNDERSTORM WIND"

# we only need to modify "TSTM WIND" to "THUNDERSTORM WIND" or "MARINE THUNDERSTORM WIND" to match ----
stormData.pre1993$EVTYPE <-
        gsub("TSTM WIND", "THUNDERSTORM WIND", stormData.pre1993$EVTYPE)
table(stormData.pre1993$EVTYPE %in% eventTypes$Event.Name)
# now all are matching Valid Event Name

# post-1993 data ----
# let's check out the status of post1993 and attempt to correct spelling errors ----
table(stormData.post1993$EVTYPE %in% eventTypes$Event.Name) # not matching over 175,000 entries

# some quick gsub and filters ----
# "TSTM" was commonly abbreviated for "THUNDERSTORM" in pre-1993 data, how about post-1993?
sum(grepl("TSTM", stormData.post1993$EVTYPE))

# almost 140,000 entries use TSTM instead of THUNDERSTORM, let's fix that
stormData.post1993$EVTYPE <- gsub("TSTM", "THUNDERSTORM", stormData.post1993$EVTYPE)
table(stormData.post1993$EVTYPE %in% eventTypes$Event.Name)

# that one gsub command corrected more than half of the entry errors

# some are "day summaries"
table(grepl("SUMMARY", stormData.post1993$EVTYPE))
stormData.post1993[grep("SUMMARY", stormData.post1993$EVTYPE), "EVTYPE"]

# this data is not useful, remove those rows ----
stormData.post1993 <- stormData.post1993[-(grep("SUMMARY", stormData.post1993$EVTYPE)), ]

# unbiased function to tidy names (try not to introduce bias) ----
events <- eventTypes$Event.Name
EVTYPEs <- stormData.post1993$EVTYPE

# before
summary(EVTYPEs %in% events)

for (i in seq_along(events)) {
        # generate regex key
        regex <- str_sub(events[i], end = 3)
        
        # get indices of matches
        found <- grep(regex, EVTYPEs)
        
        # pull those values
        matches <- EVTYPEs[found]
        
        # pull the event names
        table <- events[grep(regex, events)]
        
        # fuzzy match and store the keys
        keys <- amatch(matches, table, method = "jw", maxDist = 0.3)
        
        # replace EVTYPE with cleaned value
        for (j in seq_along(keys)) {
                if (is.na(keys[j])) {
                        EVTYPEs[found[j]] <- NA
                }
                else {
                        EVTYPEs[found[j]] <- table[keys[j]]
                }
        }
}

# after
summary(EVTYPEs %in% events)

# what percentage are not matching?
sum(!(EVTYPEs %in% events))/length(EVTYPEs)
# only 1.3% will be removed after this filter, that is acceptable

# move cleaned EVTYPEs into original data frame ----
stormData.post1993$EVTYPE <- EVTYPEs

# remove NA EVTYPEs and those not matching certified EVTYPEs
stormData.post1993 <- stormData.post1993 %>%
        filter(!is.na(EVTYPE)) %>%
        filter(EVTYPE %in% events)

# reveal filtered set
summary(stormData.post1993$EVTYPE %in% events)
summary(stormData.pre1993$EVTYPE %in% events)

# check EVTYPES one more time ----
unique(stormData.post1993$EVTYPE)
unique(stormData.pre1993$EVTYPE)

# clean up environnment
rm(list = ls(pattern = "[^1993]$"))

# plot 1 - distribution of EVTYPEs ----
events.post1993 <- stormData.post1993 %>%
        group_by(EVTYPE) %>%
        summarize(count = n(), .groups = "keep")

events.pre1993 <- stormData.pre1993 %>%
        group_by(EVTYPE) %>%
        summarize(count = n(), .groups = "keep")

par(mfrow = c(1,2))
g <- ggplot(events.post1993, aes(count, EVTYPE))
g + geom_col(fill = "red")

barplot(events.post1993$count,
        names.arg = events.post1993$EVTYPE, 
        horiz = TRUE, 
        las = 2, 
        cex.names = 0.5, 
        col = "red")
barplot(events.pre1993$count,
        names.arg = events.pre1993$EVTYPE, 
        horiz = TRUE, 
        las = 2, 
        cex.names = 0.5, 
        col = "red")



