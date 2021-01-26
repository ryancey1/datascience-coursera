## rm(list = ls(pattern = "[^stormData | ^eventTypes]"))

library(dplyr)
library(R.utils)
library(stringr)
library(stringdist)
library(lubridate)

setwd("4-reproducible-research/week4/")

if (!dir.exists("data")) {
        dir.create("data")
}

## Download files
# Data Files
if(!file.exists("data/repdata-data-StormData.csv.bz2"))
download.file(
        url = "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2",
        destfile = "data/repdata-data-StormData.csv.bz2"
)

# Document Files
if(!file.exists("StormData-Documentation.pdf"))
download.file(
        url = "https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf",
        destfile = "StormData-Documentation.pdf"
)

bunzip2(
        filename = "data/repdata-data-StormData.csv.bz2", 
        destname = "data/repdata-data-StormData.csv",
        skip = TRUE, 
        remove = FALSE
)

stormData <- read.csv("data/repdata-data-StormData.csv")
eventTypes <- read.delim("data/events.txt")

# filter dates ----
fixed.BGN_DATE <- stormData$BGN_DATE %>%
        str_split(pattern = " ", simplify = TRUE)

# segment data ----
# pre/post 1993
stormData.pre1993 <- stormData %>%
        mutate(BGN_DATE = mdy(fixed.BGN_DATE[,1])) %>%
        filter (BGN_DATE <= "1992-12-31")

stormData.post1993 <- stormData %>%
        mutate(BGN_DATE = mdy(fixed.BGN_DATE[,1])) %>%
        filter(BGN_DATE >= "1993-01-01")

rm(fixed.BGN_DATE)

# remove leading/lagging spaces in EVTYPE ----
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
stormData.pre1993$EVTYPE <- gsub("TSTM WIND", "THUNDERSTORM WIND", stormData.pre1993$EVTYPE)
table(stormData.pre1993$EVTYPE %in% eventTypes$Event.Name)
# now all are matching Valid Event Name

# let's check out the status of post1993 and attempt to correct spelling errors ----
table(stormData.post1993$EVTYPE %in% eventTypes$Event.Name) # not matching over 175,000 entries

# some quick gsub and filters ----
# TSTM is commonly abbreviated for Thunderstorm, let's fix this.
stormData.post1993$EVTYPE <- gsub("TSTM", "THUNDERSTORM", stormData.post1993$EVTYPE)
table(stormData.post1993$EVTYPE %in% eventTypes$Event.Name) # still not matching nearly 41,000 entries

# some are "day summaries", do those appear in both?
table(grepl("SUMMARY", stormData.post1993$EVTYPE)) # yes; 76 summaries
SUMMARY <- grep("SUMMARY", stormData.post1993$EVTYPE)
stormData.post1993[SUMMARY, "EVTYPE"]

# remove summary data ----
stormData.post1993 <- stormData.post1993[-(SUMMARY),]
rm(SUMMARY)

# unbiased function to tidy names (try not to introduce bias) ----
events <- eventTypes$Event.Name
EVTYPE_clone_cleaned <- stormData.post1993$EVTYPE

for (i in seq_along(events)) {
        # generate regex key
        regex <- str_sub(events[i], end = 3)
        
        # get indices of matches
        found <- grep(regex, EVTYPE_clone_cleaned)
        
        # pull those values
        matches <- EVTYPE_clone_cleaned[found] # match "marine" without throwing out spelling errors
        
        # pull the event names
        table <- events[grep(regex, events)]
        
        # fuzzy match and store the keys
        keys <- amatch(matches, table, method = "jw", maxDist = 0.3)
        
        # replace EVTYPE with cleaned value
        for (j in seq_along(keys)) {
                if(is.na(keys[j])) {
                        EVTYPE_clone_cleaned[found[j]] <- NA
                }
                else {
                        EVTYPE_clone_cleaned[found[j]] <- table[keys[j]]
                }
        }
}
summary(EVTYPE_clone_cleaned %in% events)

# move cleaned EVTYPEs into original data frame ----
stormData.post1993$EVTYPE <- EVTYPE_clone_cleaned

# remove NA
stormData.post1993 <- filter(stormData.post1993, !is.na(EVTYPE))
matched <- (stormData.post1993$EVTYPE %in% eventTypes$Event.Name)

# finalize filtered data
stormData.post1993 <- stormData.post1993[matched, ]

# reveal filtered set
summary(stormData.post1993$EVTYPE %in% eventTypes$Event.Name)

# check EVTYPES one more time ----
unique(stormData.post1993$EVTYPE)
unique(stormData.pre1993$EVTYPE)

# clean up environnment
rm(list = ls(pattern = "[^1993]$"))

# which columns do we need to remove? ----
names(stormData.post1993)










