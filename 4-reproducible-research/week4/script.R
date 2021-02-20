## rm(list = ls(pattern = "[^stormData | ^eventTypes]"))
# START HOUSEKEEPING ----
library(dplyr)
library(R.utils)
library(stringr)
library(stringdist)
library(lubridate)
library(ggplot2)
library(reshape2)


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
if (!file.exists("data/StormData-Documentation.pdf")) {
        download.file(url = "https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf",
                      destfile = "StormData-Documentation.pdf")
}

bunzip2(
        filename = "data/repdata-data-StormData.csv.bz2",
        destname = "data/repdata-data-StormData.csv",
        skip = TRUE,
        remove = FALSE
)

# END HOUSEKEEPING ----

# START CLEANUP ----
# read in data ----
stormData <- read.csv("data/repdata-data-StormData.csv")
eventTypes <- read.delim("data/events.txt")

# remove unnecessary columns ----
names(stormData)
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

rm(fixed.BGN_DATE, stormData)


# STEP 1: CLEAN UP EVTYPE COLUMN ----
# remove leading/lagging spaces in EVTYPE & Event.Name ----
stormData.pre1993$EVTYPE <- stormData.pre1993$EVTYPE %>%
        str_to_upper() %>%
        str_trim() %>%
        str_squish()

stormData.post1993$EVTYPE <- stormData.post1993$EVTYPE %>%
        str_to_upper() %>%
        str_trim() %>%
        str_squish()

eventTypes$EVTYPE <- eventTypes$EVTYPE %>%
        str_to_upper() %>%
        str_trim() %>%
        str_squish()
eventTypes <- arrange(eventTypes, EVTYPE)

events <- eventTypes$EVTYPE

# pre-1993 data ----
# how many match this format? ----
table(stormData.pre1993$EVTYPE %in% events)
# what are the unique EVTYPEs? ----
unique(stormData.pre1993$EVTYPE)
# [1] "TORNADO"   "HAIL"      "TSTM WIND"

# according to NWS (where "marine" is classified), whether or not an area is "marine" is designated
# by the state code (https://www.weather.gov/marine/nws_dissemination)
marineCodes <- c(73, 75, 77, 57, 58, 59, 65, 61, 91, 92, 93, 94, 96, 97, 98)

table(stormData.pre1993$STATE__ %in% marineCodes) # FALSE 187559
# none match, so all TSTM can be "THUNDERSTORM WIND"

# we only need to modify "TSTM WIND" to "THUNDERSTORM WIND" for pre-1993 entries to match ----
stormData.pre1993$EVTYPE <- gsub("TSTM WIND", "THUNDERSTORM WIND", stormData.pre1993$EVTYPE)
table(stormData.pre1993$EVTYPE %in% events)
# now all are matching Valid Event Name

# post-1993 data ----
# let's check out the status of post1993 and attempt to correct spelling errors ----
table(stormData.post1993$EVTYPE %in% events) # not matching over 175,000 entries

# checking "marineCodes" matches ----
table(stormData.post1993$STATE__ %in% marineCodes) # FALSE 712974 TRUE 1688
marine <- stormData.post1993[stormData.post1993$STATE__ %in% marineCodes, "EVTYPE"]
unique(marine) # what are the entries in marineCode states?
unique(marine) %in% events # only "MARINE TSTM WIND" does not match, we will fix that later

# some quick gsub and filters ----
# "TSTM" was commonly abbreviated for "THUNDERSTORM" in pre-1993 data, how about post-1993?
sum(grepl("TSTM", stormData.post1993$EVTYPE))

# almost 140,000 entries use TSTM instead of THUNDERSTORM, let's fix that
stormData.post1993$EVTYPE <- gsub("TSTM", "THUNDERSTORM", stormData.post1993$EVTYPE)
table(stormData.post1993$EVTYPE %in% events)
# that one gsub command corrected more than half of the entry errors

# we can summarize which don't match still
unique(stormData.post1993[!(stormData.post1993$EVTYPE %in% events), "EVTYPE"])
# there are a lot of random entries, some just don't match due to their minor differences, this will be fixed below

# tidy names via unbiased function ----
EVTYPEs <- stormData.post1993$EVTYPE

# before
summary(EVTYPEs %in% events)
# FALSE    TRUE 
# 40821  673917 

for (i in seq_along(events)) {
        rx <- str_sub(events[i], end = 3)
        found <- grep(rx, EVTYPEs) # get indices of matches
        matches <- EVTYPEs[found] # pull those values
        table <- events[grep(rx, events)] # pull the event names
        keys <- amatch(matches, table, method = "jw", maxDist = 0.3) # fuzzy match and store the keys

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
# FALSE    TRUE 
# 9010  705728

# what percentage are not matching?
sum(!(EVTYPEs %in% events))/length(EVTYPEs)
# only 1.3% will be removed after this filter, that is acceptable

# our function above coded non-matching entries as "NA" -- how many are unmatched still? ----
sum(is.na(EVTYPEs))
# [1] 5854
sum(is.na(EVTYPEs))/length(EVTYPEs)
# [1] 0.008190414
# this cleanup was effective 

# we can move cleaned EVTYPEs into original data frame ----
stormData.post1993$EVTYPE <- EVTYPEs

# remove NA EVTYPEs and those not matching certified EVTYPEs
stormData.post1993 <- stormData.post1993 %>%
        filter(!is.na(EVTYPE)) %>%
        filter(EVTYPE %in% events)

# reveal filtered set
summary(stormData.post1993$EVTYPE %in% events)
summary(stormData.pre1993$EVTYPE %in% events)

# bind rows to facilitate easier analysis & add pre-/post- factorization ----
stormData.post1993$pre.post.1993 <- "After 1993"
stormData.pre1993$pre.post.1993 <- "Before 1993"
cleanData <- rbind(stormData.pre1993, stormData.post1993)

# clean up environment
rm(list = setdiff(ls(), c("cleanData", "stormData.pre1993", "stormData.post1993", "stormData2")))

# STEP 2: CLEAN UP PROPDMGEXP COLUMN ----
cleanData2 <- cleanData

unique(cleanData2$PROPDMGEXP)
#  [1] "M" "K" ""  "B" "0" "?" "6" "4" "5" "h" "m" "+" "H" "3" "2" "1" "7" "8" "-"
table(cleanData2$PROPDMGEXP)
# a total of 13 entries have punctuation in the EXP cell, so we can remove without further investigation
punct <- grep("[[:punct:]]", cleanData2$PROPDMGEXP)
cleanData2 <- cleanData2[-(punct), ]

table(cleanData2$PROPDMGEXP)
cleanData2$PROPDMGEXP <- cleanData2$PROPDMGEXP %>%
        str_to_upper() %>%
        str_replace("H", "100") %>%
        str_replace("K", "1000") %>%
        str_replace("M", "1000000") %>%
        str_replace("B", "1000000000") %>%
        as.numeric()

for (i in seq_along(cleanData2$PROPDMGEXP)) {
        if(cleanData2$PROPDMGEXP[i] < 100 & !is.na(cleanData2$PROPDMGEXP[i])) {
                cleanData2$PROPDMG[i] <- paste0(cleanData2$PROPDMG[i], as.character(cleanData2$PROPDMGEXP[i]))
                cleanData2$PROPDMGEXP[i] <- 1
        } else {
                cleanData2$PROPDMGEXP[i] <- 1
        }
}

cleanData2$PDMG <- cleanData2$PROPDMG * cleanData2$PROPDMGEXP

# STEP 3: CLEAN UP CROPDMGEXP COLUMN ----
unique(cleanData2$CROPDMGEXP)
# [1] ""  "K" "M" "?" "0" "B" "k" "2" "m"
table(cleanData2$CROPDMGEXP)
# a total of 13 entries have punctuation in the EXP cell, so we can remove without further investigation
punct <- grep("[[:punct:]]", cleanData2$CROPDMGEXP)
cleanData2 <- cleanData2[-(punct), ]
cleanData2$CROPDMGEXP <- cleanData2$CROPDMGEXP %>%
        str_to_upper() %>%
        str_replace("H", "100") %>%
        str_replace("K", "1000") %>%
        str_replace("M", "1000000") %>%
        str_replace("B", "1000000000") %>%
        as.numeric()

cleanData2$CROPDMG <- as.numeric(cleanData2$CROPDMG)
cleanData2$CDMG <- cleanData2$CROPDMG * cleanData2$CROPDMGEXP
# END CLEANUP ----

# START ANALYSIS ----
# plot 1 - distribution of EVTYPEs ----
EVTYPE.dist <- stormData.combined %>%
        group_by(EVTYPE, pre.post.1993) %>%
        summarize(count = n(), .groups = "keep")

ggplot(EVTYPE.dist, aes(count, reorder(EVTYPE, count), fill = pre.post.1993)) + 
        theme_light() + 
        theme(axis.text.y = element_text(face = "italic")) +
        geom_col(show.legend = FALSE) + 
        facet_grid(. ~ pre.post.1993)

# before 1993, only 3 event types were cataloged

# plot 2 - Across the United States, which types of events are most harmful with respect to population health? ----
# we need to normalize the data by the number of occurrences to determine which types of events result in higher 
# death rates per event
EVTYPE.norm <- stormData.combined %>%
        group_by(EVTYPE) %>%
        summarize(count = n(), 
                  Fatalities = mean(FATALITIES), 
                  Injuries = mean(INJURIES), 
                  TotalHarms = mean(FATALITIES + INJURIES)) %>%
        slice_max(order_by = TotalHarms, n = 10)
EVTYPE.norm$EVTYPE <- str_to_title(EVTYPE.norm$EVTYPE)

melted <- melt(EVTYPE.norm, id.vars = c("EVTYPE", "count"), variable.name = "Occurrence")
        
ggplot(subset(melted, Occurrence != "TotalHarms"), aes(y = value, x = reorder(EVTYPE, -value), fill = Occurrence)) + 
        geom_col() + 
        theme_light() +
        theme(axis.text.x=element_text(angle = 45, hjust = 1, face = "bold"), 
              plot.title = element_text(hjust = 0.5, face = "bold"),
              plot.subtitle = element_text(hjust = 0.5),
              plot.caption = element_text(face = "italic")) + 
        ylab("Injuries & Fatalities (per event)") + 
        xlab(NULL) +
        labs(title = "Most Harmful Events Across the USA",
             subtitle = "(1950 - 2011)",
             caption = "NWS Instruction 10-1605 (2007-08-17)")

# plot 3 - Across the United States, which types of events have the greatest economic consequences? ----
