setwd("2-getting-and-cleaning-data/week4")
if(!file.exists("data")) dir.create("data")

## Question 1
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
download.file(url, "data/idaho.csv")
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FPUMSDataDict06.pdf"
download.file(url, "data/idaho_metadata.pdf")

idaho <- read.csv("data/idaho.csv")
split <- strsplit(names(idaho), "wgtp")
split[[123]]

## Question 2
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
download.file(url, "data/GDP.csv")
read.csv("data/GDP.csv", nrows = 10, header = FALSE)

# skip first 5 rows and read without headers
gdp <- read.csv("data/GDP.csv", header = FALSE, skip = 5, na.strings = c("NA", ""))

# clean up raw
str(gdp)
head(gdp)
tail(gdp) ## need to remove footnotes and excess columns 

# subset and remove NAs
gdp <- gdp[,c(1,2,4,5)] # remove excess columns
colnames(gdp) <- c("CountryCode", "Rank", "FullName", "GDP")

# remove unranked
ranked = !is.na(gdp$Rank)
gdp <- gdp[ranked,]

# remove those without GDP
has.gdp = !is.na(gdp$GDP)
gdp <- gdp[has.gdp,]

# remove commas from GDP and return the mean
GDPs <- gdp$GDP %>%
        str_trim %>%
        gsub(pattern = ",", replacement = "") %>%
        as.numeric %>%
        mean %>%
        print

## Question 3
countryNames <- gdp$FullName
length(grep("^United", countryNames))

## Question 4
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
download.file(url, "data/GDP2.csv")
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"
download.file(url, "data/edu_data.csv")

read.csv("data/GDP2.csv", header=FALSE, nrows = 10)
gdp <- read.csv("data/GDP2.csv", header=FALSE, skip=5, na.strings = c("NA", ""))


# clean up raw
str(gdp)
head(gdp)
tail(gdp) ## need to remove footnotes and excess columns 

# subset and remove NAs
gdp <- gdp[,c(1,2,4,5)] # remove excess columns
colnames(gdp) <- c("CountryCode", "Rank", "FullName", "GDP")

# remove unranked
ranked = !is.na(gdp$Rank)
gdp <- gdp[ranked,]

# remove those without GDP
has.gdp = !is.na(gdp$GDP)
gdp <- gdp[has.gdp,]

edu_data <- read.csv("data/edu_data.csv", header=TRUE)
matched <- merge(gdp, edu_data, by = "CountryCode")

grep("Fiscal year end: June", matched$Special.Notes, value = TRUE, fixed = TRUE)

## Question 5
install.packages("quantmod")
install.packages("lubridate")
library(quantmod); library(lubridate)
amzn = getSymbols("AMZN",auto.assign=FALSE)
sampleTimes = index(amzn)

twelve <- grep("2012", sampleTimes)
length(twelve) ## 250
weekdays <- wday(sampleTimes[twelve], label = TRUE)
sum(weekdays == "Mon") ## 47
