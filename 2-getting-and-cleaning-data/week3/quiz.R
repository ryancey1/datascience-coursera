setwd("./2-getting-and-cleaning-data/week3")
if(!file.exists("./data")) dir.create("./data")

## Question 1
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
url2 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FPUMSDataDict06.pdf"

if(!file.exists("data/idaho_variables.pdf")) download.file(url2, destfile = "data/idaho_variables.pdf")

if(!file.exists("data/idaho.csv")) download.file(url, destfile = "data/idaho.csv")
idaho <- read.csv("data/idaho.csv")
head(idaho)

agricultureLogical <- (idaho$ACR == 3 & idaho$AGS == 6)
which(agricultureLogical)[1:3]

## Question 2
library(jpeg)
jpeg <- "https://d396qusza40orc.cloudfront.net/getdata%2Fjeff.jpg"
if(!file.exists("data/pic.jpeg")) download.file(jpeg, destfile = "data/pic.jpeg")
pic <- readJPEG("data/pic.jpeg", native = TRUE)
quantile(pic, probs = c(0.30, 0.80))

# library(dplyr)
# pic <- readJPEG("data/pic.jpeg", native = TRUE) %>%
#         quantile(probs = c(0.3, 0.8)) %>%
#         print

## Question 3
library(dplyr)
library(readr)
url1 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
if(!file.exists("data/GDP.csv")) download.file(url1, destfile = "data/GDP.csv")
url2 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"
if(!file.exists("data/fedstats.csv")) download.file(url2, destfile = "data/fedstats.csv")
read.csv("data/fedstats.csv", nrows=5)
fedstats <- read.csv("data/fedstats.csv")
read.csv("data/GDP.csv", nrows = 5, header=FALSE, skip = 5) # skips 5 lines
gdp <- read.csv("data/GDP.csv", header = FALSE, skip = 5)
gdp <- gdp[,c(1,2,4,5)] # only need cols 1,2,4,5
head(gdp)
colnames(gdp) <- c("CountryCode", "Rank", "FullName","GDP")
gdp <- gdp[(gdp$CountryCode != ""),] # filter out entries with missing country codes
gdp <- gdp[1:190,] # filter out those without rank
str(gdp)
## Re-format number columns
gdp$Rank <- parse_number(gdp$Rank)
gdp$GDP <- parse_number(gdp$GDP)
str(gdp)

table(gdp$CountryCode %in% fedstats$CountryCode)
by_rank <- arrange(gdp, desc(Rank), FullName)
by_rank[13, "FullName"]

## Question 4
merged = merge(gdp, fedstats, by = "CountryCode")
by_income <- group_by(merged, Income.Group)
meanRank <- dplyr::summarize(by_income, mean(Rank))
meanRank[1:2,]

## Question 5
library(Hmisc)
merged$rankGroups <- cut2(merged$Rank, g=5)
table(merged$Income.Group, merged$rankGroups)
