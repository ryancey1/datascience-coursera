setwd("./2-getting-and-cleaning-data/week3/")

if(!file.exists("data")) {
        dir.create("data")
}

url <- "https://data.baltimorecity.gov/api/views/k5ry-ef3g/rows.csv?accessType=DOWNLOAD"
download.file(url, destfile = "data/restaurants.csv", method = "curl")

restData <- read.csv("data/restaurants.csv", stringsAsFactors = TRUE)
## remove last 3 columns
restData <- restData[, 1:6]

## look at dataset
head(restData, n=3)
tail(restData, n=3)
summary(restData) ## note that "min." in zip code has a negative value?
str(restData)
## quantiles can have default probs or specified probs
quantile(restData$councilDistrict, na.rm=TRUE)
quantile(restData$councilDistrict, probs=c(0.5,0.75,0.9))

## interrogate interesting variables with table (negative zipcode?)
table(restData$zipCode, useNA="ifany") ## neg zipcode only has one val
table(restData$councilDistrict, restData$zipCode) ## can do 2D tables as well

## check for NAs in specific variables
sum(is.na(restData$councilDistrict))
any(is.na(restData$councilDistrict))
all(restData$zipCode > 0) ## "FALSE" due to negative zipcode
colSums(is.na(restData)) ## checks for NAs across whole dataset
all(colSums(is.na(restData)) == 0) 
