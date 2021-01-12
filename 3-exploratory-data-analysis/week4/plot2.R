# plot2.R -----------------------------------------------------------------
# 2. Have total emissions from PM2.5 decreased in the Baltimore City, Maryland
# (fips == "24510") from 1999 to 2008? Use the `base` plotting system to make
# a plot answering this question.

# HOUSEKEEPING ------------------------------------------------------------
## load libraries
library(dplyr)
library(ggplot2)

## set working directory
if (!grepl("3-exploratory-data-analysis/week4", getwd(), fixed = TRUE)) {
    setwd("3-exploratory-data-analysis/week4")
}

## create directories
if (!dir.exists("data")) {
    dir.create("data")
}

if (!dir.exists("plots")) {
    dir.create("plots")
}

## download & extract RDS files if not already done
if (!file.exists("exdata_data_NEI_data.zip")) {
    zip = "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
    file = "exdata_data_NEI_data.zip"
    download.file(url = zip, destfile = file)
}

if (!file.exists("data/Source_Classification_Code.rds") &
    !file.exists("data/summarySCC_PM25.rds")) {
    unzip(zipfile = file, exdir = "data/")
}

# read in RDS files
if (!any(ls() == "NEI"))
    NEI <- readRDS(file = "data/summarySCC_PM25.rds")
if (!any(ls() == "SCC"))
    SCC <- readRDS(file = "data/Source_Classification_Code.rds")

# PLOT --------------------------------------------------------------------
## filter data frame for base plot
NEI %>%
        select(fips, Emissions, year) %>%
        filter(fips == "24510") %>%
        group_by(year) %>%
        summarize(across(.cols = Emissions, .fns = sum), .groups = "keep") -> result

## base plot
png(filename = "plots/plot2.png")

plot(
    Emissions ~ year,
    data = result,
    type = "o",
    pch = 16,
    lty = 3,
    xlab = "Year",
    ylab = "PM2.5 Emissions (ton)",
    main = "Yearly Total PM2.5 Emissions in Baltimore, MD"
)

## add regression line to plot
abline(
    lm(Emissions ~ year, result),
    col = "red",
    lty = 1,
    lwd = 1
)

dev.off()

## clean up working environment
rm(list = ls(pattern = "[^NEI|^SCC]"))