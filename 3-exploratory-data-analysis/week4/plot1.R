# plot1.R -----------------------------------------------------------------
# 1. Have total emissions from PM2.5 decreased in the United States from 1999 to
# 2008? Using the `base` plotting system, make a plot showing the total PM2.5
# emission from all sources for each of the years 1999, 2002, 2005, and 2008.

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
    file = "exdata_data_NEI_data.zip"
    unzip(zipfile = file, exdir = "data/")
}

# read in RDS files
if (!any(ls() == "NEI"))
    NEI <- readRDS(file = "data/summarySCC_PM25.rds")
if (!any(ls() == "SCC"))
    SCC <- readRDS(file = "data/Source_Classification_Code.rds")

# PLOT --------------------------------------------------------------------
# prepare data frame and variables for plot
plot <- NEI %>%
    select(Emissions, year) %>%
    group_by(year) %>%
    summarize(across(.fns = sum), .groups = "keep")
colors = c("red", "blue", "purple", "green")
lm <- lm(Emissions ~ year, data = plot)

# plot to PNG
png("plots/plot1.png")
bar <- barplot(
    Emissions ~ year,
    data = plot,
    col = colors,
    ylab = "PM2.5 Emissions (tons)",
    xlab = "",
    main = "Total PM2.5 Emissions in the United States\n(1999 - 2008)"
)
lines(x = bar, y = lm$fitted.values, lty = 3, lwd = 2, col = "black")
legend("topright", legend = "Linear Regression", lty = 3, lwd = 2, col = "black", cex = 0.7, bty = "o")
text(x = bar, y = plot$Emissions - 1.5e6, labels = as.integer(plot$Emissions), cex = 0.8)
dev.off()

## clean up working environment
rm(list = ls(pattern = "[^NEI|^SCC]"))
