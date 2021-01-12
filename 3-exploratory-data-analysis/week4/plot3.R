# plot3.R -----------------------------------------------------------------
# 3. Of the four types of sources indicated by the`type` (point, nonpoint,
# onroad, nonroad) variable, which of these four sources have seen decreases in
# emissions from 1999–2008 for Baltimore City? Which have seen increases in
# emissions from 1999–2008? Use the `ggplot2` plotting system to make a plot
# answer this question.

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
## prepare data frame for ggplot2
NEI %>%
        select(fips, Emissions, type, year) %>%
        filter(fips == "24510") %>%
        group_by(year, type) %>%
        summarize(across(.cols = Emissions, .fns = sum),
                  .groups = "keep") -> pm25_baltimore
pm25_baltimore$year <- factor(pm25_baltimore$year)

## ggplot2
g <-
    ggplot(pm25_baltimore, aes(x = year, y = Emissions, fill = type)) +
    theme_bw() +
    geom_col(show.legend = FALSE) +
    facet_wrap(. ~ type, scale = "free", nrow = 1) +
    xlab("Year") +
    ylab("Emissions (tons)") +
    ggtitle("Yearly Source-Separated Total PM2.5 Emissions in Baltimore, MD") +
    theme(plot.title = element_text(hjust = 0.5)) +
    geom_smooth(
        aes(x = as.integer(year), y = Emissions),
        method = "lm",
        se = FALSE,
        color = "red",
        formula = "y ~ x",
        show.legend = FALSE
    )

## save plot to PNG
png(filename = "plots/plot3.png")
print(g)
dev.off()

## clean up working environment
rm(list = ls(pattern = "[^NEI|^SCC]"))
