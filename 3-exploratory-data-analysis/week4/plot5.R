# plot5.R -----------------------------------------------------------------
# 5. How have emissions from motor vehicle sources changed from 1999–2008 in
# Baltimore City?

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
## prepare data frame for ggplot2
NEI %>%
        filter(fips == "24510" & type == "ON-ROAD") %>%
        group_by(year, type) %>%
        summarize(across(.cols = Emissions, .fns = sum), .groups = "keep") -> pm25_veh_baltimore
pm25_veh_baltimore$year <- factor(pm25_veh_baltimore$year)

## ggplot2
g <- ggplot(pm25_veh_baltimore, aes(year, Emissions, fill = type)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(. ~ type, scale = "free") +
    ylab("Emissions (tons)") +
    xlab("Years") +
    ggtitle("PM2.5 Emissions in Baltimore, MD\nVehicle Sources (1999 - 2008)") +
    theme(plot.title = element_text(hjust = 0.5)) +
    geom_smooth(
        aes(as.integer(year), Emissions),
        method = "lm",
        se = FALSE,
        color = "black",
        formula = "y ~ x",
        show.legend = FALSE
    )

## save plot to PNG
png(filename = "plots/plot5.png")
print(g)
dev.off()

## clean up working environment
rm(list = ls(pattern = "[^NEI|^SCC]"))