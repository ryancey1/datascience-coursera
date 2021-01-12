# plot6.R -----------------------------------------------------------------
# 6. Compare emissions from motor vehicle sources in Baltimore City with
# emissions from motor vehicle sources in Los Angeles County, California (`fips
# == "06037"`). Which city has seen greater changes over time in motor vehicle
# emissions?

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
        filter(fips == "24510" | fips == "06037") %>%
        filter(type == "ON-ROAD") %>%
        group_by(fips, year) %>%
        summarize(across(.cols = Emissions, .fns = sum), .groups = "keep") %>%
        mutate(fips = case_when(
            fips == "24510" ~ "Baltimore City",
            fips == "06037" ~ "Los Angeles",
            TRUE ~ fips
        )) -> pm25_veh_baltimore
pm25_veh_baltimore$year <- factor(pm25_veh_baltimore$year)


## ggplot2
g <- ggplot(pm25_veh_baltimore, aes(year, Emissions, fill = fips)) +
    theme_bw() +
    geom_col(show.legend = FALSE) +
    facet_wrap(. ~ fips, scale = "free_y") +
    ylab("Emissions (tons)") +
    xlab("Years") +
    ggtitle("Yearly Motor Vehicle PM2.5 Emissions in Baltimore and Los Angeles") +
    theme(plot.title = element_text(hjust = 0.5)) +
    geom_smooth(
        aes(as.integer(year), Emissions),
        method = "lm",
        se = FALSE,
        color = "red",
        formula = "y ~ x",
        show.legend = FALSE
    )

## save plot to PNG
png(filename = "plots/plot6.png")
print(g)
dev.off()

## clean up working environment
rm(list = ls(pattern = "[^NEI|^SCC]"))
