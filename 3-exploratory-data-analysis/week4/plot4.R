# plot4.R -----------------------------------------------------------------
# 4. Across the United States, how have emissions from coal combustion-related
# sources changed from 1999–2008?

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
## find SCC codes matching "coal combustion"
comb <- grepl("[Cc]omb*", SCC$SCC.Level.One)
coal <- grepl("[Cc]oal", SCC$SCC.Level.Three)
SCC_coal_comb <- filter(SCC, comb == TRUE & coal == TRUE)
SCC_code <- as.character(SCC_coal_comb$SCC)

## map to NEI SCCs
filter <- NEI$SCC %in% SCC_code

## prepare data frame for ggplot2
NEI_SCC <- NEI %>%
    filter(filter == TRUE) %>%
    group_by(year) %>%
    summarize(across(.cols = Emissions, .fns = sum), .groups = "keep")
NEI_SCC$year <- factor(NEI_SCC$year)

## ggplot2
g <- ggplot(NEI_SCC, aes(year, Emissions)) +
    geom_col(show.legend = FALSE) +
    ylab("Emissions (tons)") +
    xlab("Years") +
    ggtitle("PM2.5 Emissions in the United States\nCoal Sources (1999 - 2008)") +
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
png(filename = "plots/plot4.png")
print(g)
dev.off()

## clean up working environment
rm(list = ls(pattern = "[^NEI|^SCC]"))
