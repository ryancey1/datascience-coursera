setwd("3-exploratory-data-analysis/week4")

library(dplyr)

if (!dir.exists("data")) {
    dir.create("data")
}

if (!file.exists("exdata_data_NEI_data.zip")) {
    zip = "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
    file = "exdata_data_NEI_data.zip"
    download.file(url = zip, destfile = file)
}


if (!file.exists("data/Source_Classification_Code.rds") &
    !file.exists("data/summarySCC_PM25.rds")) {
    unzip(zipfile = file, exdir = "data/")
}

# Read in RDS files
NEI <- readRDS(file = "data/summarySCC_PM25.rds")
SCC <- readRDS(file = "data/Source_Classification_Code.rds")

# plot1.R -----------------------------------------------------------------
# 1. Have total emissions from PM2.5 decreased in the United States from 1999 to
# 2008? Using the `base` plotting system, make a plot showing the total PM2.5
# emission from all sources for each of the years 1999, 2002, 2005, and 2008.
head(NEI)

NEI %>% 
    select(Emissions, year) %>% 
    group_by(year) %>%
    summarize(across(.fns = sum), .groups = "keep") -> result

result$Emissions_kiloton <- (result$Emissions / 1E6)

plot(
    Emissions_kiloton ~ year,
    data = result,
    type = "o",
    pch = 16,
    lty = 1,
    ylab = "Emissions (kilotons)",
    xlab = "Year",
    axes = FALSE
)

axis(1, at=unique(result$year)); axis(2); box()

abline(lm(Emissions_kiloton ~ year, result), col = "blue", lty = 3, lwd = 2)

rm(list = ls(pattern = "[^NEI|^SCC]"))

# plot2.R -----------------------------------------------------------------
# 2. Have total emissions from PM2.5 decreased in the Baltimore City, Maryland
# (fips == "24510") from 1999 to 2008? Use the `base` plotting system to make
# a plot answering this question.
head(NEI)

NEI %>%
    select(fips, Emissions, year) %>%
    filter(fips == "24510") %>%
    group_by(year) %>%
    summarize(across(.cols = Emissions, .fns = sum)) -> result
    
fips_24510 %>%
    group_by(year) %>%
    summarize(across(.cols = Emissions, .fns = sum)) -> result

plot(
    Emissions ~ year,
    data = result,
    type = "o",
    pch = 16,
    lty = 2,
    xlab = "Year",
    ylab = "PM2.5 Emissions (ton)",
    main = "Annual PM2.5 Emissions in Baltimore, MD"
)

abline(lm(Emissions ~ year, result), col = "red", lwd = 2)

rm(list = ls(pattern = "[^NEI|^SCC]"))

# plot3.R -----------------------------------------------------------------
# 3. Of the four types of sources indicated by the`type` (point, nonpoint,
# onroad, nonroad) variable, which of these four sources have seen decreases in
# emissions from 1999–2008 for Baltimore City? Which have seen increases in
# emissions from 1999–2008? Use the `ggplot2` plotting system to make a plot
# answer this question.
library(ggplot2)

head(NEI)

NEI %>%
    select(fips, Emissions, type, year) %>%
    filter(fips == "24510") %>%
    group_by(year, type) %>%
    summarize(across(.cols = Emissions, .fns = sum), 
              .groups = "keep") -> pm25_baltimore

pm25_baltimore$year <- factor(pm25_baltimore$year)

ggplot(pm25_baltimore, aes(x = year, y = Emissions, fill = type)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(. ~ type, scale = "free", nrow = 1) +
    xlab("Year") +
    ylab("Emissions (tons)") +
    geom_smooth(
        aes(x = as.integer(year), y = Emissions),
        method = "lm",
        se = FALSE,
        color = "black",
        formula = "y ~ x",
        show.legend = FALSE
    )
dev.copy(png, file = "plot3.png")
dev.off()

rm(list = ls(pattern = "[^NEI|^SCC]"))

# plot4.R -----------------------------------------------------------------
# 4. Across the United States, how have emissions from coal combustion-related
# sources changed from 1999–2008?
library(ggplot2)
## find SCC codes matching "coal combustion"
comb <- grepl("[Cc]omb*", SCC$SCC.Level.One)
coal <- grepl("[Cc]oal", SCC$SCC.Level.Three)
SCC_coal_comb <- filter(SCC, comb == TRUE & coal == TRUE)
SCC_code <- as.character(SCC_coal_comb$SCC)

# generate logical vector of matching NEI measurements by SCC code
filter <- NEI$SCC == SCC_code

NEI %>%
    filter(filter == TRUE) %>%
    group_by(type, year) %>%
    summarize(across(.cols = Emissions, .fns = sum), .groups = "keep") -> NEI_SCC

NEI_SCC$year <- factor(NEI_SCC$year)

ggplot(NEI_SCC, aes(year, Emissions, fill = type)) + 
    theme_bw() +
    geom_col(show.legend = FALSE) + 
    facet_wrap(. ~ type, scale = "free") +
    ylab("Emissions (tons)") +
    xlab("Years") +
    ggtitle("Source-Separated Annual PM2.5 Coal-Combustion Emissions") +
    theme(plot.title = element_text(hjust = 0.5)) +
    geom_smooth(
        aes(as.integer(year), Emissions), 
        method = "lm", 
        se = FALSE,
        color = "black",
        formula = "y ~ x",
        show.legend = FALSE)

rm(list = ls(pattern = "[^NEI|^SCC]"))


# plot5.R -----------------------------------------------------------------
# 5. How have emissions from motor vehicle sources changed from 1999–2008 in
# Baltimore City?

NEI %>%
    filter(fips == "24510" & type == "ON-ROAD") %>%
    group_by(year, type) %>%
    summarize(across(.cols = Emissions, .fns = sum), .groups = "keep") -> pm25_veh_baltimore

pm25_veh_baltimore$year <- factor(pm25_veh_baltimore$year)

ggplot(pm25_veh_baltimore, aes(year, Emissions, fill = type)) + 
    theme_bw() +
    geom_col(show.legend = FALSE) + 
    facet_wrap(. ~ type, scale = "free") +
    ylab("Emissions (tons)") +
    xlab("Years") +
    ggtitle("Annual Motor Vehicle PM2.5 Emissions in Baltimore, MD") +
    theme(plot.title = element_text(hjust = 0.5)) +
    geom_smooth(
        aes(as.integer(year), Emissions), 
        method = "lm", 
        se = FALSE,
        color = "black",
        formula = "y ~ x",
        show.legend = FALSE)

rm(list = ls(pattern = "[^NEI|^SCC]"))


# plot6.R -----------------------------------------------------------------
# 6. Compare emissions from motor vehicle sources in Baltimore City with
# emissions from motor vehicle sources in Los Angeles County, California (`fips
# == "06037"`). Which city has seen greater changes over time in motor vehicle
# emissions?

NEI %>%
    filter(fips == "24510" | fips == "06037") %>%
    filter(type == "ON-ROAD") %>%
    group_by(fips, year) %>%
    summarize(across(.cols = Emissions, .fns = sum), .groups = "keep") -> pm25_veh_baltimore

pm25_veh_baltimore$year <- factor(pm25_veh_baltimore$year)

ggplot(pm25_veh_baltimore, aes(year, Emissions, fill = fips)) + 
    theme_bw() +
    geom_col(show.legend = TRUE) + 
    facet_wrap(. ~ fips, scale = "free_y") +
    ylab("Emissions (tons)") +
    xlab("Years") +
    ggtitle("Annual Motor Vehicle PM2.5 Emissions in Los Angeles and Baltimore") +
    theme(plot.title = element_text(hjust = 0.5)) +
    geom_smooth(
        aes(as.integer(year), Emissions), 
        method = "lm", 
        se = FALSE,
        color = "black",
        formula = "y ~ x",
        show.legend = FALSE)

rm(list = ls(pattern = "[^NEI|^SCC]"))
