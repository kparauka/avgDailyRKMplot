### Author: Kalli Parauka
## Sample code - avg. daily RKM calculations and plot
# Last Modified: 29 October 2024


# Library calls, etc. -----------------------------------------------------

# clear the work space
rm(list = ls(all = TRUE))

# and set the working directory (obviously put in whatever yours is)
setwd("~/Desktop/Thesis/Data/Code") 

# install and load packages
install.packages(c("lubridate", "readr", "plyr", "hydroTSM", "tidyr"))
library(lubridate); library(plyr); library(readr); library(dplyr); library(ggplot2); library(hydroTSM); library(tidyr); library(paletteer)


# Read in data ------------------------------------------------------------

# Reading in some sample data. These are all of the detections for two fish (one male, one female), minus any potentially false detections (in this case, where a fish pinged < 2x on a given date).
dat <- read.csv("https://raw.githubusercontent.com/kparauka/avgDailyRKMplot/refs/heads/main/dat.csv", stringsAsFactors = F)

# I'm not sure what your data look like, but most of this information isn't really pertinent to the calculation.
# The most important things will be the format of your detection dates/times and that your receiver identifier (in this case "Beacon") is attached to a river kilometer.

# Progress check ----------------------------------------------------------
dailyRKM <- dat %>% 
  group_by(TagID, timestamp = as.Date(timestamp)) %>%
  summarize(rkm = mean(RKM))

dailyRKM <- aggregate(rkm~timestamp, dailyRKM, mean)

dailyRKM$date <- as.POSIXct(dailyRKM$timestamp, format= "%Y-%m-%d")

plot(rkm~date, data = dailyRKM, type = "l") # this is just a quick and dirty check to make sure things are mostly running okay.

# Presentation plot -----------------------------------------------

# calculate the average RKM for each fish, each day
dailyFish <- dat %>% 
  group_by(TagID, timestamp = as.Date(timestamp)) %>% # group fish by date, ignore time
  summarize(rkm = mean(RKM)) # calculate mean RKM

# convert the dates into POSIXct objects
dailyFish$date <- as.POSIXct(dailyFish$timestamp, format= "%Y-%m-%d")

quartz() # opens the Quartz window (runs on Mac)

# creates the scatter plot of the average daily RKM for each fish, each day
plot(dailyFish$rkm ~ dailyFish$date,
     cex = .2, # point size
     pch = 4, # plotting character (x)
     col = "grey67", # point color
     ylim = c(210, 384), # set the y-axis limits
     las = 3, # set vertical x-axis labels
     ann = FALSE, # stops R from automatically adding axis labels and titles
     yaxt = "n", # remove default y-axis label
     xaxt = "n") # remove default x-axis label

# store dates for labels
dates <- c(as.POSIXct("2023-09-01"), as.POSIXct("2023-10-01"), as.POSIXct("2023-11-01"), as.POSIXct("2023-12-01"), as.POSIXct("2024-01-01"), as.POSIXct("2024-02-01"), as.POSIXct("2024-03-01"), as.POSIXct("2024-04-01"), as.POSIXct("2024-05-01"), as.POSIXct("2024-06-01"), as.POSIXct("2024-07-01"), as.POSIXct("2024-08-01"), as.POSIXct("2024-09-01"), as.POSIXct("2024-10-01"))

# store months for labels
months <- c("Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct")

# adding seasons
abline(v = as.POSIXct("2023-01-01"), lty = 3, col = "grey")
abline(v = as.POSIXct("2023-03-01"), lty = 3, col = "grey")
abline(v = as.POSIXct("2023-06-01"), lty = 3, col = "grey")
abline(v = as.POSIXct("2023-09-01"), lty = 3, col = "grey")
abline(v = as.POSIXct("2023-12-01"), lty = 3, col = "grey")
abline(v = as.POSIXct("2024-03-01"), lty = 3, col = "grey")
abline(v = as.POSIXct("2024-06-01"), lty = 3, col = "grey")
abline(v = as.POSIXct("2024-09-01"), lty = 3, col = "grey")

text(x = as.POSIXct("2023-01-15"), y = 209, labels = "Winter", col = "grey", cex = .80)
text(x = as.POSIXct("2023-4-17"), y = 209, labels = "Spring", col = "grey", cex = .80)
text(x = as.POSIXct("2023-07-16"), y = 209, labels = "Summer", col = "grey", cex = .80)
text(x = as.POSIXct("2023-10-10"), y = 209, labels = "Fall", col = "grey", cex = .80)
text(x = as.POSIXct("2024-01-15"), y = 209, labels = "Winter", col = "grey", cex = .80)
text(x = as.POSIXct("2024-4-17"), y = 209, labels = "Spring", col = "grey", cex = .80)
text(x = as.POSIXct("2024-07-16"), y = 209, labels = "Summer", col = "grey", cex = .80)
text(x = as.POSIXct("2024-10-01"), y = 209, labels = "Fall", col = "grey", cex = .80)

# label the x-axis
axis(1, at = dates, labels = months, cex.axis = 1, las = 3)

# store the river kilometer labels
rkm <- c(214, 268, 324, 380)

# label the y-axis
axis(2, at = rkm, cex.axis = 1)
title(ylab = "River Kilometer", cex.lab = 1.5, adj =.63)

# add the title
title(main = "Average Paddlefish Movements January 2024 - October 2024")

# add the sections and dams
abline(h = 380, lwd = 3)
text(x = as.POSIXct("2024-01-01"), y = 383, labels = "R.F. Henry Lock and Dam", pos = 4)

abline(h = 324, lty = 3, lwd = 1.5)
text(x = as.POSIXct("2024-01-01"), y = 365, labels = "Upstream", col = "grey", cex = 0.8)

abline(h = 268, lty = 3, lwd = 1.5)
text(x = as.POSIXct("2024-01-01"), y = 238.5, labels = "Downstream", col = "grey", cex = 0.8)

abline(h = 214, lwd = 3)
text(x = as.POSIXct("2024-01-01"), y = 217, labels = "Millers Ferry Lock and Dam", pos = 4)

# add the average location
lines(dailyRKM$rkm~dailyRKM$date,
      type = "l",
      lwd = 1.5,
      col = "navyblue")

legend(x = c(as.POSIXct("2024-08-15"), as.POSIXct("2024-10-01")), y = c(325, 375), 
       c("Average Location", "Individual Location", "Dam", "Section Break"),
       lty = c(1, NA, 1, 3),
       pch = c(NA, 4, NA, NA),
       col = c("navyblue", "grey67", "black", "black"),
       lwd = c(1.5, NA, 4, 1),
       cex = .8,
       bg = "white",
       bty = "n")








