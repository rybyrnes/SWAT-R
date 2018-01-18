# Load Packages doBy for summarizing statistics and ggplot2 for plotting #
library("doBy")
library("ggplot2")

# Set Directory Path #
swatpath <- "C:/Users/rbyrnes/Desktop/SWAT R Analysis/SWAT Output" 

# Read in data #
swat.output.test <- read.csv(file.path(swatpath, "hru.output.csv"), header = TRUE) 

# Summarize data using doBy package and summaryBy() #
