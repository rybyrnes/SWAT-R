# Install doBy and ggplot2 #
install.packages("doBy")
install.packages("ggplot2")

# Load Packages doBy for summarizing statistics and ggplot2 for plotting #
library("doBy")
library("ggplot2")

# Set Directory Path #
swatpath <- "C:/Users/rbyrnes/Desktop/SWAT R Analysis/SWAT Output" 

# Read in data as text #
swat.output.text <- read.table(file.path(swatpath, "hru2.txt"), header = TRUE, fill = TRUE, sep=',') 

# Summarize data using doBy package and summaryBy() #
