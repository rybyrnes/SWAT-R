<<<<<<< HEAD
### Install doBy and ggplot2 ###
install.packages("doBy")
install.packages("ggplot2")

### Load Packages doBy for summarizing statistics and ggplot2 for plotting ###
library("doBy")
library("ggplot2")

### Set Directory Path ###
swatpath <- "C:/Users/rbyrnes/Desktop/SWAT R Analysis/SWAT Output" 

##################################################################################################

### Read in HRU.text data from Access export ###
swat.output.text <- read.table(file.path(swatpath, "hru2.txt"), header = TRUE, fill = TRUE, sep=',') 

##################################################################################################

### Reading in HRU file with simple method only ###
file <- 'C:/Users/rbyrnes/Desktop/SWAT R Analysis/SWAT Output/output2.hru'

# read the lines of the data #
txt= readLines(file)
i = grep("LULC",txt)[1]
hru.output.test = read.table(file,skip=i)

str(hru.output.test)

##################################################################################################

### Read in data as an HRU file - Adapted SWA_HRU_fxn script ###
# format of the .hru file (SWAT 2012) #
fmt=list(var=c('LULC','HRU','GIS','SUB','MGT','MON','AREA','PRECIP','SNOFALL','SNOMELT','IRR',
               'PET','ET','SW_INIT','SW_END','PERC','GW_RCHG','DA_RCHG','REVAP','SA_IRR','DA_IRR','SA_ST',
               'DA_ST','SURQ_GEN','SURQ_CNT','TLOSS','LATQ','GW_Q','WYLD','DAILYCN','TMP_AV','TMP_MX',
               'TMP_MN','SOL_TMP','SOLAR','SYLD','USLE','N_APP','P_APP','NAUTO','PAUTO','NGRZ','PGRZ',
               'NCFRT','PCFRT','NRAIN','NFIX','F-MN','A-MN','A-SN','F-MP','AO-LP','L-AP','A-SP','DNIT',
               'NUP','PUP','ORGN','ORGP','SEDP','NSURQ','NLATQ','NO3L','NO3GW','SOLP','P_GW','W_STRS',
               'TMP_STRS','N_STRS','P_STRS','BIOM','LAI','YLD','BACTP','BACTLP',
               'WTAB','WTABELO','SNO_HRU','CMUP_KGH','CMTOT_KGH','QTILE','TNO3','LNO3','GW_Q_D','LATQ_CNT'),
         col=c(4,5,9,5,5,5,rep(10,79)))

# Read data #
file <- 'C:/Users/rbyrnes/Desktop/SWAT R Analysis/SWAT Output/output2.hru'

# read the lines of the data #
txt= readLines(file)
i = grep("LULC",txt)[1]

# Read data again in tabular form, skipping to first cell of wanted text #
hru.output.test = read.table(file, fmt$col,skip=i)

colnames(hru.output.test) <- fmt$var
hru.output.test <- hru.output.test[order(hru.output.test$HRU),]

str(hru.output.text)

###################################################################################################


### Read in data as an HRU file using swat_readOutputhru() - not working as of 1/18/2018 ####
file <- 'C:/Users/rbyrnes/Desktop/SWAT R Analysis/SWAT Output/output2.hru'
w <- c(1,2,3,4,5,6)
swat.hru.test <- swat_readOutputhru(file, col=w,ver=2012)

###################################################################################################

# Summarize data using doBy package and summaryBy() #
=======
### Install doBy and ggplot2 ###
install.packages("doBy")
install.packages("ggplot2")

### Load Packages doBy for summarizing statistics and ggplot2 for plotting ###
library("doBy")
library("ggplot2")

### Set Directory Path ###
swatpath <- "C:/Users/rbyrnes/Desktop/SWAT R Analysis/SWAT Output" 

##################################################################################################

### Read in HRU.text data from Access export ###
swat.output.text <- read.table(file.path(swatpath, "hru2.txt"), header = TRUE, fill = TRUE, sep=',') 

##################################################################################################

### Reading in HRU file with simple method only ###
file <- 'C:/Users/rbyrnes/Desktop/SWAT R Analysis/SWAT Output/output2.hru'

# read the lines of the data #
txt= readLines(file)
i = grep("LULC",txt)[1]
hru.output.test = read.table(file,skip=i)

str(hru.output.test)

##################################################################################################

### Read in data as an HRU file - Adapted SWA_HRU_fxn script ###
# format of the .hru file (SWAT 2012) #
fmt=list(var=c('LULC','HRU','GIS','SUB','MGT','MON','AREA','PRECIP','SNOFALL','SNOMELT','IRR',
               'PET','ET','SW_INIT','SW_END','PERC','GW_RCHG','DA_RCHG','REVAP','SA_IRR','DA_IRR','SA_ST',
               'DA_ST','SURQ_GEN','SURQ_CNT','TLOSS','LATQ','GW_Q','WYLD','DAILYCN','TMP_AV','TMP_MX',
               'TMP_MN','SOL_TMP','SOLAR','SYLD','USLE','N_APP','P_APP','NAUTO','PAUTO','NGRZ','PGRZ',
               'NCFRT','PCFRT','NRAIN','NFIX','F-MN','A-MN','A-SN','F-MP','AO-LP','L-AP','A-SP','DNIT',
               'NUP','PUP','ORGN','ORGP','SEDP','NSURQ','NLATQ','NO3L','NO3GW','SOLP','P_GW','W_STRS',
               'TMP_STRS','N_STRS','P_STRS','BIOM','LAI','YLD','BACTP','BACTLP',
               'WTAB','WTABELO','SNO_HRU','CMUP_KGH','CMTOT_KGH','QTILE','TNO3','LNO3','GW_Q_D','LATQ_CNT'),
         col=c(4,5,9,5,5,5,rep(10,79)))

# Read data #
file <- 'C:/Users/rbyrnes/Desktop/SWAT R Analysis/SWAT Output/output2.hru'

# read the lines of the data #
txt= readLines(file)
i = grep("LULC",txt)[1]

# Read data again in tabular form, skipping to first cell of wanted text #
hru.output.test = read.table(file, fmt$col,skip=i)

colnames(hru.output.test) <- fmt$var
hru.output.test <- hru.output.test[order(hru.output.test$HRU),]

str(hru.output.text)

###################################################################################################


### Read in data as an HRU file using swat_readOutputhru() - not working as of 1/18/2018 ####
file <- 'C:/Users/rbyrnes/Desktop/SWAT R Analysis/SWAT Output/output2.hru'
w <- c(1,2,3,4,5,6)
swat.hru.test <- swat_readOutputhru(file, col=w,ver=2012)

###################################################################################################

# Summarize data using doBy package and summaryBy() #
>>>>>>> 2bf2cd7bcae336a9d521c19be970b63ffbf00df3
