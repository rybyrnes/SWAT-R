### Install doBy and ggplot2 ###
install.packages("doBy")
install.packages("ggplot2")

### Load Packages doBy for summarizing statistics and ggplot2 for plotting ###
library("doBy")
library("ggplot2")

### Set Directory Path - Generic and change as needed ###
swatpath <- "/Volumes/GoogleDrive/My Drive/SWAT R Analysis/SWAT Output"
swatpathsoil <- "/Volumes/GoogleDrive/My Drive/SWAT R Analysis/SWAT_Soil" 

##################################################################################################

### Read in HRU.text data from Access export ###
swat.output <- read.table(file.path(swatpath, "hru2.txt"), header = TRUE, fill = TRUE, sep=',')
swat.output$AREAacre <- swat.output$AREAkm2*247.105

### Read in HRU_info data  ###
hru.info <- read.table(file.path(swatpathsoil, "FINAL_HRU.txt"), header = TRUE, fill = TRUE, sep=',')

### Read in TAMU and SSJV soil data ###
ssjv.tamu.soil <- read.table(file.path(swatpathsoil, "SSJV_SSURGO_TAMU_prj_new.txt"), header = TRUE, fill = TRUE, sep=',')

### Read in TAMU, SSJV and FINAL HRU ###
ssjv.ssurgo.tamu.final.hru.test<- read.table(file.path(swatpathsoil, "SSJV_SSURGO_TAMU_FINAL_HRU_TEST.txt"), header = TRUE, fill = TRUE, sep=',')

### Merge ssjv.ssurgo.tamu.final.hru.test with SWAT output ###
hru.info.swat1 <- merge(swat.output, ssjv.ssurgo.tamu.final.hru.test, by="HRUGIS", all=T)

##################################################################################################

### Reading in HRU file with simple method only ###
# Modify file path for your machine #
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

### Merge HRU data to soil data
swat.all.merged <- merge(hru.info.swat.output, ssjv.tamu.soil, by.x = "SOIL_CODE", by.y = "SEQN")




