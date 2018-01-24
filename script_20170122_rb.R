### Install doBy and ggplot2 ###
install.packages("doBy")
install.packages("ggplot2")
install.packages("data.table")

### Load Packages doBy for summarizing statistics and ggplot2 for plotting ###
library("doBy")
library("ggplot2")
library("data.table")

### Set Directory Path - Generic and change as needed ###
swatpath <- "/Volumes/GoogleDrive/My Drive/SWAT R Analysis/SWAT Output"
swatpathsoil <- "/Volumes/GoogleDrive/My Drive/SWAT R Analysis/SWAT_Soil"

##################################################################################################
###################### Read in data, add calculated columns to SWAT output and ###################
###################### merge dataframes ##########################################################

# Read in HRU.text data from Access export and set as data.table #
swat.output <- setDT(read.table(file.path(swatpath, "hru20180123.txt"), header = TRUE, fill = TRUE, sep=','))

# Calculate and add NUE column to swat.output #
swat.output$NUE <- ((swat.output$NFIXkg_ha+swat.output$NUP_kg_ha)/((swat.output$NRAINkg_ha+swat.output$N_APPkg_ha+swat.output$N_AUTOkg_ha+swat.output$F_MNkg_ha+swat.output$A_MNkg_ha)-(swat.output$NO3Lkg_ha+swat.output$DNITkg_ha+swat.output$NSURQkg_ha+swat.output$NLATQkg_ha)))

# Calcualte and add leaching fraction column to swat.output #
swat.output$LFrac <- swat.output$PERCmm/(swat.output$IRRmm+swat.output$PRECIPmm-swat.output$SURQ_GENmm)

# Calcualte and add runoff fraction column to swat.output #
swat.output$RUNfrac <- swat.output$SURQ_GENmm/(swat.output$IRRmm+swat.output$PRECIPmm)

# Calculate and add %N in biomass column to swat.output #
swat.output$PctN <- ((swat.output$NUP_kg_ha/swat.output$BIOMt_ha)*.1)

# Calculate and add HI column to swat.output #
swat.output$HI <- swat.output$YLDt_ha/swat.output$BIOMt_ha

# Calculate and add acres column #
swat.output$AREAacre <- swat.output$AREAkm2*247.105

# Read in HRU_info data  #
hru.info <- read.table(file.path(swatpath, "FINAL_HRU20180223.txt"), header = TRUE, fill = TRUE, sep=',')
# Set as data.table #
hru.info <- setDT(hru.info)

# read in TAMU soil data #
#tamu.soil <- read.csv(file.path(swatpathsoil, "SOIL_tamu.csv"), header = TRUE, fill = TRUE, sep=',')
# set as data.table #
#tamu.soil <- setDT(tamu.soil)

# Read in SSJV soil data #
ssjv.soil <- read.table(file.path(swatpathsoil, "SSJV_soil.txt"), header = TRUE, fill = TRUE, sep=',')
# set as data.table #
ssjv.soil <- setDT(ssjv.soil)

### Merge dataframes ###

# Merge swat.output and hru.info by HRUGIS #
swat.output.hru <- merge(swat.output, hru.info, all =F)
# set as data.table #
swat.output.hru <- setDT(swat.output.hru)

# Merge swat.output.hru to ssjv.soil #
swat.output.ssjv.soilT <- merge(swat.output.hru, ssjv.soil, by.x="SOIL_CODE", by.y="SEQN", all=T, allow.cartesian = TRUE)


# Merge tamu.soil to swat.output.ssjv.soil #
#swat.output.hru.tamu.soil <- merge(tamu.soil, swat.output.ssjv.soil, by.x="SEQN", by.y ="SOIL_CODE")

# Seperate merge of ssjv and tamu soil db's #
ssjv.tamu.soil <- merge(tamu.soil, ssjv.soil, by="SEQN")


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
############################ Data Summaries #######################################################





