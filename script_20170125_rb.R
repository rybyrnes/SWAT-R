### Install doBy and ggplot2 ###
install.packages("doBy")
install.packages("ggplot2")
install.packages("data.table")
install.packages("zoo")
install.packages("gridExtra")
install.packages("grid")

### Load Packages doBy for summarizing statistics and ggplot2 for plotting ###
library("doBy")
library("ggplot2")
library("data.table")
library("zoo")
library("grid")
library("gridExtra")

### Set Directory Path - Generic and change as needed ###
swatpath <- "/Volumes/GoogleDrive/My Drive/SWAT R Analysis/SWAT Output"
swatpathsoil <- "/Volumes/GoogleDrive/My Drive/SWAT R Analysis/SWAT_Soil"

##################################################################################################
###################### Read in data, add calculated ##############################################
###################### columns to SWAT output and ################################################
###################### merge dataframes ##########################################################

# Read in HRU.text data from Access export and set as data.table #
swat.output <- setDT(read.table(file.path(swatpath, "hru20180123.txt"), header = TRUE, fill = TRUE, sep=','))

# Set swat.output variables as factors and YYYYMM as date#
swat.output$HRU <- as.factor(swat.output$HRU)
swat.output$HRUGIS <- as.factor(swat.output$HRUGIS)
swat.output$SUB <- as.factor(swat.output$SUB)
swat.output$YEAR <- as.factor(swat.output$YEAR)
swat.output$MON <- as.factor(swat.output$MON)
swat.output$YYYYMM <- as.Date(paste0(as.character(swat.output$YYYYMM), '01'), format='%Y%m%d')

# Read in HRU_info data  and set as data.table#
hru.info <- setDT(read.table(file.path(swatpath, "FINAL_HRU20180223.txt"), header = TRUE, fill = TRUE, sep=','))

# read in TAMU soil data and set as data.table #
#tamu.soil <- setDT(read.csv(file.path(swatpathsoil, "SOIL_tamu.csv"), header = TRUE, fill = TRUE, sep=','))
# set as data.table #
#tamu.soil <- setDT(tamu.soil)

# Read in SSJV soil data and set as data.table#
ssjv.soil <- setDT(read.table(file.path(swatpathsoil, "SSJV_soil.txt"), header = TRUE, fill = TRUE, sep=','))

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
test <- setNames(data.frame(matrix(ncol = 85, nrow = 0)), 
         c('LULC','HRU','HRUGIS','SUB', 'YEAR', 'MON','AREAkm2','PRECIPmm','SNOFALLmm','SNOMELTmm','IRRmm',
           'PETmm','ETmm','SW_INITmm','SW_ENDmm','PERCmm','GW_RCHGmm','DA_RCHGmm','REVAPmm','SA_IRRmm','test',
           'SA_STmm','DA_STmm','SURQ_GENmm','SURQ_CNTmm','TLOSSmm','LATQmm','GW_Qmm','WYLD_Qmm','DAILYCN',
           'TMP_AVdgC','TMP_MXdgC','TMP_MNdgC','SOL_TMPdgC','SOLARmj_m2','SYLDt_ha','USLEt_ha',
           'N_APPkg_ha','P_APPkg_ha','N_AUTOkg_ha','P_AUTOkg_ha','NGRZkg_ha', 'PGRZkg_ha','NCFRTkg_ha',
           'PCFRTkg_ha','NRAINkg_ha','NFIXkg_ha','F_MNkg_ha','A_MNkg_ha','A_SNkg_ha','F_MPkg_ha','AO_LPkg_ha',
           'L_APkg_ha','A_SPkg_ha','DNITkg_ha','NUP_kg_ha','PUP_kg_ha','ORGNkg_ha','ORGPkg_ha','SEDPkg_ha',
           'NSURQkg_ha','NLATQkg_ha','NO3Lkg_ha','NO3GWkg_ha','SOLPkg_ha','P_GWkg_ha','W_STRS',
           'TMP_STRS','N_STRS','P_STRS','BIOMt_ha','LAI','YLDt_ha','BACTPct','BACTLPct',
           'WTAB','WTABELO','SNOmm','CMUPkg_ha','CMTOTkg_ha','QTILEmm','TNO3kg_ha','LNO3kg_ha','YYMMMM','test'))

test2 <-  c('LULC','HRU','HRUGIS','SUB', 'YEAR', 'MON','AREAkm2','PRECIPmm','SNOFALLmm','SNOMELTmm','IRRmm',
                   'PETmm','ETmm','SW_INITmm','SW_ENDmm','PERCmm','GW_RCHGmm','DA_RCHGmm','REVAPmm','SA_IRRmm','test',
                   'SA_STmm','DA_STmm','SURQ_GENmm','SURQ_CNTmm','TLOSSmm','LATQmm','GW_Qmm','WYLD_Qmm','DAILYCN',
                   'TMP_AVdgC','TMP_MXdgC','TMP_MNdgC','SOL_TMPdgC','SOLARmj_m2','SYLDt_ha','USLEt_ha',
                   'N_APPkg_ha','P_APPkg_ha','N_AUTOkg_ha','P_AUTOkg_ha','NGRZkg_ha', 'PGRZkg_ha','NCFRTkg_ha',
                   'PCFRTkg_ha','NRAINkg_ha','NFIXkg_ha','F_MNkg_ha','A_MNkg_ha','A_SNkg_ha','F_MPkg_ha','AO_LPkg_ha',
                   'L_APkg_ha','A_SPkg_ha','DNITkg_ha','NUP_kg_ha','PUP_kg_ha','ORGNkg_ha','ORGPkg_ha','SEDPkg_ha',
                   'NSURQkg_ha','NLATQkg_ha','NO3Lkg_ha','NO3GWkg_ha','SOLPkg_ha','P_GWkg_ha','W_STRS',
                   'TMP_STRS','N_STRS','P_STRS','BIOMt_ha','LAI','YLDt_ha','BACTPct','BACTLPct',
                   'WTAB','WTABELO','SNOmm','CMUPkg_ha','CMTOTkg_ha','QTILEmm','TNO3kg_ha','LNO3kg_ha','YYMMMM','test'))

# Read data #
file <- '/Volumes/GoogleDrive/My Drive/SWAT R Analysis/SWAT Output/output.hru'

# read the lines of the data #
txt= readLines(file)
i = grep("LULC",txt)[1]

# Read data again in tabular form, skipping to first cell of wanted text #
hru.test <- read.table(file,skip=i)
hru.test[, (test2) := df[test2]]
setnames(hru.test, 
         new =  c('LULC','HRU','HRUGIS','SUB', 'YEAR', 'MON','AREAkm2','PRECIPmm','SNOFALLmm','SNOMELTmm','IRRmm',
                  'PETmm','ETmm','SW_INITmm','SW_ENDmm','PERCmm','GW_RCHGmm','DA_RCHGmm','REVAPmm','SA_IRRmm','test',
                  'SA_STmm','DA_STmm','SURQ_GENmm','SURQ_CNTmm','TLOSSmm','LATQmm','GW_Qmm','WYLD_Qmm','DAILYCN',
                  'TMP_AVdgC','TMP_MXdgC','TMP_MNdgC','SOL_TMPdgC','SOLARmj_m2','SYLDt_ha','USLEt_ha',
                  'N_APPkg_ha','P_APPkg_ha','N_AUTOkg_ha','P_AUTOkg_ha','NGRZkg_ha', 'PGRZkg_ha','NCFRTkg_ha',
                  'PCFRTkg_ha','NRAINkg_ha','NFIXkg_ha','F_MNkg_ha','A_MNkg_ha','A_SNkg_ha','F_MPkg_ha','AO_LPkg_ha',
                  'L_APkg_ha','A_SPkg_ha','DNITkg_ha','NUP_kg_ha','PUP_kg_ha','ORGNkg_ha','ORGPkg_ha','SEDPkg_ha',
                  'NSURQkg_ha','NLATQkg_ha','NO3Lkg_ha','NO3GWkg_ha','SOLPkg_ha','P_GWkg_ha','W_STRS',
                  'TMP_STRS','N_STRS','P_STRS','BIOMt_ha','LAI','YLDt_ha','BACTPct','BACTLPct',
                  'WTAB','WTABELO','SNOmm','CMUPkg_ha','CMTOTkg_ha','QTILEmm','TNO3kg_ha','LNO3kg_ha','YYMMMM','test'))


colnames(hru.test) <- test
hru.test <- hru.test[order(hru.output.test$HRU),]

str(hru.output.text)

###################################################################################################


### Read in data as an HRU file using swat_readOutputhru() - not working as of 1/18/2018 ####
file <- 'C:/Users/rbyrnes/Desktop/SWAT R Analysis/SWAT Output/output2.hru'
w <- c(1,2,3,4,5,6)
swat.hru.test <- swat_readOutputhru(file, col=w,ver=2012)

##################################################################################################
################### Calculate and add new columns ################################################

# Calculate lbs ac-1 from t ha-1 #
swat.output$YLDt_ha <- 892.179*(swat.output$YLDt_ha)

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

###################################################################################################
############################ Data Summaries #######################################################

# Simple test summaryBy #
#sum_test <- summaryBy(max(YLDt_ha) ~ LULC + MON, data=swat.output)
#sum_test2 <- aggregate(max(YLDt_ha)~LULC + MON, data=swat.output)

# Calculate lbs ac-1 from t ha-1 and rename columns #
swat.output$YLDt_ha <- swat.output$YLDt_ha/2.47
swat.output$BIOMt_ha <- swat.output$BIOMt_ha/2.47
colnames(swat.output)[colnames(swat.output)=="YLDt_ha"] <- "YLDlbs_ac"
colnames(swat.output)[colnames(swat.output)=="BIOMt_ha"] <- "BIOMlbs_ac"

# Simple test aggregate()  for grain/fruit yield and biomass yield #
# and calculated/added column for harvest index                    #
sum_test.yld <- aggregate(YLDlbs_ac ~ LULC, 
                          data = swat.output, 
                          max)
sum_test.yld$YLDlbs_ac <- round(sum_test.yld$YLDlbs_ac,
                                digits = 2)
sum_test.biom <- aggregate(BIOMlbs_ac ~ LULC, 
                           data = swat.output, 
                           max)
sum_test.plant <- sum_test.yld
sum_test.plant$BIOMlbs_ac <- round(sum_test.biom$BIOMlbs_ac, 
                                   digits = 2)
sum_test.plant$HI <- round(sum_test.plant$YLDlbs_ac/sum_test.plant$BIOMlbs_ac, 
                           digits = 2)

# Simple test aggregate()  for grain/fruit yield and biomass yield #
# and calculated/added column for harvest index                    #
sum_test.env <- summaryBy(LAI + PRECIPmm + IRRmm + ETmm + N_APPkg_ha + N_AUTOkg_ha + F_MNkg_ha + A_MNkg_ha +  NSURQkg_ha + NLATQkg_ha + NUP_kg_ha + DNITkg_ha + NO3Lkg_ha + NFIXkg_ha + NRAINkg_ha~ LULC,
                          data = swat.output,
                          keep.names = TRUE,
                          FUN = sum)
sum_test.env[,2:16] <- round(sum_test.env[,2:16],
                             digits=3)

sum.plant.env <- merge(sum_test.plant,sum_test.env, 
                  by = "LULC")

# Simple test plot summary #
plot(sum_test3, data=sum_test3, 
     subset = LULC=="TOMA")

plot(YLDt_ha.mean~LULC+MON, 
     data=sum_test)

plot(sum_test3~LULC, 
     data=sum_test3)

plot(sum_test3, 
     subset = sum_test$LULC=="TOMA")


grid.newpage()
grid.ftable(sum.plant.env, padding = unit(1, "mm"), 
            x = 10, y = 10)

