### Set Directory Path - Generic and change as needed ###
swatpath <- "/Volumes/GoogleDrive/My Drive/SWAT R Analysis/SWAT Output"
swatpathsoil <- "/Volumes/GoogleDrive/My Drive/SWAT R Analysis/SWAT_Soil"

### Reading in HRU file with simple method only ###
# Modify file path for your machine #
swatpath <- 'C:/Users/rbyrnes/Desktop/SWAT R Analysis/SWAT Output/'

### Reading in HRU file with simple method only ###
# Modify file path for your machine #
file <- 'C:/Users/rbyrnes/Google Drive/SWAT R Analysis/SWAT Output/output2.hru'

# read the lines of the data #
txt= readLines(file)
i = grep("LULC",txt)[1]
hru.test2 = read.table(file,skip=i)

str(hru.output.test)

# read the lines of the data #
file <- 'C:/Users/rbyrnes/Google Drive/SWAT R Analysis/SWAT Output/output.hru'
txt= readLines(file)
i = grep("LULC",txt)[1]

# Read data again in tabular form, skipping to first cell of wanted text #
hru.test <- read.table(file,skip=9, header= T, sep = "\n")
#hru.test[, (test2) := df[test2]]


colnames(hru.test) <- c('LULC','HRU','HRUGIS','SUB', 'YEAR', 'MON','AREAkm2','PRECIPmm','SNOFALLmm','SNOMELTmm','IRRmm',
                  'PETmm','ETmm','SW_INITmm','SW_ENDmm','PERCmm','GW_RCHGmm','DA_RCHGmm','REVAPmm','SA_IRRmm','test',
                  'SA_STmm','DA_STmm','SURQ_GENmm','SURQ_CNTmm','TLOSSmm','LATQmm','GW_Qmm','WYLD_Qmm','DAILYCN',
                  'TMP_AVdgC','TMP_MXdgC','TMP_MNdgC','SOL_TMPdgC','SOLARmj_m2','SYLDt_ha','USLEt_ha',
                  'N_APPkg_ha','P_APPkg_ha','N_AUTOkg_ha','P_AUTOkg_ha','NGRZkg_ha', 'PGRZkg_ha','NCFRTkg_ha',
                  'PCFRTkg_ha','NRAINkg_ha','NFIXkg_ha','F_MNkg_ha','A_MNkg_ha','A_SNkg_ha','F_MPkg_ha','AO_LPkg_ha',
                  'L_APkg_ha','A_SPkg_ha','DNITkg_ha','NUP_kg_ha','PUP_kg_ha','ORGNkg_ha','ORGPkg_ha','SEDPkg_ha',
                  'NSURQkg_ha','NLATQkg_ha','NO3Lkg_ha','NO3GWkg_ha','SOLPkg_ha','P_GWkg_ha','W_STRS',
                  'TMP_STRS','N_STRS','P_STRS','BIOMt_ha','LAI','YLDt_ha','BACTPct','BACTLPct',
                  'WTAB','WTABELO','SNOmm','CMUPkg_ha','CMTOTkg_ha','QTILEmm','TNO3kg_ha','LNO3kg_ha','YYMMMM','test')                   

# Set swat.output variables as factors and YYYYMM as date#
hru.test$HRU <- as.factor(hru.test$HRU)
hru.test$HRUGIS <- as.factor(hru.test$HRUGIS)
hru.test$SUB <- as.factor(hru.test$SUB)
hru.test$YEAR <- as.factor(hru.test$YEAR)
hru.test$MON <- as.factor(hru.test$MON)
hru.test$YYYYMM <- as.Date(paste0(as.character(hru.test$YYYYMM), '01'), format='%Y%m%d')