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
