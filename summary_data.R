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

# Convert from metic to standard and rename columns #
swat.output$YLDt_ha <- swat.output$YLDt_ha/2.47
swat.output$BIOMt_ha <- swat.output$BIOMt_ha/2.47
swat.output$PRECIPmm <- swat.output$PRECIPmm/0.0393
swat.output$ETmm <- swat.output$ETmm/0.0393
swat.output$PERCmm <- swat.output$PERCmm/0.0393
swat.output$N_APPkg_ha <- swat.output$N_APPkg_ha/0.892
swat.output$N_AUTOkg_ha <- swat.output$N_AUTOkg_ha/0.892
swat.output$F_MNkg_ha <- swat.output$F_MNkg_ha/0.892
swat.output$A_MNkg_ha <- swat.output$A_MNkg_ha/0.892
swat.output$NSURQkg_ha <- swat.output$NSURQkg_ha/0.892
swat.output$NLATQkg_ha <- swat.output$NLATQkg_ha/0.892
swat.output$NUP_kg_ha <- swat.output$NUP_kg_ha/0.892
swat.output$DNITkg_ha <- swat.output$DNITkg_ha/0.892
swat.output$NO3Lkg_ha <- swat.output$NO3Lkg_ha/0.892
swat.output$NFIXkg_ha <- swat.output$NFIXkg_ha/0.892
swat.output$NRAINkg_ha <- swat.output$NRAINkg_ha/0.892

colnames(swat.output)[colnames(swat.output)=="YLDt_ha"] <- "YLDlbs_ac" #
colnames(swat.output)[colnames(swat.output)=="BIOMt_ha"] <- "BIOMlbs_ac" #
colnames(swat.output)[colnames(swat.output)=="PRECIPmm"] <- "PRECIPin" #
colnames(swat.output)[colnames(swat.output)=="IRRmm"] <- "IRRin" #
colnames(swat.output)[colnames(swat.output)=="ETmm"] <- "ETin" #
colnames(swat.output)[colnames(swat.output)=="N_APPkg_ha"] <- "N_APPlbs_ac" #
colnames(swat.output)[colnames(swat.output)=="N_AUTOkg_ha"] <- "N_AUTOlbs_ac" #
colnames(swat.output)[colnames(swat.output)=="F_MNkg_ha"] <- "F_MNlbs_ac" #
colnames(swat.output)[colnames(swat.output)=="A_MNkg_ha"] <- "A_MNlbs_ac" #
colnames(swat.output)[colnames(swat.output)=="NSURQkg_ha"] <- "NSURQlbs_ac" #
colnames(swat.output)[colnames(swat.output)=="NLATQkg_ha"] <- "NLATQlbs_ac" #
colnames(swat.output)[colnames(swat.output)=="NUP_kg_ha"] <- "NUP_lbs_ac" #
colnames(swat.output)[colnames(swat.output)=="DNITkg_ha"] <- "DNITlbs_ac" #
colnames(swat.output)[colnames(swat.output)=="NO3Lkg_ha"] <- "NO3Llbs_ac"
colnames(swat.output)[colnames(swat.output)=="NFIXkg_ha"] <- "NFIXlbs_ac"
colnames(swat.output)[colnames(swat.output)=="NRAINkg_ha"] <- "NRAINlbs_ac"


# Simple aggregate() for grain/fruit yield and biomass yield #
# and calculated/added column for harvest index              #
sum.yld <- aggregate(YLDlbs_ac ~ LULC, 
                          data = swat.output, 
                          max)
sum.yld$YLDlbs_ac <- round(sum.yld$YLDlbs_ac,
                                digits = 2)
sum.biom <- aggregate(BIOMlbs_ac ~ LULC, 
                           data = swat.output, 
                           max)
sum.plant <- sum.yld
sum.plant$BIOMlbs_ac <- round(sum.biom$BIOMlbs_ac, 
                                   digits = 2)
sum.plant$HI <- round(sum.plant$YLDlbs_ac/sum.plant$BIOMlbs_ac, 
                           digits = 2)

# Simple aggregate()  for grain/fruit yield and biomass yield #
# and calculated/added column for harvest index               #
sum.env <- summaryBy(LAI + PRECIPin + IRRin + ETmm + N_APPlbs_ac + N_AUTOlbs_ac + F_MNlbs_ac + A_MNlbs_ac +  NSURQlbs_ac + NLATQlbs_ac + NUP_lbs_ac + DNITlbs_ac + NO3Llbs_ac + NFIXlbs_ac + NRAINlbs_ac + PERCin ~ LULC,
                          data = swat.output,
                          keep.names = TRUE,
                          FUN = sum)
sum.env[,2:16] <- round(sum.env[,2:16],
                             digits=3)

sum.plant.env <- merge(sum.plant,sum.env, 
                       by = "LULC")

# Simple plot summary #
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
