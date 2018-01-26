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

# Simple aggregate() for grain/fruit yield and biomass yield #
# and calculated/added column for harvest index    #

plot(sum.yld, data=sum.yld,subset = sum.yld$LULC=="TOMA")

sum.yld.max <- aggregate(YLDt_ac ~ LULC + HRU + MON + YEAR, 
                          data = swat.output, 
                          max)
sum.yld <- summaryBy(YLDt_ac ~ LULC + MON + YEAR, 
                     data = swat.output, 
                     FUN = mean)
#sum.yld$YLDlbs_ac <- round(sum.yld$YLDlbs_ac,
#                                digits = 2)
sum.biom <- aggregate(BIOMlbs_ac ~ LULC + HRU, 
                           data = swat.output, 
                           max)
sum.plant <- sum.yld
sum.plant$BIOMlbs_ac <- round(sum.biom$BIOMlbs_ac, 
                                   digits = 2)
sum.plant$HI <- round(sum.plant$YLDlbs_ac/sum.plant$BIOMlbs_ac, 
                           digits = 2)

# Simple aggregate()  for grain/fruit yield and biomass yield #
# and calculated/added column for harvest index               #
sum.env.sum <- summaryBy(LAI + PRECIPin + IRRin + ETin + N_APPlbs_ac + N_AUTOlbs_ac + F_MNlbs_ac + A_MNlbs_ac +  NSURQlbs_ac + NLATQlbs_ac + NUP_lbs_ac + DNITlbs_ac + NO3Llbs_ac + NFIXlbs_ac + NRAINlbs_ac + PERCin ~ YEAR + LULC + HRU,
                          data = swat.output,
                          keep.names = TRUE,
                          FUN = sum)

sum.env <- summaryBy(LAI + PRECIPin + IRRin + ETin + N_APPlbs_ac + N_AUTOlbs_ac + F_MNlbs_ac + A_MNlbs_ac +  NSURQlbs_ac + NLATQlbs_ac + NUP_lbs_ac + DNITlbs_ac + NO3Llbs_ac + NFIXlbs_ac + NRAINlbs_ac + PERCin ~ YEAR + LULC,
                     data = sum.env.sum,
                     keep.names = TRUE,
                     FUN = mean)

sum.env[,3:18] <- sum.env[,3:16] / 2612

sum.env[,3:18] <- round(sum.env[,3:18],
                        digits=2)

sum.env[,2:16] <- round(sum.env[,2:16],
                             digits=2)

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
