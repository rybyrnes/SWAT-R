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

# summary maximum value of crop yield and biomass by land use, year and HRU #
plant.max <- summaryBy(YLDt_ha + BIOMt_ha ~ LULC + YEAR + HRU, 
                     data = swat.output, 
                     FUN = max,
                     keep.names = TRUE)

# summary mean of yield and biomass by land use and year #
plant.yr <- summaryBy(YLDt_ha + BIOMt_ha ~ LULC + YEAR,
                     data = plant.max,
                     FUN = mean,
                     keep.names = TRUE)

# caclulate and add harvest index column #
plant.yr$HI <- plant.yr$YLDt_ha/plant.yr$BIOMt_ha

View(plant.yr)

# summary mean of yield and biomass by land use only #
plant.lulc <- summaryBy(YLDt_ha + BIOMt_ha ~ LULC,
                        data = plant.yr,
                        FUN = mean,
                        keep.names = TRUE)

# calculate and add harvest index column #
plant.lulc$HI <- plant.lulc$YLDt_ha/plant.lulc$BIOMt_ha

View(plant.lulc)

# Summary mean of variables by year, land use and HRU #
env.sum <- summaryBy(LAI + PRECIPmm + IRRmm + ETmm + N_APPkg_ha + N_AUTOkg_ha + F_MNkg_ha + A_MNkg_ha +  NSURQkg_ha + NLATQkg_ha + NUP_kg_ha+ DNITkg_ha + NO3Lkg_ha + NFIXkg_ha + NRAINkg_ha + PERCmm ~ YEAR + LULC + HRU,
                          data = swat.output,
                          keep.names = TRUE,
                          FUN = sum)

# Summary mean of variables by HRU #
env.yr <- summaryBy(LAI + PRECIPmm + IRRmm + ETmm + N_APPkg_ha + N_AUTOkg_ha + F_MNkg_ha + A_MNkg_ha +  NSURQkg_ha + NLATQkg_ha + NUP_kg_ha+ DNITkg_ha + NO3Lkg_ha + NFIXkg_ha + NRAINkg_ha + PERCmm  ~ YEAR + LULC,
                     data = env.sum,
                     keep.names = TRUE,
                     FUN = mean)

# merge original plant and env tables with all HRU's#
plant.env.hru <- merge(plant.max, env.sum
                       by = "HRU",
                       allow.cartesian = TRUE)

# merge plant.yr and env.yr #
plant.env.yr <- merge(plant.yr, env.yr)

View(plant.env.yr)

# summary of all variables averaged by year to leave means of land use #
plant.env.lulc <- summaryBy(YLDt_ha + BIOMt_ha + LAI + PRECIPmm + IRRmm + ETmm + N_APPkg_ha + N_AUTOkg_ha + F_MNkg_ha + A_MNkg_ha +  NSURQkg_ha + NLATQkg_ha + NUP_kg_ha+ DNITkg_ha + NO3Lkg_ha + NFIXkg_ha + NRAINkg_ha + PERCmm  ~ LULC,
                            data = plant.env.yr,
                            keep.names = TRUE,
                            FUN = mean)

# Calulcate and add harvest index and round database to two digits #
plant.env.lulc$HI <- plant.env.lulc$YLDt_ha/plant.env.lulc$BIOMt_ha

# round to 2 digits #
plant.env.lulc[,2:20] <- round(plant.env.lulc[,2:20],
                               digits = 2)

###################################################################################
############################### View final table ##################################
###################################################################################

View(plant.env.lulc)


###################################################################################
###################################################################################

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
grid.ftable(plant.env.lulc, padding = unit(0.1, "mm"), 
            x = 10, y = 10)
