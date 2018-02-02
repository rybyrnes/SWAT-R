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
<<<<<<< HEAD
swatpath <- "C:/Users/rbyrnes/Google Drive/SWAT R Analysis/SWAT Output"
=======
swatpath <- "/Volumes/GoogleDrive/My Drive/SWAT R Analysis/SWAT Output"
>>>>>>> 0a755ba85de983be6b9c6607934b09a1c4b5c33d
swatpathsoil <- "/Volumes/GoogleDrive/My Drive/SWAT R Analysis/SWAT_Soil"

##################################################################################################
###################### Read in data, add calculated ##############################################
###################### columns to SWAT output and ################################################
###################### merge dataframes ##########################################################

# Read in HRU.text data from Access export and set as data.table #
swat.output <- setDT(read.table(file.path(swatpath, "hru20180123.txt"), header = TRUE, fill = TRUE, sep=','))

<<<<<<< HEAD

=======
>>>>>>> 0a755ba85de983be6b9c6607934b09a1c4b5c33d
# Set swat.output variables as factors and YYYYMM as date#
swat.output$HRU <- as.factor(swat.output$HRU)
swat.output$HRUGIS <- as.factor(swat.output$HRUGIS)
swat.output$SUB <- as.factor(swat.output$SUB)
swat.output$YEAR <- as.factor(swat.output$YEAR)
swat.output$MON <- as.factor(swat.output$MON)
swat.output$YYYYMM <- as.Date(paste0(as.character(swat.output$YYYYMM), '01'), format='%Y%m%d')

###################################################################################################
############################ Data Summaries #######################################################

# summary maximum value of crop yield and biomass by land use and HRU #
<<<<<<< HEAD
plant.max.mon <- summaryBy(YLDt_ha + BIOMt_ha ~ LULC + HRU + YEAR + MON, 
=======
plant.max <- summaryBy(YLDt_ha + BIOMt_ha ~ LULC + HRU, 
>>>>>>> 0a755ba85de983be6b9c6607934b09a1c4b5c33d
                       data = swat.output, 
                       FUN = max,
                       keep.names = TRUE)

<<<<<<< HEAD
plant.mean.yr <- summaryBy(YLDt_ha + BIOMt_ha ~ LULC + HRU + YEAR, 
                           data = plant.max.mon, 
                           FUN = max,
                           keep.names = TRUE)

plant.mean <- summaryBy(YLDt_ha + BIOMt_ha ~ LULC + HRU, 
                        data = plant.mean.yr, 
                        FUN = mean,
                        keep.names = TRUE)

# Summary mean of variables by land use, month, year and HRU #
env.sum <- summaryBy(LAI + PRECIPmm + IRRmm + ETmm + N_APPkg_ha + N_AUTOkg_ha + F_MNkg_ha + A_MNkg_ha +  NSURQkg_ha + NLATQkg_ha + NUP_kg_ha+ DNITkg_ha + NO3Lkg_ha + NFIXkg_ha + NRAINkg_ha + PERCmm ~ LULC + HRU + MON + YEAR,
=======
# Calculate and add HI #
plant.max$HI <- plant.max$YLDt_ha/plant.max$BIOMt_ha

# Summary mean of variables by land use and HRU #
env.sum <- summaryBy(LAI + PRECIPmm + IRRmm + ETmm + N_APPkg_ha + N_AUTOkg_ha + F_MNkg_ha + A_MNkg_ha +  NSURQkg_ha + NLATQkg_ha + NUP_kg_ha+ DNITkg_ha + NO3Lkg_ha + NFIXkg_ha + NRAINkg_ha + PERCmm ~ LULC + HRU + YEAR,
>>>>>>> 0a755ba85de983be6b9c6607934b09a1c4b5c33d
                     data = swat.output,
                     keep.names = TRUE,
                     FUN = sum)

<<<<<<< HEAD
# Summary mean of variables by Landuse, HRU and Year #
env.mean.yr <- summaryBy(LAI + PRECIPmm + IRRmm + ETmm + N_APPkg_ha + N_AUTOkg_ha + F_MNkg_ha + A_MNkg_ha +  NSURQkg_ha + NLATQkg_ha + NUP_kg_ha+ DNITkg_ha + NO3Lkg_ha + NFIXkg_ha + NRAINkg_ha + PERCmm  ~ HRU + LULC + YEAR,
=======
# Summary mean of variables by Landuse and HRU #
env.mean <- summaryBy(LAI + PRECIPmm + IRRmm + ETmm + N_APPkg_ha + N_AUTOkg_ha + F_MNkg_ha + A_MNkg_ha +  NSURQkg_ha + NLATQkg_ha + NUP_kg_ha+ DNITkg_ha + NO3Lkg_ha + NFIXkg_ha + NRAINkg_ha + PERCmm  ~ HRU + LULC,
>>>>>>> 0a755ba85de983be6b9c6607934b09a1c4b5c33d
                      data = env.sum,
                      keep.names = TRUE,
                      FUN = mean)

<<<<<<< HEAD
# summary mean of variables by landuse and HRU #

env.mean <- summaryBy(LAI + PRECIPmm + IRRmm + ETmm + N_APPkg_ha + N_AUTOkg_ha + F_MNkg_ha + A_MNkg_ha +  NSURQkg_ha + NLATQkg_ha + NUP_kg_ha+ DNITkg_ha + NO3Lkg_ha + NFIXkg_ha + NRAINkg_ha + PERCmm  ~ HRU + LULC,
                         data = env.mean.yr,
                         keep.names = TRUE,
                         FUN = mean)




# merge plant.max and env.sum #
plant.env.hru <- merge(plant.mean, env.mean)
# add harvest index column
plant.env.hru$HI <- plant.env.hru$YLDt_ha/plant.env.hru$BIOMt_ha
# round digits to 2 decimal points
cols <- names(plant.env.hru)[3:21]
plant.env.hru[,(cols) := round(.SD,2), .SDcols=cols]

=======


# merge plant.max and env.sum #
plant.env.hru <- merge(plant.max, env.mean)
plant.env.hru[,(3:21)] <- round(plant.env.hru[,(3:21)],
                                digits = 2)
>>>>>>> 0a755ba85de983be6b9c6607934b09a1c4b5c33d

###################################################################################
############################### View final table ##################################
###################################################################################

summary(subset(plant.env.hru,
<<<<<<< HEAD
               LULC == "TOMA"))

# View dataframe by LULC subset #
View(subset(plant.env.hru, 
            LULC == "TOMA"))
=======
               LULC == "CORN"))

# View dataframe by LULC subset #
View(subset(plant.env.hru, 
            LULC == "CORN"))
>>>>>>> 0a755ba85de983be6b9c6607934b09a1c4b5c33d


###################################################################################
############################  Simple Histogram of crop yield ######################

hist(subset(plant.env.hru$YLDt_ha, 
            plant.env.hru$LULC == "CORN"))

     