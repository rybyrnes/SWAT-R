### Install doBy and ggplot2 ###
install.packages("doBy")
install.packages("ggplot2")

### Load Packages doBy for summarizing statistics and ggplot2 for plotting ###
library("doBy")
library("ggplot2")


### Set Directory Path - Generic and change as needed ###

functions <- "/Volumes/GoogleDrive/My Drive/SWAT R Analysis/R Project/Functions"
swatpath <- "/Volumes/GoogleDrive/My Drive/SWAT R Analysis/SWAT Output"
file <- "/Volumes/GoogleDrive/My Drive/SWAT R Analysis/SWAT Output/output.hru"

##################################################################################################
###################### Read in data, add calculated ##############################################
###################### columns to SWAT output and ################################################
###################### merge dataframes ##########################################################

source(file.path(functions,'read_SWAT2012hru_v2.R'))

system.time(test2 <- swat_readOutputhru(file))

###################################################################################################
############################ Data Summaries #######################################################

plant.max <- summaryBy(YLDt_ha + BIOMt_ha ~ LULC + HRU + MON + YEAR, 
                   data = test2, 
                   FUN = max,
                   keep.names = TRUE)
plant.mean <- summaryBy(YLDt_ha + BIOMt_ha ~ LULC + HRU + YEAR, 
                       data = plant.max, 
                       FUN = mean,
                       keep.names = TRUE)

env.sum <- summaryBy(LAI + PRECIPmm + IRRmm + ETmm + N_APPkg_ha + N_AUTOkg_ha + F_MNkg_ha + A_MNkg_ha +  NSURQkg_ha + NLATQkg_ha + NUP_kg_ha+ DNITkg_ha + NO3Lkg_ha + NFIXkg_ha + NRAINkg_ha + PERCmm ~ LULC + HRU + MON + YEAR,
                     data = test2,
                     keep.names = TRUE,
                     FUN = sum)

env.mean <- summaryBy(LAI + PRECIPmm + IRRmm + ETmm + N_APPkg_ha + N_AUTOkg_ha + F_MNkg_ha + A_MNkg_ha +  NSURQkg_ha + NLATQkg_ha + NUP_kg_ha+ DNITkg_ha + NO3Lkg_ha + NFIXkg_ha + NRAINkg_ha + PERCmm ~ LULC + HRU + YEAR,
                                data = env.sum,
                                keep.names = TRUE,
                                FUN = mean)
                   
merged.output <- merge(plant.mean, env.mean)

###################################################################################
############################### View final table ##################################
###################################################################################

summary(subset(merged.output,
               LULC == "TOMA"))

# View dataframe by LULC subset #
View(subset(merged.output, 
            LULC == "TOMA"))



###################################################################################
############################  Simple Histogram of crop yield ######################

hist(subset(plant.env.hru$YLDt_ha, 
            plant.env.hru$LULC == "CORN"))

     