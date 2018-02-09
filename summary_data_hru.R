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

plant.max <- summaryBy(YLDt_ha + BIOMt_ha ~ LULC + HRU + YEAR, 
                   data = test2, 
                   FUN = max,
                   keep.names = TRUE)

env.sum <- summaryBy(LAI + PRECIPmm + IRRmm + ETmm + N_APPkg_ha + N_AUTOkg_ha + F_MNkg_ha + A_MNkg_ha +  NSURQkg_ha + NLATQkg_ha + NUP_kg_ha+ DNITkg_ha + NO3Lkg_ha + NFIXkg_ha + NRAINkg_ha + PERCmm ~ LULC + HRU + YEAR,
                     data = test2,
                     keep.names = TRUE,
                     FUN = sum)

#### Merge data tables ####

merged.output <- merge(plant.max, env.sum)

######## Tables ########

summary(subset(merged.output$YLDt_ha,
               merged.output$LULC == "TOMA"))

# View dataframe by LULC subset #
View(subset(merged.output, 
            LULC == "TOMA"))


######## Histograms #####

hist(subset(plant.env.hru$YLDt_ha, 
            plant.env.hru$LULC == "CORN"))



     