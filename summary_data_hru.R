### Install doBy and ggplot2 ###
install.packages("doBy")

### Load Packages doBy for summarizing statistics and ggplot2 for plotting ###
library("doBy")

### Set Directory Path - Generic and change as needed ###

functions <- "/Volumes/GoogleDrive/My Drive/SWAT R Analysis/R Project/Functions" # where function script is stored
file <- "/Volumes/GoogleDrive/My Drive/SWAT R Analysis/SWAT Output/output.hru" # # where .hru file is stored

##################################################################################################
###################### Read in data, add calculated ##############################################
###################### columns to SWAT output and ################################################
###################### merge dataframes ##########################################################

source(file.path(functions,'read_SWAT2012hru_v2.R')) # read in function

test2 <- swat_readOutputhru(file) # import .hru file

###################################################################################################
############################ Data Summaries #######################################################

# summary of maximum value of yield and biomass by LULC, HRU and YEAR
plant.max <- summaryBy(YLDt_ha + BIOMt_ha ~ LULC + HRU + YEAR, 
                   data = test2, 
                   FUN = max,
                   keep.names = TRUE)

# summary of sum of variables of interest, must sum because monthly values are not additive
env.sum <- summaryBy(LAI + PRECIPmm + IRRmm + ETmm + N_APPkg_ha + N_AUTOkg_ha + F_MNkg_ha + A_MNkg_ha +  NSURQkg_ha + NLATQkg_ha + NUP_kg_ha+ DNITkg_ha + NO3Lkg_ha + NFIXkg_ha + NRAINkg_ha + PERCmm ~ LULC + HRU + YEAR,
                     data = test2,
                     keep.names = TRUE,
                     FUN = sum)

#### Merge data tables ####

# merged databases to unify variables of interest, now we have one dataframe with all the variables
# and can now run further summary queries to extract mean values by HRU, LULC or YEAR
merged.output <- merge(plant.max, env.sum)





     