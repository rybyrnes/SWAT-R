## This function has been adapted from numerous other scripts, this may need to be updated somewhat often as the SWAT 2012 edition changes somewhat frequently (i.e. fixed width spacing seems to get modified somewhat frequently). I've adapted this to our stakeholder needs (i.e. converting to standard units). I've also modified other functions to develop a 'basic' function that simply provides a monthly output table for the duration of the model run.

# example function calls:
#1 - if you only want raw monthly data: df <- SWAT_output(file=file)
#2 - if you want summarized data at the year-lulc-hru level with crude plots: df <- SWAT_output(file=file, summary=TRUE)
#3 - if you want specific LULC data from above with summary plots: df <- SWAT_output(file=file, summary=TRUE, LULC="LULC")

# feel free to email me at rybyrnes@gmail.com for comments and suggestions

############################################################################################

SWAT_output <- function(file, summary, crop, scaled) {

if(missing(summary)){ # basic run

# Load required packages #
library(readr) # for reading in fixed width very, very, very fast
library(dplyr) # for filtering out model run averages

fmt=list(var=c('LULC','HRU','HRUGIS','SUB', 'MGT','MON','AREAkm2','PRECIPmm','SNOFALLmm','SNOMELTmm','IRRmm',
      'PETmm','ETmm','SW_INITmm','SW_ENDmm','PERCmm','GW_RCHGmm','DA_RCHGmm','REVAPmm','SA_IRRmm',
      'DA_IRmm', 'SA_STmm', 'DA_STmm','SURQ_GENmm','SURQ_CNTmm','TLOSSmm','LATQmm','GW_Qmm','WYLD_Qmm','DAILYCN',
      'TMP_AVdgC','TMP_MXdgC','TMP_MNdgC','SOL_TMPdgC','SOLARmj_m2','SYLDt_ha','USLEt_ha',
      'N_APPkg_ha','P_APPkg_ha','N_AUTOkg_ha','P_AUTOkg_ha','NGRZkg_ha', 'PGRZkg_ha','NCFRTkg_ha',
      'PCFRTkg_ha','NRAINkg_ha','NFIXkg_ha','F_MNkg_ha','A_MNkg_ha','A_SNkg_ha','F_MPkg_ha','AO_LPkg_ha',
      'L_APkg_ha','A_SPkg_ha','DNITkg_ha','NUP_kg_ha','PUP_kg_ha','ORGNkg_ha','ORGPkg_ha','SEDPkg_ha',
      'NSURQkg_ha','NLATQkg_ha','NO3Lkg_ha','NO3GWkg_ha','SOLPkg_ha','P_GWkg_ha','W_STRS',
      'TMP_STRS','N_STRS','P_STRS','BIOMt_ha','LAI','YLDt_ha','BACTPct','BACTLPct',
      'WTAB','SOLmm','SNOmm','CMUPkg_ha','CMTOTkg_ha','QTILEmm','TNO3kg_ha','LNO3kg_ha','GW_QDmm', 'LATQCNmm','TVAPkg_ha'),

    col=c(4,5,10,rep(5,3),rep(10,69), rep(11,2), rep(10,9))) # spacing between columns in .hru output

## read in the file, with fixed width formatting. Column widths are specified in the column vector, kept fmt object with variable names and column width in case using R base read.fwf is desired

#res <- read.fwf(file,fmt$col,head=F,skip=9,row.names = NULL,col.names = fmt$var,encoding='latin1',strip.white=TRUE,nrow=-1,buffersize=20000) # very slow but leaving here for functionality
res <- read_fwf(file, # read_fwf is incredibly fast compared to read.fwf
                skip=9,
                col_types = "ccnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnn", # designate column data type, here c = character and n = numeric, change as needed here or in post-processing
                fwf_widths(c(4,5,10,rep(5,3), rep(10,69), rep(11,2), rep(10,9)))) # column widths

res <- as.data.frame(res) # convert to normal data frame, necessary?
colnames(res) <- fmt$var # add columns
# res <- res[complete.cases(res[ , 6]),] # needed earlier, leaving here in case

# monthly and annual tables
#res <- res[!(res$MON==10.7),] # deletes model run average rows, need to figure out a better method though for longer time series, not needing anymore but leaving still
#res <- res[c(1:(length(res$LULC)-((end.year-start.year)+1)*nhru)),] # potential filtering option
mon <- res[res$MON<=12,] # subsets 'monthly' data
annual <- res[res$MON>12,]  # subsets 'yearly' data for use in building YEAR column

#changes MON to YEAR in the column headers for the year table
colnames(annual) <- sub('MON','YEAR',colnames(annual))

#identifies the row(s) that have the same HRU and month as the first row
w <- which(mon$HRU==mon$HRU[1] & mon$MON==mon$MON[1] & mon$HRUGIS==mon$HRUGIS[1])
# ******* DON"T UNDERSTAND ********
ww <- c((w-1)[-1],nrow(mon))

years <- min(annual$YEAR):max(annual$YEAR)   # saves the range of years in the data

mon$YEAR <- NA   # adds an empty year column

for (i in 1:length(w)) {
  mon[w[i]:ww[i],][,'YEAR'] <- years[i]      #fills in the year column for the mon table
}

#filters based on the year - may add functionality later
#if (!is.null(year)) {
#  mon <- mon[mon$YEAR>=min(year) & mon$YEAR<=max(year),]
#  annual <- annual[annual$YEAR>=min(year) & annual$YEAR<=max(year),]


# rearrange
rownames(mon) <- rownames(annual) <- NULL
w <- which(colnames(mon)=='MON')     # the column corresponding to MON
ww <- which(colnames(mon)=='YEAR')     #the column corresponding to YEAR
mon <- mon[,c(colnames(mon)[c(1:w)],'YEAR',colnames(mon)[-c(1:w,ww)])]

## get rid of model run averages
jantooct <- filter(mon, MON <= 10) # filters out january to october
nov <- filter(mon, MON == 11) # filters out november
dec <- filter(mon, MON == 12) # filters out december

mon <- rbind(jantooct, nov, dec)

return(mon)

} # end for basic run

else if (summary==TRUE){

if(missing(crop)) { # provide basic summaries

# Load required packages #
library(doBy) # for data summaries
library(readr) # for reading in fixed width very, very, very fast
library(dplyr) # for filtering out model run averages

fmt=list(var=c('LULC','HRU','HRUGIS','SUB', 'MGT','MON','AREAkm2','PRECIPmm','SNOFALLmm','SNOMELTmm','IRRmm',
      'PETmm','ETmm','SW_INITmm','SW_ENDmm','PERCmm','GW_RCHGmm','DA_RCHGmm','REVAPmm','SA_IRRmm',
      'DA_IRmm', 'SA_STmm', 'DA_STmm','SURQ_GENmm','SURQ_CNTmm','TLOSSmm','LATQmm','GW_Qmm','WYLD_Qmm','DAILYCN',
      'TMP_AVdgC','TMP_MXdgC','TMP_MNdgC','SOL_TMPdgC','SOLARmj_m2','SYLDt_ha','USLEt_ha',
      'N_APPkg_ha','P_APPkg_ha','N_AUTOkg_ha','P_AUTOkg_ha','NGRZkg_ha', 'PGRZkg_ha','NCFRTkg_ha',
      'PCFRTkg_ha','NRAINkg_ha','NFIXkg_ha','F_MNkg_ha','A_MNkg_ha','A_SNkg_ha','F_MPkg_ha','AO_LPkg_ha',
      'L_APkg_ha','A_SPkg_ha','DNITkg_ha','NUP_kg_ha','PUP_kg_ha','ORGNkg_ha','ORGPkg_ha','SEDPkg_ha',
      'NSURQkg_ha','NLATQkg_ha','NO3Lkg_ha','NO3GWkg_ha','SOLPkg_ha','P_GWkg_ha','W_STRS',
      'TMP_STRS','N_STRS','P_STRS','BIOMt_ha','LAI','YLDt_ha','BACTPct','BACTLPct',
      'WTAB','SOLmm','SNOmm','CMUPkg_ha','CMTOTkg_ha','QTILEmm','TNO3kg_ha','LNO3kg_ha','GW_QDmm', 'LATQCNmm','TVAPkg_ha'),

    col=c(4,5,10,rep(5,3),rep(10,69), rep(11,2), rep(10,9))) # spacing between columns in .hru output

## read in the file, with fixed width formatting. Column widths are specified in the column vector, kept fmt object with variable names and column width in case using R base read.fwf is desired

#res <- read.fwf(file,fmt$col,head=F,skip=9,row.names = NULL,col.names = fmt$var,encoding='latin1',strip.white=TRUE,nrow=-1,buffersize=20000) # very slow but leaving here for functionality
res <- read_fwf(file, # read_fwf is incredibly fast compared to read.fwf
                skip=9,
                col_types = "ccnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnn", # designate column data type, here c = character and n = numeric, change as needed here or in post-processing
                fwf_widths(c(4,5,10,rep(5,3), rep(10,69), rep(11,2), rep(10,9)))) # column widths

res <- as.data.frame(res) # convert to normal data frame, necessary?
colnames(res) <- fmt$var # add columns
# res <- res[complete.cases(res[ , 6]),] # needed earlier, leaving here in case

# monthly and annual tables
#res <- res[!(res$MON==10.7),] # deletes model run average rows, need to figure out a better method though for longer time series, not needing anymore but leaving still
#res <- res[c(1:(length(res$LULC)-((end.year-start.year)+1)*nhru)),] # potential filtering option
mon <- res[res$MON<=12,] # subsets 'monthly' data
annual <- res[res$MON>12,]  # subsets 'yearly' data for use in building YEAR column

#changes MON to YEAR in the column headers for the year table
colnames(annual) <- sub('MON','YEAR',colnames(annual))

#identifies the row(s) that have the same HRU and month as the first row
w <- which(mon$HRU==mon$HRU[1] & mon$MON==mon$MON[1] & mon$HRUGIS==mon$HRUGIS[1])
# ******* DON"T UNDERSTAND ********
ww <- c((w-1)[-1],nrow(mon))

years <- min(annual$YEAR):max(annual$YEAR)   # saves the range of years in the data

mon$YEAR <- NA   # adds an empty year column

for (i in 1:length(w)) {
  mon[w[i]:ww[i],][,'YEAR'] <- years[i]      #fills in the year column for the mon table
}

#filters based on the year - may add functionality later
#if (!is.null(year)) {
#  mon <- mon[mon$YEAR>=min(year) & mon$YEAR<=max(year),]
#  annual <- annual[annual$YEAR>=min(year) & annual$YEAR<=max(year),]


# rearrange
rownames(mon) <- rownames(annual) <- NULL
w <- which(colnames(mon)=='MON')     # the column corresponding to MON
ww <- which(colnames(mon)=='YEAR')     #the column corresponding to YEAR
mon <- mon[,c(colnames(mon)[c(1:w)],'YEAR',colnames(mon)[-c(1:w,ww)])]

## get rid of model run averages
jantooct <- filter(mon, MON <= 10) # filters out january to october
nov <- filter(mon, MON == 11) # filters out november
dec <- filter(mon, MON == 12) # filters out december

mon <- rbind(jantooct, nov, dec)

#return(mon)

mon$LULC <- as.factor(mon$LULC)
mon$HRU <- as.factor(mon$HRU)
mon$YEAR <- as.factor(mon$YEAR) #make these factors

#summary of maximum value of yield and biomass by LULC, HRU and YEAR
plant.max <- summaryBy(YLDt_ha + BIOMt_ha ~ LULC + HRU + YEAR + AREAkm2,
                     data = mon,
                     FUN = max,
                     keep.names = TRUE)
#return(plant.max)

# summary of sum of variables of interest, must sum because monthly values are not additive
env.sum <- summaryBy(SURQ_GENmm + LAI + PRECIPmm + IRRmm + ETmm + N_APPkg_ha + N_AUTOkg_ha + F_MNkg_ha + A_MNkg_ha +  NSURQkg_ha + NLATQkg_ha + NUP_kg_ha+ DNITkg_ha + NO3Lkg_ha + NFIXkg_ha + NRAINkg_ha + PERCmm ~ LULC + YEAR + HRU + AREAkm2,
                       data = mon,
                       keep.names = TRUE,
                      FUN = sum)
#return(env.sum)

#### Merge data tables ####

# merged databases to unify variables of interest, now we have one dataframe with all the variables
# and can now run further summary queries to extract mean values by HRU, LULC or YEAR

env.sum <- env.sum[ # order env.sum to match plant.max order
  with(env.sum, order(LULC, HRU, YEAR)), # orders on LULC -> HRU -> YEAR
  ]

id <- rownames(env.sum) # create unique ID to join on
env.sum <- cbind(id=id, env.sum) #add ID to env.sum
plant.max <- cbind(id=id, plant.max) # add ID to plant.max, plant.max mirrors row lengh or env.sum so same ID's used

merged.summary <- merge(env.sum, plant.max, by="id") # merge on id and then the rest

# Convert dataframe to standard units
merged.summary$YLDt_ac <- merged.summary$YLDt_ha*0.404686
merged.summary$BIOMt_ac <- merged.summary$BIOMt_ha*0.404686
merged.summary$PRECIPin <- merged.summary$PRECIPmm*0.0393701
merged.summary$IRRin <- merged.summary$IRRmm*0.0393701
merged.summary$ETin <- merged.summary$ETmm*0.0393701
merged.summary$N_APPlb_ac <- merged.summary$N_APPkg_ha*0.89218
merged.summary$N_AUTOlb_ac <- merged.summary$N_AUTOkg_ha*0.89218
merged.summary$F_MNlb_ac <- merged.summary$F_MNkg_ha*0.89218
merged.summary$A_MNlb_ac <- merged.summary$A_MNkg_ha*0.89218
merged.summary$NSURQlb_ac <- merged.summary$NSURQkg_ha*0.89218
merged.summary$NLATQlb_ac <- merged.summary$NLATQkg_ha*0.89218
merged.summary$NUP_lb_ac <- merged.summary$NUP_kg_ha*0.89218
merged.summary$DNITlb_ac <- merged.summary$DNITkg_ha*0.89218
merged.summary$NO3Llb_ac <- merged.summary$NO3Lkg_ha*0.89218
merged.summary$NFIXlb_ac <- merged.summary$NFIXkg_ha*0.89218
merged.summary$NRAINlb_ac <- merged.summary$NRAINkg_ha*0.89218
merged.summary$SURQ_GENin <- merged.summary$SURQ_GENmm*0.0393701
merged.summary$PERCin <- merged.summary$PERCmm*0.0393701
merged.summary$ACRE <- merged.summary$AREAkm2.x*0.386102

# Calculated nitrogen use and irrigation use efficiency and various indexes of loss and productivity
merged.summary$F_recovery <- (merged.summary$NUP_lb_ac+merged.summary$NFIXlb_ac)/merged.summary$N_APPlb_ac
#merged.summary$IRR_NUE <- NA # irrigated NUE
#merged.summary$TRUE_NUE <- NA # full NUE
#merged.summary$F_IRR_NUE <- NA # fertilizer AND irrigation NUE
merged.summary$IRR_EFF <- (merged.summary$ETin-merged.summary$PRECIPin*0.50)/merged.summary$IRRin # irrigation efficiency
merged.summary$LCH_FRC <- merged.summary$PERCin/(merged.summary$IRRin+merged.summary$PRECIPin-merged.summary$SURQ_GENin) #leaching fraction
merged.summary$RUN <- merged.summary$SURQ_GENin/(merged.summary$IRRin+merged.summary$PRECIPin) #runoff fraction
merged.summary$N_PCT <- (merged.summary$NUP_lb_ac/merged.summary$BIOMt_ac)/10 # %N in biomass
merged.summary$HI <- merged.summary$YLDt_ac/merged.summary$BIOMt_ac # Harvest index
merged.summary$F_partial <- merged.summary$YLDt_ac/merged.summary$N_APPlb_ac


# Rounding function
round_df <- function(df, digits) {
  nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))

  df[,nums] <- round(df[,nums], digits = digits)

  (df)
}

merged.summary <- round_df(merged.summary, digits = 4) # round to 4 digits

monthly.output <<- mon #returns monthly output in separate dataframe for manual manipulation, these are raw data and units are non-standard

colnames(merged.summary)[colnames(merged.summary)=="HRU.x"] <- "HRU" #rename column
colnames(merged.summary)[colnames(merged.summary)=="LULC.x"] <- "LULC" #rename column
colnames(merged.summary)[colnames(merged.summary)=="YEAR.x"] <- "YEAR" #rename column
merged.summary <- subset(merged.summary, select = -c(NO3Lkg_ha, id, AREAkm2.x, SURQ_GENmm, PRECIPmm, IRRmm, ETmm, N_APPkg_ha, N_AUTOkg_ha, F_MNkg_ha, A_MNkg_ha, NSURQkg_ha, NLATQkg_ha, NUP_kg_ha, DNITkg_ha, NFIXkg_ha, NRAINkg_ha, PERCmm, LULC.y, HRU.y, YEAR.y, AREAkm2.y, YLDt_ha, BIOMt_ha)) # remove unwanted columns

plot(YLDt_ac~LULC, data=merged.summary)
plot(ETin~LULC, data=merged.summary)
plot(IRRin~LULC, data=merged.summary)
plot(BIOMt_ac~LULC, data=merged.summary)
plot(HI~LULC, data=merged.summary)

return(merged.summary)

} # end basic summaries

else {

if(missing(scaled)){

# Load required packages #
library(doBy) # for data summaries
library(readr) # for reading in fixed width very, very, very fast
library(dplyr) # for filtering out model run averages

fmt=list(var=c('LULC','HRU','HRUGIS','SUB', 'MGT','MON','AREAkm2','PRECIPmm','SNOFALLmm','SNOMELTmm','IRRmm',
      'PETmm','ETmm','SW_INITmm','SW_ENDmm','PERCmm','GW_RCHGmm','DA_RCHGmm','REVAPmm','SA_IRRmm',
      'DA_IRmm', 'SA_STmm', 'DA_STmm','SURQ_GENmm','SURQ_CNTmm','TLOSSmm','LATQmm','GW_Qmm','WYLD_Qmm','DAILYCN',
      'TMP_AVdgC','TMP_MXdgC','TMP_MNdgC','SOL_TMPdgC','SOLARmj_m2','SYLDt_ha','USLEt_ha',
      'N_APPkg_ha','P_APPkg_ha','N_AUTOkg_ha','P_AUTOkg_ha','NGRZkg_ha', 'PGRZkg_ha','NCFRTkg_ha',
      'PCFRTkg_ha','NRAINkg_ha','NFIXkg_ha','F_MNkg_ha','A_MNkg_ha','A_SNkg_ha','F_MPkg_ha','AO_LPkg_ha',
      'L_APkg_ha','A_SPkg_ha','DNITkg_ha','NUP_kg_ha','PUP_kg_ha','ORGNkg_ha','ORGPkg_ha','SEDPkg_ha',
      'NSURQkg_ha','NLATQkg_ha','NO3Lkg_ha','NO3GWkg_ha','SOLPkg_ha','P_GWkg_ha','W_STRS',
      'TMP_STRS','N_STRS','P_STRS','BIOMt_ha','LAI','YLDt_ha','BACTPct','BACTLPct',
      'WTAB','SOLmm','SNOmm','CMUPkg_ha','CMTOTkg_ha','QTILEmm','TNO3kg_ha','LNO3kg_ha','GW_QDmm', 'LATQCNmm','TVAPkg_ha'),

    col=c(4,5,10,rep(5,3),rep(10,69), rep(11,2), rep(10,9))) # spacing between columns in .hru output

## read in the file, with fixed width formatting. Column widths are specified in the column vector, kept fmt object with variable names and column width in case using R base read.fwf is desired

#res <- read.fwf(file,fmt$col,head=F,skip=9,row.names = NULL,col.names = fmt$var,encoding='latin1',strip.white=TRUE,nrow=-1,buffersize=20000) # very slow but leaving here for functionality
res <- read_fwf(file, # read_fwf is incredibly fast compared to read.fwf
                skip=9,
                col_types = "ccnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnn", # designate column data type, here c = character and n = numeric, change as needed here or in post-processing
                fwf_widths(c(4,5,10,rep(5,3), rep(10,69), rep(11,2), rep(10,9)))) # column widths

res <- as.data.frame(res) # convert to normal data frame, necessary?
colnames(res) <- fmt$var # add columns
# res <- res[complete.cases(res[ , 6]),] # needed earlier, leaving here in case

# monthly and annual tables
#res <- res[!(res$MON==10.7),] # deletes model run average rows, need to figure out a better method though for longer time series, not needing anymore but leaving still
#res <- res[c(1:(length(res$LULC)-((end.year-start.year)+1)*nhru)),] # potential filtering option
mon <- res[res$MON<=12,] # subsets 'monthly' data
annual <- res[res$MON>12,]  # subsets 'yearly' data for use in building YEAR column

#changes MON to YEAR in the column headers for the year table
colnames(annual) <- sub('MON','YEAR',colnames(annual))

#identifies the row(s) that have the same HRU and month as the first row
w <- which(mon$HRU==mon$HRU[1] & mon$MON==mon$MON[1] & mon$HRUGIS==mon$HRUGIS[1])
# ******* DON"T UNDERSTAND ********
ww <- c((w-1)[-1],nrow(mon))

years <- min(annual$YEAR):max(annual$YEAR)   # saves the range of years in the data

mon$YEAR <- NA   # adds an empty year column

for (i in 1:length(w)) {
  mon[w[i]:ww[i],][,'YEAR'] <- years[i]      #fills in the year column for the mon table
}

#filters based on the year - may add functionality later
#if (!is.null(year)) {
#  mon <- mon[mon$YEAR>=min(year) & mon$YEAR<=max(year),]
#  annual <- annual[annual$YEAR>=min(year) & annual$YEAR<=max(year),]

# rearrange
rownames(mon) <- rownames(annual) <- NULL
w <- which(colnames(mon)=='MON')     # the column corresponding to MON
ww <- which(colnames(mon)=='YEAR')     #the column corresponding to YEAR
mon <- mon[,c(colnames(mon)[c(1:w)],'YEAR',colnames(mon)[-c(1:w,ww)])]

## get rid of model run averages
jantooct <- filter(mon, MON <= 10) # filters out january to october
nov <- filter(mon, MON == 11) # filters out november
dec <- filter(mon, MON == 12) # filters out december

mon <- rbind(jantooct, nov, dec)

#return(mon)

mon$LULC <- as.factor(mon$LULC)
mon$HRU <- as.factor(mon$HRU)
mon$YEAR <- as.factor(mon$YEAR) #make these factors

#summary of maximum value of yield and biomass by LULC, HRU and YEAR
plant.max <- summaryBy(YLDt_ha + BIOMt_ha ~ LULC + HRU + YEAR + AREAkm2,
                     data = mon,
                     FUN = max,
                     keep.names = TRUE)
#return(plant.max)

# summary of sum of variables of interest, must sum because monthly values are not additive
env.sum <- summaryBy(SURQ_GENmm + LAI + PRECIPmm + IRRmm + ETmm + N_APPkg_ha + N_AUTOkg_ha + F_MNkg_ha + A_MNkg_ha +  NSURQkg_ha + NLATQkg_ha + NUP_kg_ha+ DNITkg_ha + NO3Lkg_ha + NFIXkg_ha + NRAINkg_ha + PERCmm ~ LULC + YEAR + HRU + AREAkm2,
                       data = mon,
                       keep.names = TRUE,
                      FUN = sum)
#return(env.sum)

#### Merge data tables ####

# merged databases to unify variables of interest, now we have one dataframe with all the variables
# and can now run further summary queries to extract mean values by HRU, LULC or YEAR

env.sum <- env.sum[ # order env.sum to match plant.max order
  with(env.sum, order(LULC, HRU, YEAR)), # orders on LULC -> HRU -> YEAR
  ]

id <- rownames(env.sum) # create unique ID to join on
env.sum <- cbind(id=id, env.sum) #add ID to env.sum
plant.max <- cbind(id=id, plant.max) # add ID to plant.max, plant.max mirrors row lengh or env.sum so same ID's used

merged.summary <- merge(env.sum, plant.max, by="id") # merge on id and then the rest

# Convert dataframe to standard units
merged.summary$YLDt_ac <- merged.summary$YLDt_ha*0.404686
merged.summary$BIOMt_ac <- merged.summary$BIOMt_ha*0.404686
merged.summary$PRECIPin <- merged.summary$PRECIPmm*0.0393701
merged.summary$IRRin <- merged.summary$IRRmm*0.0393701
merged.summary$ETin <- merged.summary$ETmm*0.0393701
merged.summary$N_APPlb_ac <- merged.summary$N_APPkg_ha*0.89218
merged.summary$N_AUTOlb_ac <- merged.summary$N_AUTOkg_ha*0.89218
merged.summary$F_MNlb_ac <- merged.summary$F_MNkg_ha*0.89218
merged.summary$A_MNlb_ac <- merged.summary$A_MNkg_ha*0.89218
merged.summary$NSURQlb_ac <- merged.summary$NSURQkg_ha*0.89218
merged.summary$NLATQlb_ac <- merged.summary$NLATQkg_ha*0.89218
merged.summary$NUP_lb_ac <- merged.summary$NUP_kg_ha*0.89218
merged.summary$DNITlb_ac <- merged.summary$DNITkg_ha*0.89218
merged.summary$NO3Llb_ac <- merged.summary$NO3Lkg_ha*0.89218
merged.summary$NFIXlb_ac <- merged.summary$NFIXkg_ha*0.89218
merged.summary$NRAINlb_ac <- merged.summary$NRAINkg_ha*0.89218
merged.summary$SURQ_GENin <- merged.summary$SURQ_GENmm*0.0393701
merged.summary$PERCin <- merged.summary$PERCmm*0.0393701
merged.summary$ACRE <- merged.summary$AREAkm2.x*0.386102

# Calculated nitrogen use and irrigation use efficiency and various indexes of loss and productivity
merged.summary$F_recovery <- (merged.summary$NUP_lb_ac+merged.summary$NFIXlb_ac)/merged.summary$N_APPlb_ac
#merged.summary$IRR_NUE <- NA # irrigated NUE
#merged.summary$TRUE_NUE <- NA # full NUE
#merged.summary$F_IRR_NUE <- NA # fertilizer AND irrigation NUE
merged.summary$IRR_EFF <- (merged.summary$ETin-merged.summary$PRECIPin*0.50)/merged.summary$IRRin # irrigation efficiency
merged.summary$LCH_FRC <- merged.summary$PERCin/(merged.summary$IRRin+merged.summary$PRECIPin-merged.summary$SURQ_GENin) #leaching fraction
merged.summary$RUN <- merged.summary$SURQ_GENin/(merged.summary$IRRin+merged.summary$PRECIPin) #runoff fraction
merged.summary$N_PCT <- (merged.summary$NUP_lb_ac/merged.summary$BIOMt_ac)/10 # %N in biomass
merged.summary$HI <- merged.summary$YLDt_ac/merged.summary$BIOMt_ac # Harvest index
merged.summary$F_partial <- merged.summary$YLDt_ac/merged.summary$N_APPlb_ac


# Rounding function
round_df <- function(df, digits) {
  nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))

  df[,nums] <- round(df[,nums], digits = digits)

  (df)
}

merged.summary <- round_df(merged.summary, digits = 4) # round to 4 digits

monthly.output <<- mon #returns monthly output in separate dataframe for manual manipulation, these are raw data and units are non-standard


colnames(merged.summary)[colnames(merged.summary)=="HRU.x"] <- "HRU" #rename column
colnames(merged.summary)[colnames(merged.summary)=="LULC.x"] <- "LULC" #rename column
colnames(merged.summary)[colnames(merged.summary)=="YEAR.x"] <- "YEAR" #rename column
merged.summary <- subset(merged.summary, select = -c(NO3Lkg_ha, id, AREAkm2.x, SURQ_GENmm, PRECIPmm, IRRmm, ETmm, N_APPkg_ha, N_AUTOkg_ha, F_MNkg_ha, A_MNkg_ha, NSURQkg_ha, NLATQkg_ha, NUP_kg_ha, DNITkg_ha, NFIXkg_ha, NRAINkg_ha, PERCmm, LULC.y, HRU.y, YEAR.y, AREAkm2.y, YLDt_ha, BIOMt_ha)) # remove unwanted columns

#return(df <- subset(merged.summary, LULC==crop))
df <- droplevels(merged.summary[merged.summary$LULC%in%crop,])

# plots of some summary data by LULC
plot(YLDt_ac~YEAR, data=df,
xlab="Year",
ylab="Yield (t/ac)",
main="Yield")

plot(ETin~YEAR, data=df,
xlab="Year",
ylab="Evapotranspiration (in)",
main="Evapotranspiration")

plot(IRRin~YEAR, data=df,
xlab="Year",
ylab="Irrigation Applied (in)",
main="Irrigation Applied")

plot(BIOMt_ac~YEAR, data=df,
xlab="Year",
ylab="Total Biomass (t/ac)",
main="Total Accrued Biomass")

plot(HI~YEAR, data=df,
xlab="Year",
ylab="Harvest Index",
main="Harvest Index")

return(df)

} # end LULC summaries

else {

# Load required packages #
library(doBy) # for data summaries
library(readr) # for reading in fixed width very, very, very fast
library(dplyr) # for filtering out model run averages

fmt=list(var=c('LULC','HRU','HRUGIS','SUB', 'MGT','MON','AREAkm2','PRECIPmm','SNOFALLmm','SNOMELTmm','IRRmm',
      'PETmm','ETmm','SW_INITmm','SW_ENDmm','PERCmm','GW_RCHGmm','DA_RCHGmm','REVAPmm','SA_IRRmm',
      'DA_IRmm', 'SA_STmm', 'DA_STmm','SURQ_GENmm','SURQ_CNTmm','TLOSSmm','LATQmm','GW_Qmm','WYLD_Qmm','DAILYCN',
      'TMP_AVdgC','TMP_MXdgC','TMP_MNdgC','SOL_TMPdgC','SOLARmj_m2','SYLDt_ha','USLEt_ha',
      'N_APPkg_ha','P_APPkg_ha','N_AUTOkg_ha','P_AUTOkg_ha','NGRZkg_ha', 'PGRZkg_ha','NCFRTkg_ha',
      'PCFRTkg_ha','NRAINkg_ha','NFIXkg_ha','F_MNkg_ha','A_MNkg_ha','A_SNkg_ha','F_MPkg_ha','AO_LPkg_ha',
      'L_APkg_ha','A_SPkg_ha','DNITkg_ha','NUP_kg_ha','PUP_kg_ha','ORGNkg_ha','ORGPkg_ha','SEDPkg_ha',
      'NSURQkg_ha','NLATQkg_ha','NO3Lkg_ha','NO3GWkg_ha','SOLPkg_ha','P_GWkg_ha','W_STRS',
      'TMP_STRS','N_STRS','P_STRS','BIOMt_ha','LAI','YLDt_ha','BACTPct','BACTLPct',
      'WTAB','SOLmm','SNOmm','CMUPkg_ha','CMTOTkg_ha','QTILEmm','TNO3kg_ha','LNO3kg_ha','GW_QDmm', 'LATQCNmm','TVAPkg_ha'),

    col=c(4,5,10,rep(5,3),rep(10,69), rep(11,2), rep(10,9))) # spacing between columns in .hru output

## read in the file, with fixed width formatting. Column widths are specified in the column vector, kept fmt object with variable names and column width in case using R base read.fwf is desired

#res <- read.fwf(file,fmt$col,head=F,skip=9,row.names = NULL,col.names = fmt$var,encoding='latin1',strip.white=TRUE,nrow=-1,buffersize=20000) # very slow but leaving here for functionality
res <- read_fwf(file, # read_fwf is incredibly fast compared to read.fwf
                skip=9,
                col_types = "ccnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnn", # designate column data type, here c = character and n = numeric, change as needed here or in post-processing
                fwf_widths(c(4,5,10,rep(5,3), rep(10,69), rep(11,2), rep(10,9)))) # column widths

res <- as.data.frame(res) # convert to normal data frame, necessary?
colnames(res) <- fmt$var # add columns
# res <- res[complete.cases(res[ , 6]),] # needed earlier, leaving here in case

# monthly and annual tables
#res <- res[!(res$MON==10.7),] # deletes model run average rows, need to figure out a better method though for longer time series, not needing anymore but leaving still
#res <- res[c(1:(length(res$LULC)-((end.year-start.year)+1)*nhru)),] # potential filtering option
mon <- res[res$MON<=12,] # subsets 'monthly' data
annual <- res[res$MON>12,]  # subsets 'yearly' data for use in building YEAR column

#changes MON to YEAR in the column headers for the year table
colnames(annual) <- sub('MON','YEAR',colnames(annual))

#identifies the row(s) that have the same HRU and month as the first row
w <- which(mon$HRU==mon$HRU[1] & mon$MON==mon$MON[1] & mon$HRUGIS==mon$HRUGIS[1])
# ******* DON"T UNDERSTAND ********
ww <- c((w-1)[-1],nrow(mon))

years <- min(annual$YEAR):max(annual$YEAR)   # saves the range of years in the data

mon$YEAR <- NA   # adds an empty year column

for (i in 1:length(w)) {
  mon[w[i]:ww[i],][,'YEAR'] <- years[i]      #fills in the year column for the mon table
}

#filters based on the year - may add functionality later
#if (!is.null(year)) {
#  mon <- mon[mon$YEAR>=min(year) & mon$YEAR<=max(year),]
#  annual <- annual[annual$YEAR>=min(year) & annual$YEAR<=max(year),]

# rearrange
rownames(mon) <- rownames(annual) <- NULL
w <- which(colnames(mon)=='MON')     # the column corresponding to MON
ww <- which(colnames(mon)=='YEAR')     #the column corresponding to YEAR
mon <- mon[,c(colnames(mon)[c(1:w)],'YEAR',colnames(mon)[-c(1:w,ww)])]

## get rid of model run averages
jantooct <- filter(mon, MON <= 10) # filters out january to october
nov <- filter(mon, MON == 11) # filters out november
dec <- filter(mon, MON == 12) # filters out december

mon <- rbind(jantooct, nov, dec)

#return(mon)

mon$LULC <- as.factor(mon$LULC)
mon$HRU <- as.factor(mon$HRU)
mon$YEAR <- as.factor(mon$YEAR) #make these factors

#summary of maximum value of yield and biomass by LULC, HRU and YEAR
plant.max <- summaryBy(YLDt_ha + BIOMt_ha ~ LULC + HRU + YEAR + AREAkm2,
                     data = mon,
                     FUN = max,
                     keep.names = TRUE)
#return(plant.max)

# summary of sum of variables of interest, must sum because monthly values are not additive
env.sum <- summaryBy(SURQ_GENmm + LAI + PRECIPmm + IRRmm + ETmm + N_APPkg_ha + N_AUTOkg_ha + F_MNkg_ha + A_MNkg_ha +  NSURQkg_ha + NLATQkg_ha + NUP_kg_ha+ DNITkg_ha + NO3Lkg_ha + NFIXkg_ha + NRAINkg_ha + PERCmm ~ LULC + YEAR + HRU + AREAkm2,
                       data = mon,
                       keep.names = TRUE,
                      FUN = sum)
#return(env.sum)

#### Merge data tables ####

# merged databases to unify variables of interest, now we have one dataframe with all the variables
# and can now run further summary queries to extract mean values by HRU, LULC or YEAR

env.sum <- env.sum[ # order env.sum to match plant.max order
  with(env.sum, order(LULC, HRU, YEAR)), # orders on LULC -> HRU -> YEAR
  ]

id <- rownames(env.sum) # create unique ID to join on
env.sum <- cbind(id=id, env.sum) #add ID to env.sum
plant.max <- cbind(id=id, plant.max) # add ID to plant.max, plant.max mirrors row lengh or env.sum so same ID's used

merged.summary <- merge(env.sum, plant.max, by="id") # merge on id and then the rest

# Convert dataframe to standard units
merged.summary$YLDt_ac <- merged.summary$YLDt_ha*0.404686
merged.summary$BIOMt_ac <- merged.summary$BIOMt_ha*0.404686
merged.summary$PRECIPin <- merged.summary$PRECIPmm*0.0393701
merged.summary$IRRin <- merged.summary$IRRmm*0.0393701
merged.summary$ETin <- merged.summary$ETmm*0.0393701
merged.summary$N_APPlb_ac <- merged.summary$N_APPkg_ha*0.89218
merged.summary$N_AUTOlb_ac <- merged.summary$N_AUTOkg_ha*0.89218
merged.summary$F_MNlb_ac <- merged.summary$F_MNkg_ha*0.89218
merged.summary$A_MNlb_ac <- merged.summary$A_MNkg_ha*0.89218
merged.summary$NSURQlb_ac <- merged.summary$NSURQkg_ha*0.89218
merged.summary$NLATQlb_ac <- merged.summary$NLATQkg_ha*0.89218
merged.summary$NUP_lb_ac <- merged.summary$NUP_kg_ha*0.89218
merged.summary$DNITlb_ac <- merged.summary$DNITkg_ha*0.89218
merged.summary$NO3Llb_ac <- merged.summary$NO3Lkg_ha*0.89218
merged.summary$NFIXlb_ac <- merged.summary$NFIXkg_ha*0.89218
merged.summary$NRAINlb_ac <- merged.summary$NRAINkg_ha*0.89218
merged.summary$SURQ_GENin <- merged.summary$SURQ_GENmm*0.0393701
merged.summary$PERCin <- merged.summary$PERCmm*0.0393701
merged.summary$ACRE <- merged.summary$AREAkm2.x*0.386102

# Calculated nitrogen use and irrigation use efficiency and various indexes of loss and productivity
merged.summary$F_recovery <- (merged.summary$NUP_lb_ac+merged.summary$NFIXlb_ac)/merged.summary$N_APPlb_ac
#merged.summary$IRR_NUE <- NA # irrigated NUE
#merged.summary$TRUE_NUE <- NA # full NUE
#merged.summary$F_IRR_NUE <- NA # fertilizer AND irrigation NUE
merged.summary$IRR_EFF <- (merged.summary$ETin-merged.summary$PRECIPin*0.50)/merged.summary$IRRin # irrigation efficiency
merged.summary$LCH_FRC <- merged.summary$PERCin/(merged.summary$IRRin+merged.summary$PRECIPin-merged.summary$SURQ_GENin) #leaching fraction
merged.summary$RUN <- merged.summary$SURQ_GENin/(merged.summary$IRRin+merged.summary$PRECIPin) #runoff fraction
merged.summary$N_PCT <- (merged.summary$NUP_lb_ac/merged.summary$BIOMt_ac)/10 # %N in biomass
merged.summary$HI <- merged.summary$YLDt_ac/merged.summary$BIOMt_ac # Harvest index
merged.summary$F_partial <- merged.summary$YLDt_ac/merged.summary$N_APPlb_ac


# Rounding function
round_df <- function(df, digits) {
  nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))

  df[,nums] <- round(df[,nums], digits = digits)

  (df)
}

merged.summary <- round_df(merged.summary, digits = 4) # round to 4 digits

monthly.output <<- mon #returns monthly output in separate dataframe for manual manipulation, these are raw data and units are non-standard


colnames(merged.summary)[colnames(merged.summary)=="HRU.x"] <- "HRU" #rename column
colnames(merged.summary)[colnames(merged.summary)=="LULC.x"] <- "LULC" #rename column
colnames(merged.summary)[colnames(merged.summary)=="YEAR.x"] <- "YEAR" #rename column
merged.summary <- subset(merged.summary, select = -c(NO3Lkg_ha, id, AREAkm2.x, SURQ_GENmm, PRECIPmm, IRRmm, ETmm, N_APPkg_ha, N_AUTOkg_ha, F_MNkg_ha, A_MNkg_ha, NSURQkg_ha, NLATQkg_ha, NUP_kg_ha, DNITkg_ha, NFIXkg_ha, NRAINkg_ha, PERCmm, LULC.y, HRU.y, YEAR.y, AREAkm2.y, YLDt_ha, BIOMt_ha)) # remove unwanted columns

#return(df <- subset(merged.summary, LULC==crop))
df <- droplevels(merged.summary[merged.summary$LULC%in%crop,])

# plots of some summary data by LULC
plot((YLDt_ac/ACRE)~YEAR, data=df,
xlab="Year",
ylab="Yield (t/ac)",
main="Yield - Area Scaled")

plot(ETin~YEAR, data=df,
xlab="Year",
ylab="Evapotranspiration (in)",
main="Evapotranspiration")

plot(IRRin~YEAR, data=df,
xlab="Year",
ylab="Irrigation Applied (in)",
main="Irrigation Applied")

plot((BIOMt_ac/ACRE)~YEAR, data=df,
xlab="Year",
ylab="Total Biomass (t/ac)",
main="Total Accrued Biomass - Area Scale")

plot(HI~YEAR, data=df,
xlab="Year",
ylab="Harvest Index",
main="Harvest Index")

return(df)

} # end scaled LULC

} # end scaled LULC

} # end basic summaries here

} # end all here

### make sure columns are correct, use Yohannes columns and check - done 4/6/2018
### make sure merge is happening correctly - done 4/9/2018
## plot yearly yield by land use, plot ETmm by land use, irrigation by land use, biomass and HI by landuse - done 4/11/18

## area weight outputs
