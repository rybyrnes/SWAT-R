# Load packages #
library(doBy)
#library(dplyr) #may use later for summarizing within the function


#fmt stores the variable names in one vector (var), and the character lengths in a second (col)
#format of function call is swat_readOutputhru(filepath)

swat_readOutputhru <- function(file,col=NULL,hru=NULL,year=1962,lulc=NULL,ver=2012) {
fmt=list(var=c('LULC','HRU','HRUGIS','SUB', 'MGT','MON','AREAkm2','PRECIPmm','SNOFALLmm','SNOMELTmm','IRRmm',
              'PETmm','ETmm','SW_INITmm','SW_ENDmm','PERCmm','GW_RCHGmm','DA_RCHGmm','REVAPmm','SA_IRRmm',
                'DA_IRmm', 'SA_STmm', 'DA_STmm','SURQ_GENmm','SURQ_CNTmm','TLOSSmm','LATQmm','GW_Qmm','WYLD_Qmm','DAILYCN',
                'TMP_AVdgC','TMP_MXdgC','TMP_MNdgC','SOL_TMPdgC','SOLARmj_m2','SYLDt_ha','USLEt_ha',
                'N_APPkg_ha','P_APPkg_ha','N_AUTOkg_ha','P_AUTOkg_ha','NGRZkg_ha', 'PGRZkg_ha','NCFRTkg_ha',
                'PCFRTkg_ha','NRAINkg_ha','NFIXkg_ha','F_MNkg_ha','A_MNkg_ha','A_SNkg_ha','F_MPkg_ha','AO_LPkg_ha',
                'L_APkg_ha','A_SPkg_ha','DNITkg_ha','NUP_kg_ha','PUP_kg_ha','ORGNkg_ha','ORGPkg_ha','SEDPkg_ha',
                'NSURQkg_ha','NLATQkg_ha','NO3Lkg_ha','NO3GWkg_ha','SOLPkg_ha','P_GWkg_ha','W_STRS',
                'TMP_STRS','N_STRS','P_STRS','BIOMt_ha','LAI','YLDt_ha','BACTPct','BACTLPct',
                'WTAB','SOLmm','SNOmm','CMUPkg_ha','CMTOTkg_ha','QTILEmm','TNO3kg_ha','GW_QDmm', 'LATQCNmm','TVAPkg_ha'),

    col=c(4,5,10,rep(5,3),rep(10,68), rep(11,2), rep(10,9))) # spacing between columns in .hru output

#read in the file, with fixed width formating. Column widths are specified in the column vector
res <- read.fwf(file,fmt$col,head=F,skip=9,row.names = NULL,col.names = fmt$var,encoding='latin1',strip.white=TRUE,nrow=-1,buffersize=20000)

# monthly and annual tables
#res <- res[!(res$MON==10.7),] # deletes model run average rows, need to figure out a better method though for longer time series
mon <- res[res$MON<=12,] # subsets 'monthly' data
annual <- res[res$MON>12,]  # subsets 'yearly' data for use in building YEAR column

#changes MON to YEAR in the column headers for the year table
colnames(annual) <- sub('MON','YEAR',colnames(annual))

#identifies the row(s) that have the same HRU and month as the first row

w <- which(mon$HRU==mon$HRU[1] & mon$MON==mon$MON[1] & mon$HRUGIS==mon$HRUGIS[1])
# ******* DON"T UNDERSTAND ********
ww <- c((w-1)[-1],nrow(mon))

years <- min(annual$YEAR):max(annual$YEAR)   #saves the range of years in the data

mon$YEAR <- NA   # adds an empty year column

for (i in 1:length(w)) {
  mon[w[i]:ww[i],][,'YEAR'] <- years[i]      #fills in the year column for the MON table
}

# select years
year = 1962 # hardcoded for now; this should be a parameter in the function

#filters based on the year - may add functionality later
#if (!is.null(year)) {
#  mon <- mon[mon$YEAR>=min(year) & mon$YEAR<=max(year),]
#  annual <- annual[annual$YEAR>=min(year) & annual$YEAR<=max(year),]
#}

# rearrange
rownames(mon) <- rownames(annual) <- NULL
w <- which(colnames(mon)=='MON')     # the column corresponding to MON
ww <- which(colnames(mon)=='YEAR')     #the column corresponding to YEAR
mon <- mon[,c(colnames(mon)[c(1:w)],'YEAR',colnames(mon)[-c(1:w,ww)])]

#return(mon)

########################################################################
######### Conversion to standard units ################################


###################################################################################################
############################ Data Summaries #######################################################
mon$HRU <- as.factor(mon$HRU)
mon$YEAR <- as.factor(mon$YEAR)

#summary of maximum value of yield and biomass by LULC, HRU and YEAR
plant.max <- summaryBy(YLDt_ha + BIOMt_ha ~ LULC + factor(HRU) + factor(YEAR),
                     data = mon,
                     FUN = max,
                     keep.names = TRUE)
#return(plant.max)

# summary of sum of variables of interest, must sum because monthly values are not additive
env.sum <- summaryBy(NUP_kg_ha + NFIXkg_ha + LAI + PRECIPmm + IRRmm + ETmm + N_APPkg_ha + N_AUTOkg_ha + F_MNkg_ha + A_MNkg_ha +  NSURQkg_ha + NLATQkg_ha + NUP_kg_ha+ DNITkg_ha + NO3Lkg_ha + NFIXkg_ha + NRAINkg_ha + PERCmm ~ LULC + factor(HRU) + factor(YEAR),
                       data = mon,
                       keep.names = TRUE,
                      FUN = sum)
#return(env.sum)


#### Merge data tables ####

# merged databases to unify variables of interest, now we have one dataframe with all the variables
# and can now run further summary queries to extract mean values by HRU, LULC or YEAR

merged.output <- merge(plant.max, env.sum)
#return(merged.output)

merged.summary <- summaryBy(.~LULC + YEAR,
                              data = merged.output,
                              keep.names = TRUE,
                              fun  = mean)
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
merged.summary$NUP_lb_ac <- merged.summary$NUP_kg_ha*0.89218
merged.summary$NFIXlb_ac <- merged.summary$NFIXkg_ha*0.89218
merged.summary$SURQ_GENin <- merged.summary$SURQ_CNTmm*0.0393701
merged.summary$PERCin <- merged.summary$PERCmm*0.0393701
merged.summary$F_NUE <- (summary.merged$NUP_lb_ac+summary.merged$NFIXlb_ac)/summary.merged$N_APPlb_ac
merged.summary$IRR_NUE <- NULL
merged.summary$TRUE_NUE <- NULL
merged.summary$F_IRR_NUE <- NULL
merged.summary$IRR_EFF <- (merged.summary$ETin-merged.summary$PRECIPin*0.50)/summary.merged$IRRin
merged.summary$LCH_FRC <- merged.summary$PERCin/(merged.summary$IRRin+merged.summary$PRECIPin-merged.summary$SURQ_GENin)
merged.summary$RUN <- merged.summary$SURQ_GENin/(merged.summary$IRRin+merged.summary$PRECIPin)
merged.summary$N_PCT <- (merged.summary$NUP_lb_ac/merged.summary$BIOMt_ac)/10
merged.summary$HI <- merged.summary$YLDt_ac/merged.summary$BIOMt_ac
#merged.summary <- merged.summary[-c(3:20)]


# Rounding function
round_df <- function(df, digits) {
  nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))

  df[,nums] <- round(df[,nums], digits = digits)

  (df)
}

merged.summary <- round_df(merged.summary, digits = 4) # round to 4 digits

monthly <<- mon

return(merged.summary)

#monthly <<- mon # store monthly model output
#return(merged.summary) # return the model output, summarized to years and lulc


  #Plotting Graphs - Will build this out later, not important right now
  #month_label <- c('jan','fev','mars','avril','mai','juin','juil','aout','sept','oct','nov','dec')
  #sum_table <- mon %>%
  #  group_by(MON) %>%
  #  summarize(mean_PRECIP = mean(PRECIP,na.rm=TRUE),
  #            mean_PERC = mean(PERC,na.rm=TRUE),
  #            mean_SNOFALL = mean(SNOFALL,na.rm=TRUE),
  #            mean_ET = mean(ET,na.rm = TRUE),
  #            mean_SURQ_GEN = mean(SURQ_GEN,na.rm= TRUE))

  #png(file = "C:/Users/pbisho02/Whiteman_Creek/ArcSWAT/Tutorial_InletDefined_MF_SW_runs/Tutorial_InletDefined/Scenarios/Default/TxtInOut/precip.png", width = 800, height = 500)
  #plot(sum_table$MON,sum_table$mean_PRECIP,  xlab = "month", xaxt='n', ylab = "Precipitation (mm)", type = "b", col = "red")
  #axis(1, at = 1:12, labels = month_label)
  #dev.off()

  #png(file = "C:/Users/pbisho02/Whiteman_Creek/ArcSWAT/Tutorial_InletDefined_MF_SW_runs/Tutorial_InletDefined/Scenarios/Default/TxtInOut/perc.png", width = 800, height = 500)
  #plot(sum_table$MON,sum_table$mean_PERC,  xlab = "month", xaxt='n', ylab = "Percolation (mm)", type = "b", col = "red")
  #axis(1, at = 1:12, labels = month_label)
  #dev.off()

  #png(file = "C:/Users/pbisho02/Whiteman_Creek/ArcSWAT/Tutorial_InletDefined_MF_SW_runs/Tutorial_InletDefined/Scenarios/Default/TxtInOut/snofall.png", width = 800, height = 500)
  #plot(sum_table$MON,sum_table$mean_SNOFALL,  xlab = "month", xaxt='n', ylab = "Snow Fall (mm)", type = "b", col = "red")
  #axis(1, at = 1:12, labels = month_label)
  #dev.off()

  #png(file = "C:/Users/pbisho02/Whiteman_Creek/ArcSWAT/Tutorial_InletDefined_MF_SW_runs/Tutorial_InletDefined/Scenarios/Default/TxtInOut/et.png", width = 800, height = 500)
  #plot(sum_table$MON,sum_table$mean_ET,  xlab = "month", xaxt='n', ylab = "Evapotranspiration (mm)", type = "b", col = "red")
  #axis(1, at = 1:12, labels = month_label)
  #dev.off()

  #png(file = "C:/Users/pbisho02/Whiteman_Creek/ArcSWAT/Tutorial_InletDefined_MF_SW_runs/Tutorial_InletDefined/Scenarios/Default/TxtInOut/surq.png", width = 800, height = 500)
  #plot(sum_table$MON,sum_table$mean_SURQ_GEN,  xlab = "month", xaxt='n', ylab = "Surface Runoff (mm)", type = "b", col = "red")
  #axis(1, at = 1:12, labels = month_label)
  #dev.off()

  #return(sum_table)

}

# SUMMARIZE Data
#uses the dplyr pacakge.
# %>% passes a datafrom onto the next argument
