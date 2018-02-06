# File read_hru.R
# Part of the SWAT2R R package, http://www.rforge.net/SWAT2R/ ; 
#                                 http://cran.r-project.org/web/packages/SWAT2R (not available yet)
# Copyright 2011-2012 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

########################################################################
#                            read_hru                                  #
########################################################################
# Purpose    : Function for reading the 'output.hru' files of SWAT 2005#
#              (HRU outputs)                                           #
########################################################################
# Output     : numeric, data.frame or zoo object                       #
########################################################################
# Author     : Mauricio Zambrano-Bigiarini                             #
########################################################################
# Started    : 27-Feb-2009                                             #
# Updates    : 02-Oct-2009                                             #
#              22-Jan-2011 at JRC                                      #
#              17-Apr-2012 at JRC ; 09-Aug-2012 
#              02-Feb-2018 by RCB  
########################################################################

# The last (additional) rows have the average value for each subbasin during all the time period

# 'drty' : Directory where is located the file that have to be read
# 'tstep'  : Time step used for the simulation that created the 'output.rch' file.
#                Must be one of the following values: c("Daily", "Monthly", "Annual"), 
#                stands for daily, monthly and annual times steps
# 'out.type': Type of results that have to be read
#                Must be one of the following values: 
#               -) "All"       : All HRU variables included
#               -) "Q+Sed"   : only results related to water quantity AND sediments are read (first 11 columns)
#                               c("RCH", "GIS", "MON", "DrAREAkm2", "FLOW_INcms", "FLOW_OUTcms", 
#                                 "EVAPcms", "TLOSScms", "SED_INtons", "SED_OUTtons", "SEDCONCmg/kg")
#               -) "Q+Sed+WQ": all the columns of the 'output.rch' are read 
# 'hruID'      : OPTIONAL. Integer with the number of the reach for wich the results will be provided.
#                If this argument is not provided, the results will be given for all the reaches in 'output.hru'
read_hru <- function(file="output.hru", 
                     out.type="All", 
                     hruID=NA, 
                     col.names=NULL,          # character with the column name in 'file' that stores the results that the user wants to convert into a zoo object
                     tstep,                    
                     Date.Ini,                # character with the starting date for the results that are stored in 'file'
                     Date.Fin,                # character with the ending date for the results that are stored in 'file'
                     date.fmt="%Y-%m-%d",     # character, with the format used to define 'Date.Ini", "Date.Fin". See 'format' in 'as.Date'.
                     verbose=TRUE
){
  
  # Checking that 'file' exists
  if ( !file.exists(file) )
    stop( paste("Invalid argument value: The file '", basename(file), "' doesn't exist", sep="" ) )
  
  # Checking that the user provided a valid value for 'tstep'
  if (missing(tstep)) {
    stop("Missing argument value: 'tstep' must be in c('daily','monthly','annual')") 
  } else  # Checking the validity of the 'unit' argument
    if ( is.na( match(tstep, c("daily", "monthly", "annual") ) ) ) {
      stop("Invalid argument value: 'tstep' must be in c('daily', 'monthly', 'annual')" ) }
  
  # Checking that the user provided a valid value for 'out.type'    
  if ( is.na( match(out.type, c("All", "Q+Sed", "Q+Sed+WQ") ) ) ) {
    stop("Invalid argument value: 'out.type' must be in c('All', 'Q+Sed', 'Q+Sed+WQ')" ) }
  
  # Column names may neeed to adapt for different time steps though #
  hru.names <-  hru.names <- c('LULC','HRU','HRUGIS','SUB', 'MGT','MON','AREAkm2','PRECIPmm','SNOFALLmm','SNOMELTmm','IRRmm',
                               'PETmm','ETmm','SW_INITmm','SW_ENDmm','PERCmm','GW_RCHGmm','DA_RCHGmm','REVAPmm','SA_IRRmm',
                               'DA_IRmm', 'SA_STmm', 'DA_STmm','SURQ_GENmm','SURQ_CNTmm','TLOSSmm','LATQmm','GW_Qmm','WYLD_Qmm','DAILYCN',
                               'TMP_AVdgC','TMP_MXdgC','TMP_MNdgC','SOL_TMPdgC','SOLARmj_m2','SYLDt_ha','USLEt_ha',
                               'N_APPkg_ha','P_APPkg_ha','N_AUTOkg_ha','P_AUTOkg_ha','NGRZkg_ha', 'PGRZkg_ha','NCFRTkg_ha',
                               'PCFRTkg_ha','NRAINkg_ha','NFIXkg_ha','F_MNkg_ha','A_MNkg_ha','A_SNkg_ha','F_MPkg_ha','AO_LPkg_ha',
                               'L_APkg_ha','A_SPkg_ha','DNITkg_ha','NUP_kg_ha','PUP_kg_ha','ORGNkg_ha','ORGPkg_ha','SEDPkg_ha',
                               'NSURQkg_ha','NLATQkg_ha','NO3Lkg_ha','NO3GWkg_ha','SOLPkg_ha','P_GWkg_ha','W_STRS',
                               'TMP_STRS','N_STRS','P_STRS','BIOMt_ha','LAI','YLDt_ha','BACTPct','BACTLPct',
                               'WTAB','SOLmm','SNOmm','CMUPkg_ha','CMTOTkg_ha','QTILEmm','TNO3kg_ha','GW_QDmm', 'LATQCNmm','TVAPkg_ha')
  
  #~ hru.widths <- c(4,5,9,5,5,5,
  #~ 10,10,10,10,10,10,10,10,10,10, 10,10,10,10,10,10,10,10,10,10,
  #~ 10,10,10,10,10,10,10,10,10,
  
  #~ 10,10,
  
  #~ 10,10,10,10,10,10,10,10,10,10, 10,10,10,10,10,10,10,10,10,10,
  #~ 10,10,10,10,10,10,10,10,10,10, 10,10,10,10,10,10,10,10,10,10,
  #~ 10,10
  
  #~ 10,10,10,10,10,10,10,10,10,10, 10,10,10,10,10,10,10,10) ?????
  
  
  
  # Reading the output file of the simulation
  #hru <- read.fwf(fname, widths= hru.widths, header=FALSE, skip=9, sep = "\t")  
  #colnames(hru) <- hru.names
  
  # Reading the output file of the simulation
  if (verbose) 
    print( paste("[Reading the file '", basename(file), "' ...]", sep="" ), quote=FALSE  ) 
  
  # Reading the output file of the simulation
  if (out.type=="All") {
    
    # Reading all variables; two columns at the end of the database needed to be extended to 11 spaces. 
    
    hru <- read.fortran(file, header=FALSE, skip=9, 
                         c("A4", "I5", "I10","3F5", "68F10", "2F11" ,"9F10"))
    
    
    # Assigning the names, maybe change this to different columns of interest? for NUE?
    colnames(hru) <- hru.names
    
  } else if (out.type=="Q+Sed") {
    
    # Reading only the 11 variables related to water quantity and sediments
    hru <- read.fortran(file, header=FALSE, skip=9, c("A4", "I5", "I9", "3F5", "29F10", "2F10"))  
    
    # Assigning the the complete list of names
    colnames(hru) <- hru.names[1:37]
    
  } else if (out.type=="Q+Sed+WQ") {
    
    # Reading ALL the outputs 
    hru <- read.fortran(file, header=FALSE, skip=9, c("A4", "I5", "I9", "3F5", "29F10", "2F10", "42F10"))    
    # Assigning the the complete list of names
    colnames(hru) <- hru.names
    
  } # ELSE end
  
  # Apply Column names
  colnames(test) <- hru.names
  # Remove YEAR value rows
  test <-test[test$MON <= 12 ,]
  #Convert columns to factors
  test$LULC <- as.factor(test$LULC)
  test$HRU <- as.factor(test$HRU)
  test$HRUGIS <- as.factor(test$HRUGIS)
  test$SUB <- as.factor(test$SUB)
  test$MGT <- as.factor(test$HRUGIS)
  test$MON <- as.factor(test$MON)
  
  # If the user provided a reach numer, only those results will be returned to the user  
  if ( !missing(hruID) ) {
    non.int <- which(hruID != floor(hruID))
    if ( length(non.int) > 0 ) {       
      stop("Invalid argument: 'hruID' must be integer" ) 
    } else hru <- hru[hru$HRU == hruID, ]
    
    #############################
    # numeric -> zoo
    if ( (length(hruID)==1) & (!missing(Date.Ini) & !missing(Date.Fin)) ) {
      if (tstep=="daily") {
        dates <- hydroTSM::dip(Date.Ini, Date.Fin, date.fmt=date.fmt)
      } else if (tstep=="monthly") {
        dates <- hydroTSM::mip(Date.Ini, Date.Fin, date.fmt=date.fmt)
      } else if (tstep=="annual") dates <- hydroTSM::yip(Date.Ini, Date.Fin, date.fmt=date.fmt)
      hru <- zoo::zoo(hru, dates)
    } # IF end
    
  } # IF end
  
  #############################
  # Selecting only some columns
  if (!is.null(col.names)) {
    if ( any( !( col.names %in% colnames(hru) ) ) )
      stop( paste("Invalid argument '", col, "' is not a column name in '", file, "'", sep="") )
    
    # Getting only the colum(s) defined by the user
    hru <- hru[, col.names]
  } # IF end
  
  return(hru)
  
} # 'read_hru' END