# File read_SWAT2012hru
# 2018-02-07 - RByrnes : Adapted from SWAT2R read_hru script; http://www.rforge.net/SWAT2R/ ;http://cran.r-project.org/web/packages/SWAT2R (not available yet)

read_SWAT2012hru <- function(file="output.hru",
                     verbose=TRUE
){

  # Checking that 'file' exists
  if ( !file.exists(file) )
    stop( paste("Invalid argument value: The file '", basename(file), "' doesn't exist", sep="" ) )

  # Column names may neeed to adapt for different time steps though #
  hru.names <- c('LULC','HRU','HRUGIS','SUB', 'MGT','MON','AREAkm2','PRECIPmm','SNOFALLmm','SNOMELTmm','IRRmm',
                               'PETmm','ETmm','SW_INITmm','SW_ENDmm','PERCmm','GW_RCHGmm','DA_RCHGmm','REVAPmm','SA_IRRmm',
                               'DA_IRmm', 'SA_STmm', 'DA_STmm','SURQ_GENmm','SURQ_CNTmm','TLOSSmm','LATQmm','GW_Qmm','WYLD_Qmm','DAILYCN',
                               'TMP_AVdgC','TMP_MXdgC','TMP_MNdgC','SOL_TMPdgC','SOLARmj_m2','SYLDt_ha','USLEt_ha',
                               'N_APPkg_ha','P_APPkg_ha','N_AUTOkg_ha','P_AUTOkg_ha','NGRZkg_ha', 'PGRZkg_ha','NCFRTkg_ha',
                               'PCFRTkg_ha','NRAINkg_ha','NFIXkg_ha','F_MNkg_ha','A_MNkg_ha','A_SNkg_ha','F_MPkg_ha','AO_LPkg_ha',
                               'L_APkg_ha','A_SPkg_ha','DNITkg_ha','NUP_kg_ha','PUP_kg_ha','ORGNkg_ha','ORGPkg_ha','SEDPkg_ha',
                               'NSURQkg_ha','NLATQkg_ha','NO3Lkg_ha','NO3GWkg_ha','SOLPkg_ha','P_GWkg_ha','W_STRS',
                               'TMP_STRS','N_STRS','P_STRS','BIOMt_ha','LAI','YLDt_ha','BACTPct','BACTLPct',
                               'WTAB','SOLmm','SNOmm','CMUPkg_ha','CMTOTkg_ha','QTILEmm','TNO3kg_ha','GW_QDmm', 'LATQCNmm','TVAPkg_ha')


  # Reading the output file of the simulation
  if (verbose)
    print( paste("[Reading the file '", basename(file), "' ...]", sep="" ), quote=FALSE  )

  # Reading all variables; two columns at the end of the database needed to be extended to 11 spaces.
  {

    hru <- read.fortran(file, header=FALSE, skip=9,
                         c("A4", "I5", "I10","3F5", "68F10", "2F11" ,"9F10"))


    # Assigning the names, maybe change this to different columns of interest? for NUE?
    colnames(hru) <- hru.names

    # For some reason the .hru file for the monthly output prints some yearly aggregation #after 12 months
    # just removing those rows here
    hru <-hru[hru$MON <= 12 ,]

  } # ELSE end

  # From character to factor
  hru$LULC <- as.factor(hru$LULC)
  hru$HRU <- as.factor(hru$HRU)
  hru$HRUGIS <- as.factor(hru$HRUGIS)
  hru$SUB <- as.factor(hru$SUB)
  hru$MGT <- as.factor(hru$HRUGIS)
  hru$MON <- as.factor(hru$MON)


  return(hru)

} # 'read_SWAT2012hru' END
