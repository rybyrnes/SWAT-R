
functions <- "/Volumes/GoogleDrive/My Drive/SWAT R Analysis/R Project/Functions"
swatpath <- "/Volumes/GoogleDrive/My Drive/SWAT R Analysis/SWAT Output"
file <- "/Volumes/GoogleDrive/My Drive/SWAT R Analysis/SWAT Output/output.hru"

source(file.path(functions,'read_SWAT2012hru.R'))

test <- read_SWAT2012hru(file.path(swatpath,'output.hru')) 
write.csv(test, file='test.csv')

library(doBy)

plant <- summaryBy(YLDt_ha + BIOMt_ha ~ LULC + HRU, 
                       data = test, 
                       FUN = mean,
                       keep.names = TRUE)        

source(file.path(functions,'read_SWAT2012hru_v2.R'))

cols = c(1:85)
system.time(test2 <- swat_readOutputhru(file))

#######################################################################################

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
         
         col=c(4,5,10,rep(5,3),rep(10,68), rep(11,2), rep(10,9)))


res <- read.fwf(file,fmt$col,
               head=F,skip=9,encoding='latin1',
               strip.white=TRUE,nrow=-1,buffersize=20000)
colnames(res) <- fmt$var
res <- res[order(res$HRU),]
