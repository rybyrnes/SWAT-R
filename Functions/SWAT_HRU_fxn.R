<<<<<<< HEAD
swat_readOutputhru <- function(file,col=NULL,hru=NULL,year=NULL,lulc=NULL,ver=2012) {
  
  
  
  if (ver==2009) {
    fmt$var <- fmt$var[1:80]
    fmt$col <- fmt$col[1:80]
  }
  if (ver==2005) {
    fmt$var <- fmt$var[1:75]
    fmt$col <- fmt$col[1:75]
  }
  if (class(w)=='numeric') {
    col <- fmt$var[w]
  }
  
  # select columns
  if (!is.null(col)) {
    if (!('MON' %in% col)) {
      col <- c('MON',col)
    }
    if (!('LULC' %in% col)) {
      col <- c('LULC',col)
    }
    if (!('HRU' %in% col)) {
      col <- c('HRU',col)
    }
    w <- fmt$var %in% col
    fmt$var <- fmt$var[w]
    fmt$col <- ifelse(w,fmt$col,-fmt$col)
  }
  
  # read file, rearrange table
  res <- read.fwf(file,fmt$col,
                  head=F,skip=9,encoding='latin1',
                  strip.white=TRUE,nrow=-1,buffersize=20000)
  colnames(res) <- fmt$var
  res <- res[order(res$HRU),]
  
  # select hrus by number or by lulc
  if (!is.null(hru)) {
    res <- res[res$HRU>=min(hru) & res$HRU<=max(hru),]
  }
  if (!is.null(lulc)) {
    res <- res[res$LULC==lulc,]
  }
  
  # monthly and annual tables
  mon <- res[res$MON<=12,]
  anu <- res[res$MON>12,]
  colnames(anu) <- sub('MON','YEA',colnames(anu))
  w <- which(mon$HRU==mon$HRU[1] & mon$MON==mon$MON[1])
  ww <- c((w-1)[-1],nrow(mon))
  years <- min(anu$YEA):max(anu$YEA)
  mon$YEA <- NA
  for (i in 1:length(w)) {
    mon[w[i]:ww[i],][,'YEA'] <- years[i]
  }
  
  # select years
  if (!is.null(year)) {
    mon <- mon[mon$YEA>=min(year) & mon$YEA<=max(year),]
    anu <- anu[anu$YEA>=min(year) & anu$YEA<=max(year),]
  }
  
  # rearrange
  rownames(mon) <- rownames(anu) <- NULL
  w <- which(colnames(mon)=='MON')
  ww <- which(colnames(mon)=='YEA')
  mon <- mon[,c(colnames(mon)[c(1:w)],'YEA',colnames(mon)[-c(1:w,ww)])]
  
  # go
  return(list(mon=mon,anu=anu))
=======
swat_readOutputhru <- function(file,col=NULL,hru=NULL,year=NULL,lulc=NULL,ver=2012) {
  
  # format of the .hru file (SWAT 2012)
  fmt=list(var=c('LULC','HRU','HRUGIS','SUB', 'MGT', 'MON','AREAkm2','PRECIPmm','SNOFALLmm','SNOMELTmm','IRRmm',
                 'PETmm','ETmm','SW_INITmm','SW_ENDmm','PERCmm','GW_RCHGmm','DA_RCHGmm','REVAPmm','SA_IRRmm',
                 'DA_IRmm', 'SA_STmm', 'DA_STmm','SURQ_GENmm','SURQ_CNTmm','TLOSSmm','LATQmm','GW_Qmm','WYLD_Qmm','DAILYCN',
                 'TMP_AVdgC','TMP_MXdgC','TMP_MNdgC','SOL_TMPdgC','SOLARmj_m2','SYLDt_ha','USLEt_ha',
                 'N_APPkg_ha','P_APPkg_ha','N_AUTOkg_ha','P_AUTOkg_ha','NGRZkg_ha', 'PGRZkg_ha','NCFRTkg_ha',
                 'PCFRTkg_ha','NRAINkg_ha','NFIXkg_ha','F_MNkg_ha','A_MNkg_ha','A_SNkg_ha','F_MPkg_ha','AO_LPkg_ha',
                 'L_APkg_ha','A_SPkg_ha','DNITkg_ha','NUP_kg_ha','PUP_kg_ha','ORGNkg_ha','ORGPkg_ha','SEDPkg_ha',
                 'NSURQkg_ha','NLATQkg_ha','NO3Lkg_ha','NO3GWkg_ha','SOLPkg_ha','P_GWkg_ha','W_STRS',
                 'TMP_STRS','N_STRS','P_STRS','BIOMt_ha','LAI','YLDt_ha','BACTPct','BACTLPct',
                 'WTAB','SNOmm','CMUPkg_ha','CMTOTkg_ha','QTILEmm','TNO3kg_ha','GW_QDmm', 'LATQCNmm','TVAPkg_ha'),
           col=c(4,5,10,rep(5,3),rep(10,67),rep(11,2),rep(10,11)))
  if (ver==2009) {
    fmt$var <- fmt$var[1:80]
    fmt$col <- fmt$col[1:80]
  }
  if (ver==2005) {
    fmt$var <- fmt$var[1:75]
    fmt$col <- fmt$col[1:75]
  }
  if (class(w)=='numeric') {
    col <- fmt$var[w]
  }
  
  # select columns
  if (!is.null(col)) {
    if (!('MON' %in% col)) {
      col <- c('MON',col)
    }
    if (!('LULC' %in% col)) {
      col <- c('LULC',col)
    }
    if (!('HRU' %in% col)) {
      col <- c('HRU',col)
    }
    w <- fmt$var %in% col
    fmt$var <- fmt$var[w]
    fmt$col <- ifelse(w,fmt$col,-fmt$col)
  }
  
  # read file, rearrange table
  res <- read.fwf(file,fmt$col,
                  head=F,skip=9,encoding='latin1',
                  strip.white=TRUE,nrow=-1,buffersize=20000)
  colnames(res) <- fmt$var
  res <- res[order(res$HRU),]
  
  # select hrus by number or by lulc
  if (!is.null(hru)) {
    res <- res[res$HRU>=min(hru) & res$HRU<=max(hru),]
  }
  if (!is.null(lulc)) {
    res <- res[res$LULC==lulc,]
  }
  
  # monthly and annual tables
  mon <- res[res$MON<=12,]
  anu <- res[res$MON>12,]
  colnames(anu) <- sub('MON','YEA',colnames(anu))
  w <- which(mon$HRU==mon$HRU[1] & mon$MON==mon$MON[1])
  ww <- c((w-1)[-1],nrow(mon))
  years <- min(anu$YEA):max(anu$YEA)
  mon$YEA <- NA
  for (i in 1:length(w)) {
    mon[w[i]:ww[i],][,'YEA'] <- years[i]
  }
  
  # select years
  if (!is.null(year)) {
    mon <- mon[mon$YEA>=min(year) & mon$YEA<=max(year),]
    anu <- anu[anu$YEA>=min(year) & anu$YEA<=max(year),]
  }
  
  # rearrange
  rownames(mon) <- rownames(anu) <- NULL
  w <- which(colnames(mon)=='MON')
  ww <- which(colnames(mon)=='YEA')
  mon <- mon[,c(colnames(mon)[c(1:w)],'YEA',colnames(mon)[-c(1:w,ww)])]
  
  # go
  return(list(mon=mon,anu=anu))

}