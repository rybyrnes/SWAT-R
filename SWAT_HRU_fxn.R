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
}