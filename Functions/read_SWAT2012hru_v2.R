
# http://santiago.begueria.es/2013/10/reading-swat-output-hru-files-in-r/

swat_readOutputhru <- function(file,col=NULL,hru=NULL,YEAR=NULL,lulc=NULL,ver=2012) {

    # format of the .hru file (SWAT 2012)

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


        #if (class(cols)=='numeric') {
        #    col <- fmt$var[cols]
        #}

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
            cols <- fmt$var %in% col
            fmt$var <- fmt$var[cols]
            fmt$col <- ifelse(cols,fmt$col,-fmt$col)
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
        mon <<- res[res$MON<=12,]
        anu <<- res[res$MON>12,]

        colnames(anu) <- sub('MON','YEAR',colnames(anu))
        cols <<- which(mon$HRU==mon$HRU[1] & mon$MON==mon$MON[1])
        ww <<- c((cols-1)[-1],nrow(mon))
        years <<- min(anu$YEAR):max(anu$YEAR)
        mon$YEAR <- NA ## I think the fuckup starts here
        for (i in 1:length(cols)) {
            mon[cols[i]:ww[i],][,'YEAR'] <- years[i]
        }

        # select years
        if (!is.null(YEAR)) {
            mon <<- mon[mon$YEAR>=min(YEAR) & mon$YEAR<=max(YEAR),] ### this is fucked up already
            anu <<- anu[anu$YEAR>=min(YEAR) & anu$YEAR<=max(YEAR),] #this looks okay...
        }

        # rearrange
        rownames(mon) <- rownames(anu) <- NULL
        cols <- which(colnames(mon)=='MON')
        ww <- which(colnames(mon)=='YEAR')
        mon2 <<- mon[,c(colnames(mon)[c(1:cols)],'YEAR',colnames(mon)[-c(1:cols,ww)])]

        # go
        return(list(mon=mon,anu=anu))

        mon$LULC <- as.factor(mon$LULC)
        mon$HRU <- as.factor(mon$HRU)
        mon$HRUGIS <- as.factor(mon$HRUGIS)
        mon$SUB <- as.factor(mon$SUB)
        mon$MGT <- as.factor(mon$HRUGIS)
        #mon$MON <- as.factor(mon$MON)
        #mon$YEAR <- as.factor(mon$YEAR)

        #return(mon)

    }
