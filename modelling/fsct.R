##rm(list=ls())
fcstModel <- function(gauge, modelPath = "./models", apiPath = "http://127.0.0.1:5678", dryRun=FALSE){

    modelFile <- file.path(modelPath,paste0(gauge,".rds"))
    if(!file.exists(modelFile) ){ stop("No model File") }
    mdl <- readRDS(modelFile)
    
    ## arrange data
    D <- fgetData(mdl$gauge_descrption$gauge,apiPath)
    for(g in mdl$gauge_descrption$upstream_gauge){
        tmp <- fgetData(g,apiPath,collapse=TRUE)
        D <- merge(D,tmp,by="dateTime",all=TRUE)
    }
    for(ii in mdl$gauge_descrption$precip_gauge$mid){
        tmp <- fgetData(ii,apiPath,collapse=TRUE)
        D <- merge(D,tmp,by="dateTime",all=TRUE)
    }
    
    ## put data on a fixed footing
    dt <- as.POSIXct(range(D$dateTime),format="%Y-%m-%dT%H:%M:%SZ",tz="GMT")
    dt[2] <- dt[2] + ((mdl$d+900)*900)
    tmp <- data.frame(dateTime= format(seq(dt[1],dt[2],by=900),format="%Y-%m-%dT%H:%M:%SZ",tz="GMT"))
    D <- merge(tmp,D,by="dateTime",all.x=TRUE)
    
    ## compute precipitation
    D$precip <- NA
    D$sumw <- 0
    for(ii in 1:nrow(mdl$gauge_descrption$precip_gauge)){
        g <- mdl$gauge_descrption$precip_gauge$mid[ii]
        w <- mdl$gauge_descrption$precip_gauge$weight[ii]
        idx <- is.finite(D[[g]])
        D$precip[ idx & is.na(D$precip) ] <- 0
        D$precip[idx] <- D$precip[idx] + w*D[[g]][idx]
        D$sumw[idx] <-  D$sumw[idx] + w
    }
    idx <- D$sumw > 0
    D$precip[idx] <- D$precip[idx] / D$sumw[idx]
    ## catch on silly rainfall values
    idx <- which( (D$precip<0) | (D$precip>1000) )
    D$precip[idx] <- 0
    
    y <- D[[mdl$gauge_descrption$gauge]]
    P <- as.matrix( D[,c(mdl$gauge_descrption$upstream_gauge,"precip")])
    
    ## fill value of P that are within data
    for(ii in names(P)){
        rng <- range( which(is.finite(P[[ii]])) )
        idx <- rng[1]:rng[2]
        idx <- idx[ is.na(P[[ii]][idx]) ]
        x <- mean( P[[ii]] , na.rm=TRUE)
        if(ii=="precip"){ x <- 0 }
        P[[ii]][idx] <- x
    }
    
    ## simulate the model
    
    t0 <- which(is.finite(y))
    if( all(t0<mdl$d) ){ stop("No initialisation data") }
    t0 <- max(t0[t0>=mdl$d])+1
    x <- dbmSim(mdl$param,y,P,d=mdl$d,n=mdl$n,yin=NULL,ymin=mdl$ymin,t0=t0,na_fail=FALSE)$x
    if(!any(is.finite(x))){ stop("No valid forecast generated") }
    
    ## trim off starting period
    idx <- max(which(is.finite(x)))
    if( idx < t0 ){ stop("No forecasts generated") }
    idx <- t0:max(which(is.finite(x))) # index of values to keep
    out <- data.frame(dateTime = D$dateTime[idx],
                      fcst = x[idx],
                      fcst_upper=NA,
                      fcst_lower=NA)
    
    ## do uncertainty estimation
    zx <- fApplyMarg(mdl$copula$mrgX,as.matrix(x))
    zz <- mdl$copula$mV; zz[] <- NA
    nf <- length(mdl$copula$mrgE)
    zz[-(1:nf)] <- zx[ t0+((-mdl$d):(nf-1)) ]
    
    try({
        tmp <- fCop(mdl$copula$V,mdl$copula$mV,zz)
        e <- rbind(tmp$mn[1:nf] - 2*diag(tmp$Vr)[1:nf],
                   tmp$mn[1:nf] + 2*diag(tmp$Vr)[1:nf])
        for(ii in 1:nf){
            e[,ii] <- fUndoMarg( mdl$copula$mrgE[[ii]],e[,ii,drop=FALSE])
        }
        jdx <- 1:min(nf,length(out$fcst))
        out$fcst_upper[jdx] <- out$fcst[jdx] + e[2,jdx]
        out$fcst_lower[jdx] <- out$fcst[jdx] + e[1,jdx]
    })
    if(dryRun){return(out)}
    
    tmp <- list(mid = gauge,
                issueTime = format(Sys.time(),"%Y-%m-%dT%H:%M:%SZ",tz="GMT"),
                data = out)
    fnm <- tempfile()
    writeLines(jsonlite::toJSON(tmp),fnm)
    p <- httr::POST( url = file.path(apiPath,"fcst"), body=httr::upload_file(fnm))
    unlink(fnm)
    if( httr::http_error(p) ){
        stop( paste("Upload error for gauge",gauge) )
    }
    
    return(TRUE)
}

