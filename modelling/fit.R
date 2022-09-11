##rm(list=ls())
fitModel <- function(gauge_description, dataPath = "/data/EnvironmentAgency/by_station"){

    print( gauge_description$gauge )
    if( dir.exists(file.path("models",gauge_description$gauge)) ){return(TRUE)}
    
    ## arrange data
    tmp <- fread(gauge_description$gauge,dataPath)
    dt <- as.POSIXct(range(tmp$dateTime),format="%Y-%m-%dT%H:%M:%SZ",tz="GMT")
    D <- data.frame(dateTime = format(seq(dt[1],dt[2],by=900),format="%Y-%m-%dT%H:%M:%SZ",tz="GMT"))
    D <- merge(D,tmp,by="dateTime",all.x=TRUE)
    
    for(g in gauge_description$upstream_gauge){
        tmp <- fread(g,dataPath)
        D <- merge(D,tmp,by="dateTime",all.x=TRUE)
        D[[g]][is.na(D[[g]])] <- mean(D[[g]],na.rm=TRUE)
    }
    
    D$precip <- 0
    D$sumw <- 0
    for(ii in 1:nrow(gauge_description$precip_gauge)){
        g <- gauge_description$precip_gauge$mid[ii]
        w <- gauge_description$precip_gauge$weight[ii]
        tmp <- fread(g,dataPath)
        D <- merge(D,tmp,by="dateTime",all.x=TRUE)
        idx <- is.finite(D[[g]])
        D$precip[idx] <- D$precip[idx] + w*D[[g]][idx]
        D$sumw[idx] <-  D$sumw[idx] + w
    }
    idx <- D$sumw > 0
    D$precip[idx] <- D$precip[idx] / D$sumw[idx]
    ## catch on silly rainfall values
    idx <- (D$precip<0) | (D$precip>1000)
    D$precip[idx] <- 0
    
    y <- D[[gauge_description$gauge]]
    P <- as.matrix( D[,c(gauge_description$upstream_gauge,"precip")])
    ## D <- read.csv("galgate.csv")
    ## P <- cbind(D$hazelrigg.Precip,D$Radar.Rainfall)
    ## P[!is.finite(P)] <- 0
    ## y <- D$Water.Level
    
    ## estimate model
    browser()
    mdlList <- list()
    dseq <- c(1,2,seq(4,48,by=4))
    for(ii in 1:length(dseq)){
        mdlList[[ii]] <- fest(y,P,dseq[ii],2)
    }
    
    mdlPerf <- do.call(rbind, lapply(mdlList,fsummary)) ## summary of model performance
    mdl <- mdlList[[which.min(mdlPerf$RMSE)]] ## select "best" model
    
    
    ## simulate model
    idx <- 1:(96+mdl$d-1)
    idx <- lapply(1:(nrow(D)-max(idx)+1), function(i,idx){idx+i-1},idx=idx)
    
    DY <- lapply(idx,function(i,y){y[i]},y=y)
    DP <- lapply(idx,function(i,y){y[i,]},y=P)
    DX <- list()

    for(ii in 1:length(DY)){
        DX[[ii]] <- dbmSim(coef(mdl$fit),DY[[ii]],DP[[ii]],d=mdl$d,n=mdl$n,yin=NULL,ymin=mdl$ymin)$x
    }
    names(DX) <- names(DY) <- names(DP) <- D$dateTime[1:length(DY)]
    
                                        #matplot(cbind(DX[[15450]],DY[[15450]]))
    ## estimate error model
    DX <- do.call(rbind,DX)
    DY <- do.call(rbind,DY)
    
    dsim <- dbmSim(coef(mdl$fit),y,P,d=mdl$d,n=mdl$n,yin=NULL,ymin=mdl$ymin)$x
    mrgX <- fMarg(dsim)
    ZX <- fApplyMarg(mrgX,DX)
    colnames(ZX) <- paste("x",1:ncol(DX),sep=".")
    
    DE <- (DY-DX)
    colnames(DE) <- paste("e",1:ncol(DX),sep=".")
    DE <- DE[,-(1:mdl$d)] ## loose the 0 errors at the start
    ZE <- DE; ZE[] <- NA
    mrgDE <- list()
    for(ii in 1:ncol(DE)){
        mrgDE[[ii]] <- fMarg(DE[,ii])
        ZE[,ii] <- fApplyMarg(mrgDE[[ii]],DE[,ii,drop=F])
    }
    
    ## estiamte Copula parameters
    ZZ <- cbind(ZE,ZX)
    V <- cov(ZZ,use="pairwise.complete.obs")
    mV <- colMeans(ZZ,na.rm=TRUE)
    
    ## fnm <- colnames(ZZ)[ grepl("e",colnames(ZZ))]
    ## mnZF <- ZZ; mnZF[,fnm] <- NA
    ## sdZF <- mnZF
    
    ## for(ii in 1:nrow(DY)){
    ##     tmp <- fCop(V,mV,mnZF[ii,])
    ##     mnZF[ii,] <- tmp$mn
    ##     sdZF[ii,] <- sqrt( diag(tmp$Vr) )
    ## }
    
    ## Zscr <- pnorm(ZZ[,fnm],mnZF[,fnm],sdZF[,fnm])
    
    dir.create(gauge_description$gauge)
    
    out <- list(gauge_description=gauge_description,
                est = list(y=y,P=P,perf=mdlPerf,mdl=mdl),
                fcst = list(DX=DX,DY=DY) ,
                copula = list(mrgX=mrgX,mrgE=mrgDE,mV=mV,Vr=V)) # ,Zscr=Zscr))
    saveRDS(out,file.path("models",gauge_description$gauge,"output.rds"))
    
    mdl$gauge_descrption <- gauge_description
    mdl$copula <- out$copula; #mdl$copula$Zscr <- NULL
    mdl$param <- coef(mdl$fit)
    mdl$perf <- mdlPerf[which.min(mdlPerf$RMSE),]
    mdl$fit <- NULL
    saveRDS(mdl,file.path("models",gauge_description$gauge,"model.rds"))
    return(TRUE)
}
