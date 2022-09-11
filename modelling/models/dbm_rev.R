Rcpp::sourceCpp("dbm.cpp")

dbmSim <- function(param,y,P,d=0,n=1,yin=NULL,ymin=min(y,na.rm=TRUE),t0=NULL,na_fail=TRUE){
    if( na_fail & !all(is.finite(P)) ){ stop("All values in p should be finite") }
    if(!is.matrix(P)){ P <- matrix(P,nrow=length(y)) }
    m <- ncol(P)
    y[!is.finite(y)] <- NA
    a <- pnorm(param[1:n])
    B <- matrix( (1-a)*exp(param[(n+1):(n+(n*m))]), ncol=ncol(P) )
    tau <- pnorm(param[(n+(n*m))+1:m])
    f(is.null(t0)){
        t0 <- which(is.finite(y))
        if(all(t0<d)){ return(list( x = rep(NA,length(y)),u=matrix(NA,nrow(P),ncol(P)))) }
        t0 <- min(t0[t0>=d])+1 #min( (which(is.finite(y)),d)+1
    }
    if( y[t0-1]-ymin > 0 ){
        x <- (rowMeans(B)/(1-a))*(y[t0-1]-ymin)
        x <- x*(y[t0-1]-ymin)/sum(x)
    }else{
        x <- rep(0,length(a))
    }
    ##browser()
    ## pjs <- -999
    fsim(x,y,P,a,B,tau,ymin,d,t0)
}

fopt <- function(param,y,p,d,n,ym){
    e <- y - dbmSim(param,y,p,d,n,ymin=ym)$x
    e <- e[is.finite(y)]
    e[!is.finite(e)] <- 2e34
    e <- sign(e)*pmin(abs(e),2e34,na.rm=TRUE)
    #print(c(range(e),sum(e)))
    return(e)
}

fest <- function(y,P,d,n){
    if(!is.matrix(P)){ P <- matrix(P,nrow=length(y)) }
    m <- ncol(P)
    y[ (y - mean(y,na.rm=T))/sd(y,na.rm=T) <= -3] <- NA
    ymin <- quantile(y,0.01,na.rm=TRUE)#min(y,na.rm=TRUE)
    ymin <- max(ymin,0.01) ## catch to stop negative ymin values and y going to 0
    param <- c( qnorm(seq(0.99,0.999,length=n)),
               log(rep(1,n*m)),
               qnorm(rep(0.3,m)) )
    suppressWarnings({
        fit <- nls(~fopt(par,y,P,d,n,ym),
                   data = list(y=y,p=P,d=d,n=n,ym=ymin),
                   start = list(par = param),
                   control = nls.control(warnOnly = TRUE,scaleOffset = 1),
                   lower=c(rep(-5,n),rep(-100,n*m),rep(-5,m)),
                   upper=c(rep(5,n),rep(100,n*m),rep(5,m)),
                   algorithm='port')
    })
    return(list(n=n,d=d,ymin=ymin,fit=fit))
}

fsummary <- function(mdl){
    mse <- mean(residuals(mdl$fit)^2)
    n <- length( residuals(mdl$fit) )
    m <- coef(mdl$fit);
    V <- tryCatch({vcov(mdl$fit)},error=function(e){diag(NA,length(m))})
    y <- get("y",mdl$fit$m$getEnv())
    den <- mean( (y - mean(y,na.rm=TRUE))^2 ,na.rm=TRUE )
    data.frame(n = mdl$n,
               d = mdl$d,
               ymin = mdl$ymin,
               RMSE = sqrt(mse),
               MAE = mean(abs(residuals(mdl$fit))),
               RT2 = 1 - mse/den,
               logLik = logLik(mdl$fit),
               AIC = 2*( length(m)+1 ) + n*(log(2*pi)+log(mse)+1),
               BIC = log(n)*( length(m)+1 ) + n*(log(2*pi)+log(mse)+1),
               YIC = sum(log( diag(V)) / (m^2) ) + log(mse/den)
               )
}
               

fz <- function(x){
    z <- x
    z[is.finite(z)] <- rank(x, na.last=NA) / (sum(is.finite(x))+1)
    z <- qnorm(z)
    return(z)
}

fMarg <- function(x,n=1000){
    z <- fz(x)
    idx <- order(x,na.last=NA) ## should remove NA values
    z <- z[idx]
    x <- x[idx]
    x <- unique(x)
    z <- unique(z)
    xx <- seq(min(x),max(x),length=n)
    zz <- approx(x,z,xout=xx)$y
    return( list(x=xx,z=zz) )
}

fApplyMarg <- function(mrg,x){
    lb <- qnorm( pnorm( min(mrg$z))/2 )
    ub <- qnorm( (1+pnorm(max(mrg$z)))/2 )
    z <- approx(mrg$x,mrg$z,xout=x,yleft=lb, yright=ub)$y
    z <- matrix( z,nrow(x) )
    return(z)
}

fUndoMarg <- function(mrg,z){
    x <- approx(mrg$z,mrg$x,xout=z,yleft=min(mrg$x)/2, yright=1.5*max(mrg$x) )$y
    x <- matrix( x,nrow(z) )
    return(x)
}

fCop <- function(V,mV,x,S=NULL){
    ## create mean
    rnms <- colnames(V)
    mn <- mV[rnms]
    #mn <- rep(0,ncol(V)); names(mn) <- rnms

    ## check x
    nms <- names(x)[is.finite(x)]
    nms <- intersect(nms,rnms)
    if(length(nms)==0){
        warning("No valid obs")
        return( list(mn = mn, Vr=V) )
    }

    if(is.null(S)){
        S <- matrix(0,length(nms),length(nms),dimnames=list(nms,nms))
    }
    S <- S[nms,nms,drop=FALSE]
    if(!all(is.finite(S))){ stop("Invalid covariance matrix") }
    
    ## assimilate the data
    
    if(all(S==0)){
        
        ## based on marginal calculation for normal
        
        unms <- setdiff(rnms,nms)
        if(length(unms)==0){
            return( list(mn=x[rnms],V=S[rnms,rnms]) )
        }
        
        V21 <- V[nms,unms]
        V12 <- V[unms,nms]
        V22 <- V[nms,nms]
        B <- V12 %*% solve(V22)
        uV <- V[unms,unms] - B %*% V21
        umn <-  mn[unms] + B %*% (x[nms] - mn[nms])

        out <- list(mn = mn, Vr=V)
        out$mn[unms] <- umn
        out$mn[nms] <- x[nms]
        out$Vr[unms,unms] <- uV
        out$Vr[,nms] <- 0
        out$Vr[nms,] <- 0
    }else{
        ## got here
        ## based on Kalman Filter aka RLS
        
        I <- diag(length(rnms))
        rownames(I) <- rnms
        B <- I[nms,,drop=FALSE]
        e <- x[nms] - mn[nms]
        k <- V %*% t(B) %*% solve(B %*% V %*% t(B) + S)
        mn <- mn + k %*% e
        Vr <- (I - k %*% B) %*% V
        out <- list(mn = drop(mn), Vr=Vr)
    }

    return(out)
}

fread <- function(g,dataPath){
    tmp <- read.csv(file.path(dataPath,paste0(g,".csv.gz")),header=FALSE)
    tmp[[2]] <- as.numeric(tmp[[2]])
    names(tmp) <- c("dateTime",g)
    return(tmp)
}

fgetData <- function(g,apiPath,collapse=FALSE){
    tmp <- jsonlite::fromJSON(file.path(apiPath,"series",g))
    tmp <- tmp$properties$data
    if(length(tmp)==0){
        tmp <- data.frame(dateTime=character(0),obs=numeric(0),fcst=numeric(0))
    }
    tmp$obs <- as.numeric(tmp$obs)
    tmp$fcst <- as.numeric(tmp$fcst)
    if(collapse){
        tmp$obs[is.na(tmp$obs)] <- tmp$fcst[is.na(tmp$obs)]
    }
    tmp <- tmp[,c("dateTime","obs")]
    names(tmp) <- c("dateTime",g)
    return(tmp)
}
