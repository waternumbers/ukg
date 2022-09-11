## clean up the old model format to look like the new one
rm(list=ls())

ld <- setdiff(list.dirs(recursive = FALSE),".")

fcop <- function(cp){
    rng <- range(cp$x,na.rm=TRUE)
    out <- list(x = seq(rng[1],rng[2],length=1000))
    out$z <- approx(cp$x,cp$z,xout=out$x)$y
    return(out)
}



for(ii in ld){
    print(ii)
    fnm <- file.path(ii,"model.rds")
    mdl <- readRDS(fnm)
    if( "fit" %in% names(mdl) ){
        file.copy(fnm,paste0(fnm,".old"))
        mdl$param <- coef(mdl$fit)
        mdl$fit <- NULL

        mdl$copula$mrgX <- fcop(mdl$copula$mrgX)
        for(ii in 1:length(mdl$copula$mrgE)){
            mdl$copula$mrgE[[ii]] <- fcop(mdl$copula$mrgE[[ii]])
        }
        saveRDS(mdl,fnm)    
    }
}
