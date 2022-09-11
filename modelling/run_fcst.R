rm(list=ls())
source("fsct.R")
source("dbm_rev.R")

setup <- readRDS("./spatial_processing/link_descriptions.rds")
names(setup) <- sapply(setup,function(x){x$gauge})

sqn <- NULL
fsqn <- function(s){ all(s$upstream_gauge %in% sqn) | (length(s$upstream_gauge)==0) }
tmp <- setup

cnt <- 0
while(length(tmp)>0){
    cnt <- cnt+1
    idx <- sapply(tmp,fsqn)
    sqn <- c(sqn,names(tmp)[idx])
    tmp <- tmp[!idx]
}

out <- list()
cnt <- c(0,length(sqn))
for(ii in sqn){    
    flg <- try( {fcstModel(ii)} )
    cnt[1] <- cnt[1] + !(class(flg) == "try-error")
}
print( paste(Sys.time(),":",cnt[1],"of",cnt[2]) )
