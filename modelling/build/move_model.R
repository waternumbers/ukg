## clean up the old model format to look like the new one
rm(list=ls())

ld <- setdiff(list.dirs(recursive = FALSE),".")



for(ii in ld){
    print(ii)
    fnm <- file.path(ii,"model.rds")
    nfnm <- file.path("../models",paste0(ii,".rds"))
    file.copy(fnm,nfnm)
}
