## this script will merge the asc files into single tif for each square then merge these
rm(list=ls())
library(terra)

## tempory directory
tD <- "./tmp"
dir.create(tD)

## directory for keeping squares
sD <- "./sq"
dir.create(sD)

## list of directories of square in put files
sqDir <- list.files("./data",full.names=TRUE)

for(sq in sqDir){
    print(sq)
    fn <- list.files(sq,pattern=".zip$",full.names=TRUE)
    tmp <- sapply(fn,unzip,exdir=tD)
    fasc = list.files(tD, pattern=".asc$", full.names=T)
    oF <- file.path(sD,paste0(basename(sq),".tif"))
    tmp <- sprc(lapply(fasc,rast))
    tmp <- mosaic(tmp,filename=oF,overwrite=TRUE)
    rm(tmp)
    unlink(list.files(tD,full.names=TRUE))
}

## convert to one big tif
ftif = list.files(sD, pattern=".tif$", full.names=T)
tmp <- vrt(ftif)
oF <- "gb50.tif"
writeRaster(tmp,oF,overwrite=TRUE)

## remove temporya nd squares directory
unlink(tD,recursive = TRUE)
unlink(sD,recursive = TRUE)
