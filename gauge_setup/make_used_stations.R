rm(list=ls())

## function for a single station
fs <- function(s){
    if(!all(c("long","lat") %in% names(s))){ return(NULL) }
    out <- list()
    for(m in s$measures){
        d <- as.data.frame(m)
        d[,c("datum","minRecord","typicalLow","typicalHigh","maxRecord","scaleMax")] <- as.numeric(NA)
        d$type <- d$qualifier
        d$mid <- basename(d$X.id);
        d$X.id <- d$parameter <- d$parameterName <- d$qualifier <- NULL
        sc <- NULL
        if( ("stageScale" %in% names(s)) & (d$type == "Stage") ){
            sc <- s$stageScale ## scale
        }
        if( ("downstageScale" %in% names(s)) & ("Downstream Stage" %in% d$type) 
           ){
            sc <- s$downstageScale
        }
        if(class(sc)== "list"){
            ## add scale
            if("datumOffset" %in% names(s)){d$datum <- s$datumOffset}
            if("minOnRecord" %in% names(sc)){d$minRecord <- sc$minOnRecord$value}
            if("typicalRangeLow" %in% names(sc)){d$typicalLow <- sc$typicalRangeLow}
            if("typicalRangeHigh" %in% names(sc)){d$typicalHigh <- sc$typicalRangeHigh }
            if("maxOnRecord" %in% names(sc)){d$maxRecord <- sc$maxOnRecord$value }
            ##d$scaleMax <- sc$scaleMax
        }
        out[[length(out)+1]] <- d
    }
    d <- do.call(rbind,out)
    d <- d[!(d$unitName=="---"),] ## remove suprisous looking records with no units
    d$type[d$type=="1"] <- "Stage" ## special case
    if( ("stageScale" %in% names(s)) ){
        d <- d[!(d$type %in% "Downstream Stage"),] ## drop downstream stage
    }
    if( ("downstageScale" %in% names(s)) & ("Downstream Stage" %in% d$type) ){
        d <- d[!(d$type %in% "Stage"),] ## drop stage
        d$type[d$type=="Downstream Stage"] <- "Stage" ## rename downstream stage
    }
    d <- d[(d$type %in% c("Stage","Tipping Bucket Raingauge","Tidal Level")),]    
    ##browser()
    ## add stations Info
    d$id <- s$stationReference
    d$lon <- s$long
    d$lat <- s$lat
    d$easting <- s$easting
    d$northing <- s$northing
    d$featureName <- as.character(NA)
    if( ("riverName" %in% names(s)) & ("Stage" %in% d$type) ){
        d$featureName[d$type=="Stage"] <- s$riverName
    }
    if("town" %in% names(s)){ d$town = s$town } else { d$town <- as.character(NA) }

    return(d)
}

## read in two input files
floodapi_stations <- jsonlite::fromJSON("flood_stations_full.json",simplifyVector=FALSE)[["items"]]
hydrologyapi_measures <- read.csv("hydrology_measures.csv")

## find stations with a stage reading
fstage <- function(s){
    any(c("Stage","Downstream Stage") %in% sapply(s$measures,function(m){m$qualifier}))
}
sdx <- sapply(floodapi_stations,fstage)

## find stations with a rainfall reading
frain <- function(s){
    "Tipping Bucket Raingauge" %in% sapply(s$measures,function(m){m$qualifier})
}
rdx <- sapply(floodapi_stations,frain)

## fin stations with a tidal level
ftide <- function(s){
    "Tidal Level" %in% sapply(s$measures,function(m){m$qualifier})
}
tdx <- sapply(floodapi_stations,ftide)

## stations that appear in the stationReference of the hydrology meansures for discharge
qdx <- sapply(floodapi_stations,function(s){s$stationReference}) %in% hydrologyapi_measures$station.stationReference[hydrologyapi_measures$parameter=="flow"]

## stations to keep
idx <- (sdx & qdx) | rdx | tdx
used_stations <- floodapi_stations[idx]

## convert to a data.frame of information
out <- list()
for(ii in 1:length(used_stations)){ out[[ii]] <- fs(used_stations[[ii]]) }

unique( unlist(sapply(out,ncol)) )
nm <- names(out[[1]])
if( !all( sapply(out,function(x){all(names(x)==nm)}) ) ){ stop("mismatched names") }

out <- do.call(rbind,out)
out$type[out$type=="Tipping Bucket Raingauge"] <- "tbr"
out$type[out$type=="Tidal Level"] <- "tidal"
out$type <- tolower(out$type)


## check there is archive data, exists and has been update recently
fnm <- list.files("/data/EnvironmentAgency/by_station/",full.names=TRUE)
fsize <- sapply(fnm,file.size)
ftime <- sapply(fnm,file.mtime)
idx <- (ftime > as.numeric(as.POSIXct("2022-05-01 00:00:00",tx="GMT"))) & (fsize > 2000)

fnm <- basename(fnm[idx])
idx <- paste0(out$mid,".csv.gz") %in% fnm
out <- out[idx,]


out <- out[!(out$unitName=="m3/s"),] ## there are some discharge recorded as stages...
## below are duplicates
remove_mid <- c(
    ## duplicate gauge records for differenent data feeds??
    "E70139-north_west-level-tidal_level-Mean-15_min-mAOD",
    "E73439-north_west-level-tidal_level-Mean-15_min-mAOD",
    "E73639-north_west-level-tidal_level-Mean-15_min-mAOD",
    ## duplicates on OS reach - removed for ease of analysis
    "43125-level-stage-i-15_min-m",
    "2844TH-level-stage-i-15_min-mASD",
    "690510-level-stage-i-15_min-m",
    "5290TH-level-stage-i-15_min-mASD",
    "2816TH-level-stage-i-15_min-mASD",
    "44220-level-stage-i-15_min-m",
    "3110TH-level-stage-i-15_min-mASD",
    "E21005-level-stage-i-15_min-m"
    )

out <- out[!(out$mid %in% remove_mid),]

## remove those without typical High levels
idx <- !is.finite(out$typicalHigh) & out$type=="stage"
out <- out[!idx,]

write.csv(out,"used_stations.csv",row.names=FALSE)

