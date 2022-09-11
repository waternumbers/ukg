## create a tree structure of the upstream gauges for each river station
rm(list=ls())
library(sf)
library(terra)

## read in the channel
chn <- st_read("channel.shp") #static_data/OS/OS_Open_Rivers_202204/data/WatercourseLink.shp")
dem <- rast("filled_dem2.tif")

## read in stations
ag <- read.csv("../../gauge_setup/used_stations.csv")
ag$gauge_num <- 1:nrow(ag)
sg <- ag[ag$type=="stage",]
sg <- st_as_sf(sg,coords=c("easting","northing"),crs=st_crs(chn))
rg <- ag[ag$type=="tbr",]
rg <- st_as_sf(rg,coords=c("easting","northing"),crs=st_crs(chn))

## assign the gauges to a channel length
idx <- sf::st_nearest_feature(sg,chn)

if( any(table(idx)> 1) ){ stop("The rest fo the code assumes only only gaguge per reach") }

## assign gauge num to a channel then work it upstream
## work out upstream gauges while it is being done
chn$gauge_num <- as.numeric(NA)
chn$gauge_num[idx] <- sg$gauge_num

lnk <- list()
cnt <- 0
for(ii in idx){
    cnt <- cnt+1
    print( paste(cnt,"of",length(idx)))
    idnt <- chn$identifier[ii]
    val <- chn$gauge_num[ii]
    us <- idnt
    usg <- NULL
    while(length(idnt)>0){
        idnt <- chn$identifier[ chn$endNode %in% chn$startNode[chn$identifier %in% idnt ] ]
        jdx <- match(idnt, chn$identifier)
        gn <- chn$gauge_num[ jdx ]
        kdx <- is.finite(gn)
        usg <- c(usg, gn[kdx])
        chn$gauge_num[ jdx[!kdx] ] <- val
        idnt <- idnt[!kdx]
    }
    lnk[[(length(lnk)+1)]] <- list(gauge_num = val,
                                   upstream_gauge_num = usg)
}

range(sapply(lnk,function(s){length(s$upstream_gauge_num)})) # 0-7...
tmp <- unlist(lapply(lnk,function(s){s$upstream_gauge_num}))
if( !(length(tmp)==length(unique(tmp)))){stop("no gauge may goto two downstreeam gauges")} ## no gauge goes to two downstream gauges :-)

## create a catchment map
ctch <- terra::rasterize(sg,dem,field = "gauge_num",touches=TRUE)
chn$tmp <- pmax(chn$gauge_num,-999,na.rm=TRUE)
ctch <- terra::cover(ctch,terra::rasterize(chn,dem,field = "tmp",touches=TRUE))
if( !all( sg$gauge_num %in% unique(as.vector(ctch),na.rm=TRUE)) ){
    stop("some gauges don't have initial catchments...")
}
chn$tmp <- NULL

## pass through dem to expand catchment areas
d <- terra::as.matrix( dem , wide=TRUE )
ch <- terra::as.matrix( ctch,  wide=TRUE )

idx <- order(d,na.last=NA)
n_to_eval <- length(idx)
print_step <- round(n_to_eval/20)
next_print <- print_step
it <- 1
rs <- terra::res( dem )
dxy <- rep(sqrt(sum(rs^2)),8)
dxy[c(2,7)] <- rs[1]; dxy[c(4,5)] <- rs[2]
dcl <- c(0.35,0.5,0.35,0.5,0.5,0.35,0.5,0.35)*mean(rs)
nr <- nrow(d); delta <- c(-nr-1,-nr,-nr+1,-1,1,nr-1,nr,nr+1)

for(ii in idx){
    if(is.finite(ch[ii])){ next }

    ## it is already assigned
    jdx <- ii+delta
    gcl <- (d[jdx]-d[ii])*dcl/dxy
    if( any(gcl<0,na.rm=TRUE) ){
        ## can go downhill at all
        kdx <- which.min(gcl)
    }else{
        if( !any(is.na(gcl)) ){
            browser()
            stop("unexpected sink") }
        kdx <- which.max(is.na(gcl))
    }
    ch[ii] <- ch[jdx[kdx]]
    
    ## verbose output here
    if(it >= next_print){
        cat(round(100*it / n_to_eval,1),
            "% complete","\n")
        next_print <- next_print+print_step
    }
}

ch[ch<1] <- NA
ctch <- terra::rast(ctch,name="catchments",val=ch)
plot(ctch)
terra::writeRaster(ctch, "catchments.tif",overwrite=TRUE)

## work out rainfall weights
vg <- terra::voronoi(terra::vect(rg), bnd=NULL, tolerance=0, as.lines=FALSE, deldir=FALSE)

rvg <- terra::rasterize(vg,ctch,field="gauge_num")
rvg <- terra::mask(rvg,ctch)

zz <- terra::zonal(rvg,ctch,fun=table)

for(ii in 1:length(lnk)){
    idx <- which(zz$catchments==lnk[[ii]]$gauge_num)
    if(length(idx)!=1){ stop( "Whoops...") }
    lnk[[ii]]$gauge <- ag$mid[lnk[[ii]]$gauge_num]
    lnk[[ii]]$precip_gauge <- data.frame(
        mid = ag$mid[ as.integer(names(zz$gauge_num[[idx]])) ],
        weight = as.numeric(zz$gauge_num[[idx]]) / sum(as.numeric(zz$gauge_num[[idx]])))
    lnk[[ii]]$upstream_gauge <- ag$mid[lnk[[ii]]$upstream_gauge]
}

saveRDS(lnk,"link_descriptions.rds")                                         
