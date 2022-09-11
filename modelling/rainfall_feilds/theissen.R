## get weights for each river gauge  by theisen polygon of the rainfall gauge
rm(list=ls())

g <- read.csv("../../gauge_setup/used_stations.csv")
g <- g[g$type=="tbr",]

ctch <- terra::rast("../spatial_processing/catchment_code.tif")
ctch[ctch==0] <- NA

sg <- terra::vect(g,geom=c("easting", "northing"), crs=terra::crs(ctch))

vg <- terra::voronoi(sg, bnd=NULL, tolerance=0, as.lines=FALSE, deldir=FALSE)
vg$code <- 1:nrow(vg)

rg <- terra::rasterize(vg,ctch,field="code")
rg <- terra::mask(rg,ctch)

zz <- terra::zonal(rg,ctch,fun=table)

fc <- function(x){
    data.frame(mid=vg$mid[as.integer(names(x))],
               weight = as.numeric(x)/sum(as.numeric(x)))
}

tmp <- lapply(zz$code,fc)
names(tmp) <- zz$end_channel

saveRDS(tmp,"rain_gauge_weights.rds")


