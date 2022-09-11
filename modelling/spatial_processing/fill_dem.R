## map the dem cells to the nearest (D8) channel reach

## #########################################
## buffer channel
## #########################################
rm(list=ls())
graphics.off()
library(terra)
dem <- rast("dem.tif")
chn <- vect("channel.shp")

## create a raster of channel id numbers
## TODO - possibly sort so do biggest area first???
chn_rst <- terra::rasterize(chn,dem,touches=TRUE)
names(chn_rst) <- "channel"

d <- terra::as.matrix( dem ,wide=TRUE) ##, mat=TRUE)
ch <-terra::as.matrix( chn_rst , wide=TRUE )
           
## values that should be valid
is_valid <- is.finite(ch) | is.na(d) # All channel and edge cells

changed <- is_valid # cells changed at last iteration
changed[c(1,nrow(changed)),] <- FALSE
changed[,c(1,ncol(changed))] <- FALSE

fd <- d; fd[] <- Inf; fd[is_valid] <- d[is_valid]
to_eval <- changed; to_eval[] <- FALSE

## distance between cell centres
min_grad <- 1e-4
rs <- terra::res( dem )
dxy <- matrix(sqrt(sum(rs^2)),3,3)
dxy[1,2] <- dxy[3,2] <- rs[2]
dxy[2,1] <- dxy[2,3] <- rs[1]
dxy[2,2] <- 0
dxy <- min_grad*dxy

it <- 1
max_it <- 1000
## start of iteration loop
while(any(changed[]) & it<=max_it){   
    ## work out all the the cells to evaluate
    ## should be next to those that are changed
    to_eval[] <- FALSE
    idx <- which(changed,arr.ind=TRUE) # index of changed cells
    jdx <- idx
    for(ii in c(-1,0,1)){
        for(jj in c(-1,0,1)){
            if(ii==0 & jj==0){next}
            ## adjust jdx
            jdx[,1] <- idx[,1]+ii
            jdx[,2] <- idx[,2]+jj
            to_eval[jdx] <- !is_valid[jdx] #TRUE
        }
    }
    
    cat("Iteration",it,"\n")
    cat("\t","Cells to evaluate:",sum(to_eval),"\n")
    cat("\t","Percentage Complete:",
        round(100*sum(is_valid)/ncell(d),1),"\n") #to_be_valid),1),"\
    ## alter min value for the evaluation cells
    idx <- which(to_eval,arr.ind=TRUE) # index of cells to evaluate
    jdx <- idx
    mind <- rep(Inf,nrow(idx))
    for(ii in c(-1,0,1)){
        for(jj in c(-1,0,1)){
            if(ii==0 & jj==0){next}
            ## adjust jdx
            jdx[,1] <- idx[,1]+ii
            jdx[,2] <- idx[,2]+jj
            mind <- pmin(mind,fd[jdx] + dxy[ii+2,jj+2])
        }
    }
    changed[] <- FALSE
    is_valid[idx] <- is.na(mind) | (d[idx]>=mind) ## cells where the dem value is valid
    mind <- pmax(d[idx],mind,na.rm=TRUE) ## mind is now the replacemnt value
    if(any(is.infinite(mind))){ browser() }
    changed[idx] <- mind < fd[idx]
    fd[idx] <- mind
    it <- it+1
}

if(it>max_it){ stop("Maximum number of iterations reached, sink filling not complete") }

filled_dem <- terra::rast( dem, names="filled_dem", vals=fd )
terra::writeRaster(filled_dem, "filled_dem2.tif",overwrite=TRUE)
           
