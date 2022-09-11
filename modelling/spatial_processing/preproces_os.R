## This file preprocesses the OS data

## #########################################
## aggregate and buffer dem, setting all coastal values <= 0 to NA
## #########################################
rm(list=ls())
graphics.off()
library(terra)
dem <- rast("../../static_data/OS/OS_Terrain_50_202208/gb50.tif")
dem <- aggregate(dem,10) ## collapse to 500 m
dem <- extend(dem,c(1,1)) # ,NA

tmp <- dem
tmp[tmp<0] <- NA

## make na clumps
na_clumps <- terra::patches(is.na(tmp), directions=8, zeroAsNA=TRUE)
## work out edge clump numbers
edge_values <- unique( c(
    terra::unique( terra::values(na_clumps, mat=TRUE, dataframe=FALSE, row=1,nrows=nrow(na_clumps), col=1, ncols=1, na.rm=TRUE) ),
    terra::unique( terra::values(na_clumps, mat=TRUE, dataframe=FALSE, row=1,nrows=nrow(na_clumps), col=ncol(na_clumps), ncols=1, na.rm=TRUE) ),
    terra::unique( terra::values(na_clumps, mat=TRUE, dataframe=FALSE, row=1,nrows=1, col=1, ncols=ncol(na_clumps), na.rm=TRUE) ),
    terra::unique( terra::values(na_clumps, mat=TRUE, dataframe=FALSE, row=nrow(na_clumps),nrows=1, col=1, ncols=ncol(na_clumps), na.rm=TRUE) )
))
sink_values <- setdiff( unique(na_clumps,na.rm=TRUE)$patches, edge_values)

na_clumps <- terra::subst(na_clumps, sink_values, NA) ## clean out sink values so only edge values left
tmp <- is.na(na_clumps) ## land area is 1
tmp[tmp==0] <- NA

dem <- terra::cover(tmp,dem,1) # replace
writeRaster(dem,"dem.tif", names = "dem",overwrite=TRUE)

## ########################################
## Clean the channel data removing loops and canals
## ########################################
rm(list=ls())
graphics.off()
library(terra)
chn <- vect("../../static_data/OS/OS_Open_Rivers_202204/data/WatercourseLink.shp")

## remove loops
chn <- chn[ !(chn$identifier %in%
              c("D3EE3B6E-A468-4BAE-B8F5-5D6A2E00618C", # creates a loop from a "good" channel
                "62AFB08E-EF39-4BCD-A33C-4ADA703A540C", # in a loop
                "7AA73E98-12C3-4136-A064-3C9A780FABEB", # in a loop
                "EDC090E3-C456-4E12-A10B-DB6AD376C3B7", # in a loop
                "C5CCEE86-9080-4E4B-AE60-AE6D5AA26192", # in a loop
                "B6B3EEBC-8DF2-4B92-8605-E1F89FE8AB88", # in a loop
                "C9AA0A09-2DC1-4A84-AB60-810F8BFD8C1D", # in a loop
                "227E4C37-8B0A-4E5C-8327-B0BA95ABE309", # a loop
                "EA4F78B6-2F3B-457C-ABAC-35774833E660", # a loop
                "9AE50925-847E-4329-A172-82E87984D1AC", # a loop
                "1C9DBD75-4DDF-469F-A735-C1D83A1CA0D1") # a loop
),]

## change direction to match appernent sequence on the ground
revdir <- c("9D27C780-D1C3-453A-9654-542E83F22F0D",
            "DE5FBCBC-BC4D-4EA1-AE4E-39A9AEF869AA",
            "88D58FB1-EA5A-4D1A-9EE2-B9CD07F5DA47",
            "770B20A1-701A-46B7-B614-01A6A2A5F005",
            "1301058D-B545-4344-A365-E9E6603D33FC",
            "695D1879-7030-4FD8-9FC1-EB4D05A9A883",
            "E6D6724D-A84E-4F87-A507-19D63A8474EF")
tmp <- chn$endNode[ chn$identifier %in% revdir ]
chn$endNode[ chn$identifier %in% revdir ] <- chn$startNode[ chn$identifier %in% revdir ]
chn$startNode[ chn$identifier %in% revdir ] <- tmp

chn <- chn[chn$form != "canal",] ## remove canals...
chn$num_code <- 1:nrow(chn)
writeVector(chn, "channel.shp",overwrite=TRUE)
