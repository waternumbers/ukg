rm(list=ls())
source("fit.R")
library("parallel")

source("dbm_rev.R")

setup <- readRDS("./spatial_processing/link_descriptions.rds")
names(setup) <- sapply(setup,function(x){x$gauge})

## From debugging - in the end "L3203-level-stage-i-15_min-mAOD" din't run
## it looks like a tide gauge and isn't upstream
## toRun <- c("50115-level-stage-i-15_min-m",    "47115-level-stage-i-15_min-m",   
##            "L3203-level-stage-i-15_min-mAOD", "45121-level-stage-i-15_min-m") 
## setup <- setup[toRun]
## fitModel(setup[[3]])

ncore <- detectCores() - 2

fx <- function(x){
    tryCatch({ fitModel(x) },error=function(e){ return(e$message) })
}
