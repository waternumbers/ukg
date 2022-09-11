## crawl through the EA archive getting data
rm(list=ls())

outDir <- "."
baseURL <- "http://environment.data.gov.uk/flood-monitoring/archive"
options(timeout = max(400, getOption("timeout"))) ## extent the timeout

dataPath <- "./archive"
postURL <- "http://127.0.0.1:5678/obs"

day <- as.Date(Sys.time(),tz="GMT")

fnm <-  paste0("./archive/readings-", day-0:5,".csv")

## clean up old files
unlink( file.path(dataPath,setdiff( list.files(dataPath), basename(fnm)) ))

## see which need to be downloaded
fnm <- fnm[ !file.exists(fnm) ]


for(ii in fnm){
    ld <- basename(ii)
    print(ld)
    ## download file
    tryCatch({
        download.file(file.path(baseURL,ld),ii)
    },error=function(e){
        ## failed remove the downloaded file
        unlink(ii)
        warning(e)
    })
    
    if(file.exists(ii)){
        p <- httr::POST( url = postURL, body=httr::upload_file(ii))
        if( httr::http_error(p) ){
            unlink(ii)
            warnings( paste("Upload error for",ii) )
        }
    }

}
