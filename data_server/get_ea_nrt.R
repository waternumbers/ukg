## crawl through the EA archive getting data
rm(list=ls())

outDir <- "."
baseURL <- "http://environment.data.gov.uk/flood-monitoring/archive"
options(timeout = max(400, getOption("timeout"))) ## extent the timeout

postURL <- "http://127.0.0.1:5678/obs"
timeURL <- "http://127.0.0.1:5678/latest"

## get latest reading from EA
ea <- read.csv("https://environment.data.gov.uk/flood-monitoring/data/readings.csv?latest")
db <- jsonlite::fromJSON(timeURL)
ea$mid <- basename(ea$measure)
db <- merge(db,ea,by="mid")
ea$mid <- NULL ## remove so can take the structure later

db <- db[db$lastObs < db$dateTime,] ## trim to those with new data


cnt <- c(0,nrow(db))

if(nrow(db) > 0){
    
    ## check to see if any are teh next timestep
    ## Relies on 900s time gap
    dStr <- "%Y-%m-%dT%H:%M:%SZ"
    db$nextObs <- format( as.POSIXct(db$lastObs,format=dStr,tz="UTC")+900,dStr,tz="UTC")
    
    idx <- db$nextObs>=db$dateTime
    D <- db[idx,names(ea)]
    cnt[1] <- cnt[1]+sum(idx)
    db <- db[!idx,]
    
    nD <- 1000 ## write to data base when number of lines exceed this value
    
    callStr <- "https://environment.data.gov.uk/flood-monitoring/id/measures/%s/readings.csv?_sorted&_limit=10000&since=%s"
    
    for(ii in 1:nrow(db)){
        ##print( paste(ii,"of",nrow(db)) )
        csvURL <- sprintf(callStr,db$mid[ii],db$lastObs[ii])
        
        newData <- tryCatch({
            read.csv(csvURL)
        },error=function(e){
            ## failed remove the downloaded file
            warning(e)
            NULL
        })
        cnt[1] <- cnt[1] + !is.null(newData)
        D <- rbind(D,newData)
        
        if(nrow(D)>nD){
            fnm <- tempfile()
            write.csv(D,fnm,row.names=FALSE)
            p <- httr::POST( url = postURL, body=httr::upload_file(fnm))
            if( httr::http_error(p) ){
                warnings( paste("Upload error around",db$mid[ii]) )
            }
            unlink(fnm)
            D <- NULL
        }
    }
    
    fnm <- tempfile()
    write.csv(D,fnm,row.names=FALSE)
    p <- httr::POST( url = postURL, body=httr::upload_file(fnm))
    if( httr::http_error(p) ){
        warnings( paste("Upload error around",db$mid[ii]) )
    }
    unlink(fnm)
    D <- NULL
}

print( paste(Sys.time(),":",cnt[1],"of",cnt[2]) )
    
