##         browser()
##     f <- list("type" = "Feature",
##               "geometry" = list(
##                   "type"= "Point",
##                   "coordinates" = c(s$long,s$lat)
##               ),
##               "properties" = s[setdiff(names(s),c("long","lat"))]
##               )
rm(list=ls())
library(DBI)
library(plumber)
## requires: RSQLite (DBI), jsonlite, plumber, DBI, readr


## ## ########################################################################
## station_table_to_list

dbFile <- "test.sqlite"
time_window <- 4*24*60*60
port <- 5678



## #########################################################################
## handler functions

## digesting observations
digest_obs_handler <- function(req){
    ##D <- read.csv(D)
    D <- read.csv(text=req$body)
    if(!all(c("measure","dateTime","value") %in% names(D))){ stop("Data not in correct format") }
    D$mid <- basename(D$measure); D$measure <- NULL
    suppressWarnings({D$value <- as.numeric(D$value)})
    
    mydb <- DBI::dbConnect(RSQLite::SQLite(), dbFile)
    on.exit( dbDisconnect(mydb) )
    
    stationTbl <- dbReadTable(mydb,"series") # 'SELECT * FROM series')
    ## check data - should check types...
    D <- D[D$mid %in% stationTbl$mid,] ## trim D to those measures present in the data set
    
    ## add data to database
    n <- dbExecute(mydb,'INSERT INTO obs (mid,dateTime,value) VALUES( :mid, :dateTime, :value) ON CONFLICT(mid,dateTime) DO UPDATE SET value=excluded.value;',param=D)

    ## update latest obs time
    maxTime <- by(D$dateTime,D$mid,max)
    tblTime <- setNames(stationTbl$lastObs,stationTbl$mid)
    tblTime[names(maxTime)] <- pmax(tblTime[names(maxTime)], maxTime)
    
    stationTbl$lastObs <- tblTime
    m <- dbExecute(mydb,'UPDATE series SET lastObs = :lastObs WHERE mid = :mid;',param=stationTbl[,c("mid","lastObs")])
    ## delete old records
    dbExecute(mydb,'DELETE FROM obs WHERE dateTime < :dt',
              param=list(dt= format(Sys.time()-time_window,"%Y-%m-%dT%H:%M:00Z",tz="GMT")))

    return(c(n,m))
}

digest_fcst_handler <- function(req){
    
    ##D <- read.csv(D)
    
    D <- jsonlite::fromJSON(req$body)
#    print(names(D))
    if(!all(c("mid","issueTime","data") %in% names(D))){ stop("JSON not in correct format") }
    if(!all(c("dateTime","fcst","fcst_upper","fcst_lower") %in% names(D$data))){
        stop("Data not in correct format") }
    mydb <- DBI::dbConnect(RSQLite::SQLite(), dbFile)
    on.exit( dbDisconnect(mydb) ) ##; print(.last.error())} )
    
    stationTbl <- dbReadTable(mydb,"series") # 'SELECT * FROM series')
    
#    print(head(stationTbl))

    if(!(D$mid %in% stationTbl$mid)){ stop("Unknown mid") }

#    print("writting csv data")
    cn <- textConnection("csvData","w")
    write.csv(D$data[,c("dateTime","fcst","fcst_upper","fcst_lower")],cn,row.names=FALSE)
    close(cn)
#    print(head(csvData))
#    print("written csv data")
    csvData <- paste(csvData,collapse="\n")
#    print("merged csv data")
    ## add data to database
    n <- dbExecute(mydb,'INSERT INTO fcst (mid,dateTime,value) VALUES( :mid, :dateTime, :value) ON CONFLICT(mid,dateTime) DO UPDATE SET value=excluded.value;',
                   param = list(mid=D$mid, dateTime = D$issueTime, value=csvData))
#    print("added csv data")
    
    ## update latest forecat time
    ii <- which( stationTbl$mid == D$mid )
#    print(ii)
    stationTbl$lastForecast[ii] <- max( stationTbl$lastForecast[ii], D$issueTime, na.rm=TRUE)
    m <- dbExecute(mydb,'UPDATE series SET lastForecast = :lastForecast WHERE mid = :mid;',param=list(mid=D$mid,lastForecast=stationTbl$lastForecast[ii]))
    ## delete old records
    dbExecute(mydb,'DELETE FROM fcst WHERE dateTime < :dt',
              param=list(dt= format(Sys.time()-time_window,"%Y-%m-%dT%H:%M:00Z",tz="GMT")))

    return(c(n,m))
}

summary_handler <- function(){
    mydb <- DBI::dbConnect(RSQLite::SQLite(), dbFile)
    on.exit( dbDisconnect(mydb) )
    stationTbl <- dbReadTable(mydb,"series")
    f <- function(s){            
        list("type" = "Feature",
             "geometry" = list(
                 "type"= "Point",
                 "coordinates" = c(s$lon,s$lat)
             ),
             "properties" = as.list(s[setdiff(names(s),c("lon","lat"))])
             )
    }
    out <- list(type = "FeatureCollection",
                features = lapply(seq_len(nrow(stationTbl)), function(i){f(stationTbl[i,])}))
    return(out)
}
    
series_handler <- function(mid){
 #   print(mid)
   
    mydb <- DBI::dbConnect(RSQLite::SQLite(), dbFile)
    on.exit( dbDisconnect(mydb) )
    
    s <- dbGetQuery(mydb,"SELECT * FROM series WHERE mid = :x",param=list(x=mid))
    if(nrow(s)==0){ stop("Unknown series") }
    out <- list("type" = "Feature",
                "geometry" = list(
                    "type"= "Point",
                    "coordinates" = c(s$lon,s$lat)
                ),
                "properties" = as.list(s[setdiff(names(s),c("lon","lat"))])
                )
    d <- dbGetQuery(mydb,"SELECT dateTime,value FROM obs WHERE mid = :x ORDER BY dateTime ASC;",param=list(x=mid))
    names(d) <- c("dateTime","obs")
    
    f <- dbGetQuery(mydb,"SELECT value FROM fcst WHERE mid = :x AND dateTime = :dt;",
                    param=list(x=mid,dt=out$properties$lastForecast))$value
    if(length(f)==0){
        d$fcst <- d$fcst_upper <- d$fcst_lower <- rep(NA,nrow(d))
    }else{
        d <- merge(d,read.csv(text=f),by="dateTime",all=TRUE,sort=TRUE)
    }
    out$properties$data <- d
    return(out)
}

lastest_time_handler <- function(){
    mydb <- DBI::dbConnect(RSQLite::SQLite(), dbFile)
    on.exit( dbDisconnect(mydb) )
    D <- dbGetQuery(mydb,"SELECT mid,lastObs,lastForecast FROM series")
    return(D)
}

    
test_csv_handler <- function(req) {
    df <- read.csv(text=req$body)
    print(head(df))
    return(nrow(df))
}


## ############################################################################
## routes
## ############################################################################
root <- plumber::pr()
root %>%
    plumber::pr_filter("CORS", function(req,res){
        res$setHeader("Access-Control-Allow-Origin", "*")
        if (req$REQUEST_METHOD == "OPTIONS") {
            res$setHeader("Access-Control-Allow-Methods","*")
            res$setHeader("Access-Control-Allow-Headers", req$HTTP_ACCESS_CONTROL_REQUEST_HEADERS)
            res$status <- 200
            return(list())
        } else {
            plumber::forward()
        }
    }) %>%
    plumber::pr_get(path = "/series",
                    handler = summary_handler,
                    serializer = plumber::serializer_unboxed_json(na = "null"),
                    comments = "Returns a list of series"
                    ) %>%
    plumber::pr_get(path = "/series/<mid>",
                    handler = series_handler,
                    serializer = plumber::serializer_unboxed_json(na = "null"),
                    comments = "Returns etails of a series"
                    ) %>%
    plumber::pr_post(path = "/obs",
                     handler = digest_obs_handler,
                     serializer = plumber::serializer_unboxed_json(na = "null"),
                     comments = "Post observed data to the server"
                     ) %>%
    plumber::pr_post(path = "/fcst",
                     handler = digest_fcst_handler,
                     serializer = plumber::serializer_unboxed_json(na = "null"),
                     comments = "Post forecast data to the server"
                     ) %>%
    plumber::pr_get(path = "/latest",
                    handler = lastest_time_handler,
                     serializer = plumber::serializer_unboxed_json(na = "null"),
                     comments = "latest times"
                     ) %>%
    plumber::pr_post(path = "/test",
                     ## parser = c("multi","csv"),
                     handler = test_csv_handler,
                     serializer = plumber::serializer_unboxed_json(),
                     comments = "Post observed data to the server"
                     ) %>%
    pr_set_error(function(req, res, err){
        res$status <- 500
        list(error = err$message)
    })
## And run it!
root %>% plumber::pr_run(port=port)

