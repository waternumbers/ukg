## Create new databse
## !!!!!!!! this will remove any existing db file !!!!!!!!

rm(list=ls())
library("DBI")

dbFile <- "test.sqlite"
unlink(dbFile)

st <- read.csv("../gauge_setup/used_stations.csv")

st$easting <- st$northing <- NULL
st$lastObs <- st$lastWarning <- st$lastForecast <- st$lastForecastWarning <- "1900-01-01T00:00:00Z"

mydb <- dbConnect(RSQLite::SQLite(), dbFile)


dbExecute(mydb,"CREATE TABLE obs(mid TEXT NOT NULL, dateTime TEXT NOT NULL, value REAL, PRIMARY KEY (mid,dateTime));")
dbExecute(mydb,"CREATE TABLE fcst(mid TEXT NOT NULL, dateTime TEXT NOT NULL, value BLOB, PRIMARY KEY (mid,dateTime));")

dbExecute(mydb,"CREATE TABLE series(period REAL, unitName TEXT, datum REAL,
minRecord REAL, typicalLow REAL, typicalHigh REAL,        
maxRecord REAL, scaleMax REAL, type TEXT NOT NULL,               
mid TEXT NOT NULL, id TEXT, lon REAL NOT NULL,  
lat REAL NOT NULL, featureName TEXT, town TEXT,
lastForecastWarning TEXT NOT NULL, lastForecast TEXT NOT NULL,
lastWarning TEXT NOT NULL, lastObs TEXT NOT NULL, PRIMARY KEY (mid));")

dbAppendTable(mydb, "series", st)
dbDisconnect(mydb)


