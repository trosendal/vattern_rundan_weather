library(RCurl)
library(jsonlite)
library(sp)
library(svamap)
## Get the forecast of a point:
weather <- function(lat, long){
    url <- paste0("https://opendata-download-metfcst.smhi.se/api/category/pmp3g/version/2/geotype/point/lon/",
                  long,
                  "/lat/",
                  lat,
                  "/data.json")
                  df <- fromJSON(RCurl::httpGET(url))
                  time <- as.POSIXct(df$timeSeries$validTime, tz = "Europe/Stockholm", format = "%Y-%M-%dT%H:%M:%SZ")
                  temp <- unlist(lapply(df$timeSeries$parameters, function(x) {x$values[x$name == "t"]}))
                  wd <- unlist(lapply(df$timeSeries$parameters, function(x) {x$values[x$name == "wd"]}))
                  ws <- unlist(lapply(df$timeSeries$parameters, function(x) {x$values[x$name == "ws"]}))
                  pmin <- unlist(lapply(df$timeSeries$parameters, function(x) {x$values[x$name == "pmin"]}))
                  pmax <- unlist(lapply(df$timeSeries$parameters, function(x) {x$values[x$name == "pmax"]}))
                  gust <- unlist(lapply(df$timeSeries$parameters, function(x) {x$values[x$name == "gust"]}))
                  df2 <- data.frame(time = time, temp = temp, wd = wd, ws = ws, pmin = pmin, pmax = pmax, gust = gust)
                  return(df2)
    }
## Calculate the times for a pace and start_time
times <- function(t0,
                  pace) {
    df <- read.csv("coordinates.csv", stringsAsFactors = FALSE)
    df$hours <- df$distance/pace
    df$time = t0 + (df$hours*60*60)
    return(df)
}
## Match the weather to the locations and times (closest in time at the place)
vattern_pred <- function(start,
                         pace) {
    df <- times(start, pace)
    df3 <- do.call("rbind", lapply(seq_len(nrow(df)), function(x){
        df2 <- weather(df[x,]$lat, df[x,]$long)
        df2 <- df2[which(min(as.numeric(abs(df2$time-df[x,]$time))) == as.numeric(abs(df2$time-df[x,]$time)))[1],]
        names(df2)[names(df2) == "time"] <- "weather_time"
        return(df2)
    }))
    cbind(df, df3)
}
## Format the information neatly for presentation:
map_format <- function(df){
    df$direction <- round(df$wd/45, 0)
    df$direction <- factor(df$direction,
                           levels = 0:8,
                           labels = c("Norr", "Norröst",
                                      "Öst", "Sydöst" ,
                                      "Söder", "Sydväst",
                                      "Väster", "Nordväst",
                                      "Norr2"))
    df$direction <- as.character(df$direction)
    df$direction[df$direction == "Norr2"] <- "Norr"
    df$popup <- paste0("Du är i ", df$Place,
                       "<br>Klockan: ", df$time,
                       "<br>Det är ", df$temp, "C",
                       "<br>Det blåser ", df$ws, "m/s(", df$gust, "m/s byvind) ", "från ", df$direction, " ",
                       "<br>Nederbörd från ", df$pmin, " till ", df$pmax, " mm/t",
                       "<br><br>Den närmaste väderprognosen är från ", df$weather_time,
                       "<br>Uppdaterad ", Sys.time())
    pts <- SpatialPointsDataFrame(SpatialPoints(cbind(df$long, df$lat)), df)
    return(pts)
}
##
##
##
## Get the race_times/weather info and format it:
df <- vattern_pred(start = as.POSIXct("2017-06-16 19:38:00 CEST", tz = "Europe/Stockholm"),
                   pace = 36)
pts <- map_format(df)
## Write the points to json
path_to_data <- svamap::write_data(pts)
## Plot them on a map:
svamap::write_page(data = path_to_data,
                   path = "~/Desktop/vattern/830/",
                   owntemplate = "map.html",
                   overwrite = TRUE,
                   browse = FALSE,)
temp <- readLines("~/.epi-cloudftp_credentials")
cred <- paste0("ftp://", temp[2], ":", temp[3], "@", temp[1], "/vattern/830/")
files <- list.files("~/Desktop/vattern/830/map/", full.names = TRUE)
for(i in files) {
    ftpUpload(i, paste0(cred, basename(i)))
}
df <- vattern_pred(start = as.POSIXct("2017-06-16 19:38:00 CEST", tz = "Europe/Stockholm"),
                   pace = 32)
pts <- map_format(df)
## Write the points to json
path_to_data <- svamap::write_data(pts)
## Plot them on a map:
svamap::write_page(data = path_to_data,
                   path = "~/Desktop/vattern/930/",
                   owntemplate = "map.html",
                   overwrite = TRUE,
                   browse = FALSE,)
temp <- readLines("~/.epi-cloudftp_credentials")
cred <- paste0("ftp://", temp[2], ":", temp[3], "@", temp[1], "/vattern/930/")
files <- list.files("~/Desktop/vattern/930/map/", full.names = TRUE)
for(i in files) {
    ftpUpload(i, paste0(cred, basename(i)))
}
