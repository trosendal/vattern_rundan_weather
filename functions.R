library(RCurl)
library(rgdal)
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
    pts <- SpatialPointsDataFrame(coords = cbind(df$long, df$lat),
                                  data = df,
                                  proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
    return(pts)
}
mydist <- function(x1, y1, x2, y2) {
    sqrt((x1 - x2)^2 + (y1 - y2)^2)
}
bearing <- function(x1, y1, x2, y2) {
    angle <- atan((x2 - x1) / (y2 - y1)) * 180/pi
    if(x2 == x1) {
        if(y2>=y1) return(0)
        ## Q2
        if(y2<y1) return(180)
    }
    if(x2>x1) {
        ## Q1
        if(y2>=y1) return(angle)
        ## Q2
        if(y2<y1) return(180 + angle)
    }
    if(x2<x1) {
        ## Q3
        if(y2<=y1) return(180 + angle)
        ## Q4
        if(y2>y1) return(360 + angle)
    }
}
myslope <- function(x1, y1, z1, x2, y2, z2) {
    distance <- sqrt((x1 - x2)^2 + (y1 - y2)^2 + (z1 - z2)^2)
    rise <- z2 - z1
    run <- mydist(x1, y1, x2, y2)
    degslope <- asin(rise/distance) * 180/pi
    percslope <- 100 * (rise/run)
    percslope
}
diff.bearing <- function(b1, b2) {
    angle <- abs(b2 - b1)
    abs(angle - as.numeric(angle > 180) * 360)
}
