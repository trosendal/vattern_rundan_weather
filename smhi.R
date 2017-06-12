library(RCurl)
library(jsonlite)
library(sp)
library(svamap)

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

times <- function(t0 = as.POSIXct("2017-06-16 19:38:00 CEST", tz = "Europe/Stockholm"), pace = 36) {
    df <- read.csv("coordinates.csv", stringsAsFactors = FALSE)

    df$hours <- df$distance/pace
    df$time = t0 + (df$hours*60*60)
    return(df)
}

vattern_pred <- function(start = as.POSIXct("2017-06-16 19:38:00 CEST", tz = "Europe/Stockholm"),
                         pace = 36) {
    df <- times(start, pace)
    df3 <- do.call("rbind", lapply(seq_len(nrow(df)), function(x){
        df2 <- weather(df[x,]$lat, df[x,]$long)
        df2 <- df2[order(abs(df2$time - df[x,]$time)) == 1,]
        names(df2)[names(df2) == "time"] <- "weather_time"
        return(df2)
    }))
    cbind(df, df3)
}

df <- vattern_pred()

map_format <- function(df){
    df$popup <- paste0("Du är i ", df$Place,
                       "<br>Klocken: ", df$time,
                       "<br>Det er ", df$temp, "C",
                       "<br>Det blåser ", df$ws, "(", df$gust, ") ", "från ", df$wd,
                       "<br>Nederbörd från ", df$pmin, " till ", df$pmax, " mm/t")
    pts <- SpatialPointsDataFrame(SpatialPoints(cbind(df$long, df$lat)), df)
    return(pts)
}
pts <- map_format(df)

path_to_data <- svamap::write_data(pts)

svamap::write_page(data = path_to_data,
                   owntemplate = "map.html",
                   overwrite = TRUE,
                   browse = TRUE)
