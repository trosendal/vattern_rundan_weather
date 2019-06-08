source("functions.R")
##
## Get the race_times/weather info and format it:
df <- vattern_pred(start = as.POSIXct("2019-06-14 19:50:00 CEST", tz = "Europe/Stockholm"),
                   pace = 38.5)
pts <- map_format(df)
## Write the points to json
path_to_data <- svamap::write_data(pts)
## Plot them on a map:
svamap::write_page(data = path_to_data,
                   path = "~/Desktop/vattern/730",
                   owntemplate = "map.html",
                   overwrite = TRUE,
                   browse = FALSE,)
file.copy("vattern_poi.js", "~/Desktop/vattern/730/map/", overwrite = TRUE)
file.copy("vattern_track.js", "~/Desktop/vattern/730/map/", overwrite = TRUE)
temp <- readLines("~/.epi-cloudftp_credentials")
cred <- paste0("ftp://", temp[2], ":", temp[3], "@", temp[1], "/vattern/730/")
files <- list.files("~/Desktop/vattern/730/map/", full.names = TRUE)
ftpUpload(files[1], paste0(cred, basename(files[1])))
## for(i in files) {
##     ftpUpload(i, paste0(cred, basename(i)))
## }
