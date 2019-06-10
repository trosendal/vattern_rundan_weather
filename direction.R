routej <- jsonlite::read_json("route.json")
routegeo <- readOGR("route.json")
route <- spTransform(routegeo, "+init=epsg:3006 +proj=utm +zone=33 +ellps=GRS80
  +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
pts <- as.data.frame(coordinates(route))
pts$elevation <- unlist(lapply(routej$features[[1]]$geometry[[2]], "[[", 3))
pts$lat <- unlist(lapply(routej$features[[1]]$geometry[[2]], "[[", 2))
pts$long <- unlist(lapply(routej$features[[1]]$geometry[[2]], "[[", 1))
pts$direction <- 0
pts$distance <- 0
pts$slope <- 0
for(i in 2:nrow(pts)) {
    a <- pts[i-1,]
    b <- pts[i,]
    pts$distance[i] <- mydist(a$X1, a$X2, b$X1, b$X2)
    pts$direction[i] <- bearing(a$X1, a$X2, b$X1, b$X2)
    pts$slope[i] <- myslope(a$X1, a$X2, a$elevation, b$X1, b$X2, b$elevation)
}
pts$cumdist <- cumsum(pts$dist)
routepts <- SpatialPointsDataFrame(coords = cbind(pts$long, pts$lat),
                                   proj4string = CRS(proj4string(routegeo)),
                                   data = pts)
