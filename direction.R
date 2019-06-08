routej <- jsonlite::read_json("route.json")
route <- readOGR("route.json")
route <- spTransform(route, "+init=epsg:3006 +proj=utm +zone=33 +ellps=GRS80
  +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
pts <- as.data.frame(coordinates(route))
pts$elevation <- unlist(lapply(routej$features[[1]]$geometry[[2]], "[[", 3))
pts$X_geo <- unlist(lapply(routej$features[[1]]$geometry[[2]], "[[", 2))
pts$Y_geo <- unlist(lapply(routej$features[[1]]$geometry[[2]], "[[", 1))
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





weather(pts$X_geo[1], pts$Y_geo[1])


plot(cumsum(pts$distance), pts$slope, type = "l")
pts$distance)
head(pts)
## Q1
bearing(0,0,0,1) ## straight up (0)
bearing(0,0,1,2) ## 26.5
bearing(0,0,1,1) ## 45
bearing(0,0,1,0.5) ## 63.4
bearing(0,0,1,0) ## 90
## Q2
bearing(0,0,1,-0.5) ## 116.5
bearing(0,0,1,-1) ## 135
bearing(0,0,1,-2) ## 153.4
bearing(0,0,0,-1) ## 180
## Q3
bearing(0,0,-0.5,-1) ## 206.5
bearing(0,0,-1,-1) ## 225
bearing(0,0,-2,-1) ## 243.4
bearing(0,0,-1,0) ## 270
## Q4
bearing(0,0,-1, 0.5) ## 296.5
bearing(0,0,-1, 1) ## 315
bearing(0,0,-1, 2) ## 333.4
bearing(0,0,0,1) ## 0
