library(rgdal)
library(rgeos)

gadm0 <- readOGR(paste(gadm_location, "level0/gadm_0.shp", sep = ""), verbose = TRUE)

crswgs84=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

pt<-SpatialPoints(list(21.43, -78.897), proj4string=crswgs84)


pt <- SpatialPoints(readWKT("POINT(-78.897 21.43)"), proj4string=crswgs84)



res <- data.frame(matrix(ncol = 3, nrow = 0, data = NA))
names(res) <- c("i", "country", "contains")

for (i in 1:dim(gadm0)[1]){
  for (poly in 1:dim(gadm0[i,])[2]){
    res <- rbind(res, data.frame(i = i, country = as.character(gadm0[i,]$NAME_0), contains = gContains(gadm0[i,poly]), pt))
  }
}



Results<-gIntersects(gadm0, pt)





