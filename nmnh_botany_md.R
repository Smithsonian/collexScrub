library(RODBC)
library(Taxonstand)
library(parallel)

dbfile <- file.choose()

ch <- odbcConnectAccess2007(dbfile)

tables <- sqlTables(ch, tableType = "TABLE")

#dataTable <- select.list(tables$TABLE_NAME)
dataTable <- tables$TABLE_NAME[1]


no_rows <- sqlQuery(ch, paste("SELECT count(*) FROM ",dataTable))



#? represent when not sure if the string belongs to the field
collectors_all <- sqlQuery(ch, paste("SELECT distinct collector_1 FROM ",dataTable))

collectors_noirn <- sqlQuery(ch, paste("SELECT distinct collector_1 FROM ",dataTable, " where irn_1 IS NULL"), stringsAsFactors = FALSE)

#for (i in 1:dim(collectors_noirn)[1]){
for (i in 1:100){
  name_to_check <- gsub("[?!*]", "", as.character(collectors_noirn[i,1]))
  results_HUH <- findCollectorsBotany(name_to_check)
  print(results_HUH)
}

#rbhl
bhl_res <- findCollectorsBHL(name_to_check, bhl_key = bhl_api_key)









#species
# species_all <- sqlQuery(ch, paste("SELECT distinct collector_1 FROM ",dataTable))
# #parallel
# no_cores <- detectCores() - 1
# # Initiate cluster
# cl <- makeCluster(no_cores)
# 
# res <- parLapply(cl, TPL, species = gsub("[?!*]", "", as.character(collectors_noirn[, 1])))
# TPL("Liriodendron")
# 
# stopCluster(cl)

























#check coords
#library(googleway)
#library(sqldf)

##DMS
coords <- sqlQuery(ch, paste("SELECT id, sheet_barcode, country, dms_lat_degrees, dms_lat_minutes, dms_lat_seconds, dms_lat_ns, dms_long_degrees, dms_long_minutes, dms_long_seconds, dms_long_ew FROM ",dataTable, "WHERE coord_unit = 'DMS'"), stringsAsFactors = FALSE)

print(paste("  There are", dim(coords)[1], "location records"))

dat <- data.frame(matrix(c(coords$id, coords$sheet_barcode, coords$country, coords$dms_lat_degrees, coords$dms_lat_minutes, coords$dms_lat_seconds, coords$dms_lat_ns, coords$dms_long_degrees, coords$dms_long_minutes, coords$dms_long_seconds, coords$dms_long_ew), 
              byrow = TRUE, nrow = 11), stringsAsFactors = FALSE)

names(dat) <- c("id", "sheet_barcode", "country", "dms_lat_degrees", "dms_lat_minutes", "dms_lat_seconds", "dms_lat_ns", "dms_long_degrees", "dms_long_minutes", "dms_long_seconds", "dms_long_ew")

# 
# coord_check <- data.frame(matrix(data = NA, ncol = 5, nrow = 0))
# 
# #for (i in 1:dim(dat)[2]){
# for (i in 1:500){
#   check_lat <- dms2dd(deg = dat[4, i], min = dat[5, i], sec = dat[6, i], direction = dat[7, i])
#   check_lon <- dms2dd(deg = dat[8, i], min = dat[9, i], sec = dat[10, i], direction = dat[11, i])
#   if (is.na(check_lat) || is.na(check_lon)){
#     coord_check <- rbind(coord_check, cbind(id = dat[1, i], dd_lat = NA, dd_lon = NA, country = NA, locality = NA), stringsAsFactors = FALSE)
#   }else{
#     #types https://developers.google.com/maps/documentation/geocoding/intro#Types
#     g_revgeo <- google_reverse_geocode(location = c(check_lat, check_lon), result_type = c("country", "locality", "administrative_area_level_3", "administrative_area_level_2", "administrative_area_level_1"), key = google_maps_api)
# 
#     if (g_revgeo$status == "OK"){
# 
#       g_geo_detail <- NA
# 
#       for (j in 1:length(g_revgeo$results$types)){
#         if (g_revgeo$results$types[[j]][1] == "country"){
#           g_geo_country <- g_revgeo$results$formatted_address[j]
#         }else if (g_revgeo$results$types[[j]][1] == "administrative_area_level_1"){
#           g_geo_detail <- g_revgeo$results$formatted_address[j]
#         }else if (g_revgeo$results$types[[j]][1] == "administrative_area_level_2"){
#           g_geo_detail <- g_revgeo$results$formatted_address[j]
#         }else if (g_revgeo$results$types[[j]][1] == "administrative_area_level_3"){
#           g_geo_detail <- g_revgeo$results$formatted_address[j]
#         }else if (g_revgeo$results$types[[j]][1] == "locality"){
#           g_geo_detail <- g_revgeo$results$formatted_address[j]
#         }
#       }
# 
#     }else{
#       g_geo_country <- NA
#       g_geo_detail <- NA
#     }
# 
#     coord_check <- rbind(coord_check, cbind(id = dat[1, i], dd_lat = check_lat, dd_lon = check_lon, country = g_geo_country, locality = g_geo_detail), stringsAsFactors = FALSE)
#   }
# }





















#other approach
library(rgdal)
library(sp)

coord_check <- data.frame(matrix(data = NA, ncol = 3, nrow = 0))

for (i in 1:dim(dat)[2]){
  check_lat <- dms2dd(deg = dat[4, i], min = dat[5, i], sec = dat[6, i], direction = dat[7, i])
  check_lon <- dms2dd(deg = dat[8, i], min = dat[9, i], sec = dat[10, i], direction = dat[11, i])
  if (!(is.na(check_lat) || is.na(check_lon))){
    coord_check <- rbind(coord_check, cbind(id = dat[1, i], dd_lat = check_lat, dd_lon = check_lon), stringsAsFactors = FALSE)
  }
}

gadm_0 <- readOGR(paste(gadm_location, "/level0/gadm_0.shp", sep= ""), stringsAsFactors = FALSE)

coord_check$dd_lon <- as.numeric(coord_check$dd_lon)
coord_check$dd_lat <- as.numeric(coord_check$dd_lat)

coordinates(coord_check) <- ~ dd_lon + dd_lat

pts <- SpatialPoints(coord_check, CRS("+init=epsg:4326"))

proj4string(gadm_0) <- CRS("+init=epsg:4326")

countries_check <- data.frame(matrix(data = NA, ncol = 1, nrow = 0))

for(i in 0:(floor(dim(coord_check)[2] / 500))){
  ii <- i * 500
  ii2 <- ii + 500
  if(ii2>dim(dat)[2]){
    ii2 = dim(dat)[2]
  }
  countries_check <- rbind(countries_check, over(pts[ii:ii2,], gadm_0))
}




odbcCloseAll()
