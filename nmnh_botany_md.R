#############################
#Load required packages
#############################
library(RODBC)
library(rgdal)
library(sp)
library(dplyr)
library(parallel)
library(stringdist)
#library(Taxonstand)





#############################
#Check arguments
#############################
args = commandArgs(trailingOnly=TRUE)

#only for batch
if(!interactive()){
  if (length(args) != 2) {
    stop("Arguments are missing, should be: 1) a settings file; and 2) an accdb file", call.=FALSE)
  }else{
    #settings file
    settings_file <- args[1]
    source(settings_file)
  }
}else{
  #interactive, assume settings.R file in the working dir
  source("settings.R")
}
#############################


#interactive or batch?
if(interactive()){
  #Select db file
  dbfile <- file.choose()
  
  if(!file.exists(dbfile)){
    stop(paste("Could not open the file ", dbfile))
  }
  
  ch <- try(odbcConnectAccess2007(dbfile), silent = TRUE)
  if(ch==-1){
    stop("There was an error reading the file. Check if R and Office match in using the 32-bit (most common) or 64-bit version.")
  }
  
  tables <- sqlTables(ch, tableType = "TABLE")
  
  #Select a table
  dataTable <- select.list(choices = tables$TABLE_NAME, title = "Select a table", multiple = FALSE, graphics = TRUE)
}else{
  #input data file
  dbfile <- paste(botany_files_loc, "/", args[2], sep = "")
  
  if(!file.exists(dbfile)){
    stop(paste("Could not read the file ", dbfile))
  }
  
  ch <- try(odbcConnectAccess2007(dbfile), silent = TRUE)
  if(ch==-1){
    stop("There was an error reading the file. Check if R and Office match in using the 32-bit (most common) or 64-bit version.")
  }
  
  tables <- sqlTables(ch, tableType = "TABLE")
  
  #Assumes one table in file to process
  dataTable <- tables$TABLE_NAME[1]
}

no_rows <- sqlQuery(ch, paste("SELECT count(*) FROM ",dataTable))
cat(paste("There are", prettyNum(no_rows[[1]], big.mark = ","), "rows in this database."))
#############################























#############################
#PRECISE_LOCATION
#############################
prec_loc_known <- sqlQuery(ch, paste("SELECT country, state_province, precise_locality FROM ",dataTable, " WHERE precise_locality not like '%[*]%' group by country, state_province, precise_locality"), stringsAsFactors = FALSE)

print(paste("There are", prettyNum(dim(prec_loc_known)[1], big.mark = ","), "known locations"))

prec_loc_unknown <- sqlQuery(ch, paste("SELECT id, sheet_barcode, country, state_province, precise_locality FROM ",dataTable, " WHERE precise_locality like '%[*]%'"), stringsAsFactors = FALSE)

print(paste("There are", prettyNum(dim(prec_loc_unknown)[1], big.mark = ","), "unknown locations"))
#


##############
#Parallel
##############
# Number of cores, leaving 1 out
per_proc_cores <- 2
no_cores <- floor((detectCores() - 1)/per_proc_cores)

#set cluster
debug_file <- paste("logs/", format(Sys.time(), "%Y%m%d_%H%M%S"), ".txt", sep = "")
cl <- makeCluster(no_cores, outfile = debug_file, type = "PSOCK")

#execute function on cluster
res <- parLapply(cl, prec_loc_unknown$precise_locality, find_match_str, database = prec_loc_known$precise_locality, threshold = 10, no_cores = per_proc_cores)

#stop cluster
stopCluster(cl)
#########





location <- prec_loc_unknown[i,]$precise_locality

a <- find_match_str(str_to_check = location, database = prec_loc_known$precise_locality, threshold = 5, no_cores = 8)


sink('b.csv', append = FALSE)
for(i in 1:dim(prec_loc_unknown)[1]){
  this_location <- prec_loc_unknown[i,]$precise_locality
  if(this_location == "-" || this_location == "NA"){
    next
  }
  this_location <- gsub("[?!*]", "", as.character(this_location))
  
  location_matches <- as.data.frame(stringdist(this_location, prec_loc_known$precise_locality))
  
  #cat(paste(this_collector, "|", min(dist.name), "\n", sep = ""))
  dist_threshold <- 25
  loc_matches <- cbind(prec_loc_known[which(location_matches < dist_threshold),], location_matches[which(location_matches < dist_threshold),1])
  
  for(j in 1:dim(loc_matches)[1]){
    cat(paste(this_location, "|", loc_matches[j,1], "\n", sep = ""))
  }
}
sink()
#

dist.name <- stringdist(collectors_known, collectors_noirn)


#for (i in 1:dim(collectors_noirn)[1]){
for (i in 1:100){
  name_to_check <- gsub("[?!*]", "", as.character(collectors_noirn[i,1]))
  results_HUH <- findCollectorsBotany(name_to_check)
  print(results_HUH)
}

#rbhl
bhl_res <- findCollectorsBHL(name_to_check, bhl_key = bhl_api_key)




























#############################
#CHECK COORDS
#############################

##DMS
coords <- sqlQuery(ch, paste("SELECT id, sheet_barcode, country, dms_lat_degrees, dms_lat_minutes, dms_lat_seconds, dms_lat_ns, dms_long_degrees, dms_long_minutes, dms_long_seconds, dms_long_ew FROM ",dataTable, "WHERE coord_unit = 'DMS'"), stringsAsFactors = FALSE)

print(paste("There are", prettyNum(dim(coords)[1], big.mark = ","), "DMS location records"))

dat <- data.frame(cbind(coords$id, coords$sheet_barcode, coords$country, coords$dms_lat_degrees, coords$dms_lat_minutes, coords$dms_lat_seconds, coords$dms_lat_ns, coords$dms_long_degrees, coords$dms_long_minutes, coords$dms_long_seconds, coords$dms_long_ew, dd_lat_ccr = NA, dd_lon_ccr = NA), stringsAsFactors = FALSE)

names(dat) <- list("id", "sheet_barcode", "country", "dms_lat_degrees", "dms_lat_minutes", "dms_lat_seconds", "dms_lat_ns", "dms_long_degrees", "dms_long_minutes", "dms_long_seconds", "dms_long_ew", "dd_lat_ccr", "dd_lon_ccr")



#coord_check <- data.frame(matrix(data = NA, ncol = 3, nrow = 0))

for (i in 1:dim(dat)[1]){
  check_lat <- dms2dd(deg = dat$dms_lat_degrees[i], min = dat$dms_lat_minutes[i], sec = dat$dms_lat_seconds[i], direction = dat$dms_lat_ns[i])
  check_lon <- dms2dd(deg = dat$dms_long_degrees[i], min = dat$dms_long_minutes[i], sec = dat$dms_long_seconds[i], direction = dat$dms_long_ew[i])
  if (!(is.na(check_lat) || is.na(check_lon))){
    #coord_check <- rbind(coord_check, cbind(id = dat$id[i], dd_lat = check_lat, dd_lon = check_lon), stringsAsFactors = FALSE)
    #id = dat$id[i]
    dat$dd_lat_ccr[i] = as.numeric(check_lat)
    dat$dd_lon_ccr[i] = as.numeric(check_lon)
  }
}


dat_check <- dat[!is.na(dat$dd_lat_ccr),]

dat_check$dd_lon_ccr <- as.numeric(dat_check$dd_lon_ccr)
dat_check$dd_lat_ccr <- as.numeric(dat_check$dd_lat_ccr)
dat_check$id <- as.numeric(dat_check$id)

coordinates(dat_check) <- ~ dd_lon_ccr + dd_lat_ccr

#pts <- SpatialPoints(dat_check, CRS("+init=epsg:4326"))

#load GADM
gadm_0 <- readOGR(paste(gadm_location, "/level0/gadm_0.shp", sep= ""), encoding = "UTF-8", use_iconv = TRUE, stringsAsFactors = FALSE)
#gadm_1 <- readOGR(paste(gadm_location, "/level1/gadm_1.shp", sep= ""), encoding = "UTF-8", use_iconv = TRUE, stringsAsFactors = FALSE)

#hack so that CRS match with points
suppressWarnings(proj4string(gadm_0) <- CRS("+init=epsg:4326"))
#suppressWarnings(proj4string(gadm_1) <- CRS("+init=epsg:4326"))



countries_check <- data.frame(matrix(data = NA, ncol = 3, nrow = 0))

#check points, 500 at a time
loop_size <- 200
#for(i in 0:(floor(dim(dat_check)[1] / loop_size))){
for(i in 0:(5000/loop_size)){
  ii <- i * loop_size + 1
  ii2 <- ii + loop_size
  if (ii2 > dim(dat_check)[1]){
    ii2 <- dim(dat_check)[1]
  }
  pts <- SpatialPoints(dat_check[ii:ii2,], CRS("+init=epsg:4326"))
  location_name <- over(pts, gadm_0)
  #countries_check <- rbind(countries_check, cbind(sheet_barcode = dat_check[ii:ii2,]$sheet_barcode, state_prov_ccr = location_name$NAME_1, country_ccr = location_name$NAME_0))
  countries_check <- rbind(countries_check, cbind(sheet_barcode = dat_check[ii:ii2,]$sheet_barcode, state_prov_ccr = NA, country_ccr = location_name$NAME_0))
}

#quick hack to select uniques
#countries_check <- distinct(countries_check, id, .keep_all = TRUE)


#How many didn't find location?
#dim(countries_check[is.na(countries_check$state_prov_ccr),])[1]
#Percent?
#(dim(countries_check[is.na(countries_check$state_prov_ccr),])[1]/dim(countries_check)[1]) * 100


#join
dat$sheet_barcode <- as.character(dat$sheet_barcode)
countries_check$sheet_barcode <- as.character(countries_check$sheet_barcode)

dat_countries <- dplyr::left_join(dat[!is.na(dat$dd_lat_ccr),], countries_check, by = "sheet_barcode")


#country mismatch?
#not match
countries_match_no <- dplyr::filter(dat_countries, country!=country_ccr)
#countries_match_no <- cbind(countries_match_no, reason = NA)
cat(paste(round((dim(countries_match_no)[1]/dim(dat_countries)[1])*100, 2), "% rows didn't match the country.\n\n Checking for inverted direction."))



##############
#Parallel
##############
# Number of cores, leaving 1 out
no_cores <- detectCores() - 1

#set cluster
debug_file <- paste("logs/", format(Sys.time(), "%Y%m%d_%H%M%S"), ".txt", sep = "")
cl <- makeCluster(no_cores, outfile = debug_file, type = "PSOCK")

#execute function on cluster
res <- parLapply(cl, countries_match_no$sheet_barcode, check_dd_dir, data = dat, gadm = gadm_0)
#28 mins in 3 cores

#stop cluster
stopCluster(cl)

res.df <- do.call(rbind.data.frame, res)

names(res.df) <- c("sheet_barcode", "reason")

res.df$sheet_barcode <- as.character(res.df$sheet_barcode)
res.df$reason <- as.character(res.df$reason)

countries_match_no_res <- dplyr::left_join(countries_match_no, res.df, by = "sheet_barcode")

write.csv(countries_match_no_res, file = "countries_match_no.csv", quote = TRUE, row.names = FALSE, fileEncoding = "UTF-8")

res_unknown <- dplyr::filter(countries_match_no_res, reason == "Country was Unknown")
res_lat <- dplyr::filter(countries_match_no_res, reason == "Latitude direction inverted")
res_lon <- dplyr::filter(countries_match_no_res, reason == "Longitude direction inverted")

cat(paste("Unknown country: ", dim(res_unknown)[1], " (", round((dim(res_unknown)[1] / dim(countries_match_no_res)[1]) * 100, 2), "%)", sep = ""))
cat(paste("Lat inverted: ", dim(res_lat)[1], " (", round((dim(res_lat)[1] / dim(countries_match_no_res)[1]) * 100, 2), "%)", sep = ""))
cat(paste("Lon inverted: ", dim(res_lon)[1], " (", round((dim(res_lon)[1] / dim(countries_match_no_res)[1]) * 100, 2), "%)", sep = ""))


#match
countries_match_yes <- filter(dat_countries, country==country_ccr)
cat(paste("Rows with countries matched: ", prettyNum(dim(countries_match_yes)[1], big.mark = ","), " (", round((dim(countries_match_yes)[1]/dim(dat_countries)[1])*100, 2), "%)", sep = ""))
#############################





































##DD
# coords <- sqlQuery(ch, paste("SELECT id, sheet_barcode, country, dd_lat, dd_long FROM ",dataTable, "WHERE coord_unit = 'DD'"), stringsAsFactors = FALSE)
# 
# print(paste("There are", prettyNum(dim(coords)[1], big.mark = ","), "DD location records"))
# 
# dat <- data.frame(cbind(coords$id, coords$sheet_barcode, coords$country, coords$dd_lat, coords$dd_long, dd_lat_ccr = NA, dd_lon_ccr = NA), stringsAsFactors = FALSE)
# 
# names(dat) <- list("id", "sheet_barcode", "country", "dd_lat", "dd_long", "dd_lat_ccr", "dd_lon_ccr")
# 
# 
# #coord_check <- data.frame(matrix(data = NA, ncol = 3, nrow = 0))
# 
# for (i in 1:dim(dat)[1]){
#   check_lat <- dms2dd(deg = dat$dms_lat_degrees[i], min = dat$dms_lat_minutes[i], sec = dat$dms_lat_seconds[i], direction = dat$dms_lat_ns[i])
#   check_lon <- dms2dd(deg = dat$dms_long_degrees[i], min = dat$dms_long_minutes[i], sec = dat$dms_long_seconds[i], direction = dat$dms_long_ew[i])
#   if (!(is.na(check_lat) || is.na(check_lon))){
#     #coord_check <- rbind(coord_check, cbind(id = dat$id[i], dd_lat = check_lat, dd_lon = check_lon), stringsAsFactors = FALSE)
#     #id = dat$id[i]
#     dat$dd_lat_ccr[i] = as.numeric(check_lat)
#     dat$dd_lon_ccr[i] = as.numeric(check_lon)
#   }
# }
# 
# 
# dat_check <- dat[!is.na(dat$dd_lat_ccr),]
# 
# dat_check$dd_lon_ccr <- as.numeric(dat_check$dd_lon_ccr)
# dat_check$dd_lat_ccr <- as.numeric(dat_check$dd_lat_ccr)
# dat_check$id <- as.numeric(dat_check$id)
# 
# coordinates(dat_check) <- ~ dd_lon_ccr + dd_lat_ccr
# 
# #pts <- SpatialPoints(dat_check, CRS("+init=epsg:4326"))
# 
# #load GADM
# gadm_0 <- readOGR(paste(gadm_location, "/level0/gadm_0.shp", sep= ""), encoding = "UTF-8", use_iconv = TRUE, stringsAsFactors = FALSE)
# gadm_1 <- readOGR(paste(gadm_location, "/level1/gadm_1.shp", sep= ""), encoding = "UTF-8", use_iconv = TRUE, stringsAsFactors = FALSE)
# 
# #hack so that CRS match with points
# suppressWarnings(proj4string(gadm_0) <- CRS("+init=epsg:4326"))
# suppressWarnings(proj4string(gadm_1) <- CRS("+init=epsg:4326"))
# 
# 
# 
# countries_check <- data.frame(matrix(data = NA, ncol = 3, nrow = 0))
# 
# #check points, 500 at a time
# loop_size <- 500
# for(i in 0:(floor(dim(dat_check)[1] / loop_size))){
#   ii <- i * loop_size + 1
#   ii2 <- ii + loop_size
#   if (ii2 > dim(dat_check)[1]){
#     ii2 <- dim(dat_check)[1]
#   }
#   pts <- SpatialPoints(dat_check[ii:ii2,], CRS("+init=epsg:4326"))
#   location_name <- over(pts, gadm_1)
#   countries_check <- rbind(countries_check, cbind(sheet_barcode = dat_check[ii:ii2,]$sheet_barcode, state_prov_ccr = location_name$NAME_1, country_ccr = location_name$NAME_0))
# }
# 
# #quick hack to select uniques
# #countries_check <- distinct(countries_check, id, .keep_all = TRUE)
# 
# 
# #How many didn't find location?
# #dim(countries_check[is.na(countries_check$state_prov_ccr),])[1]
# #Percent?
# #(dim(countries_check[is.na(countries_check$state_prov_ccr),])[1]/dim(countries_check)[1]) * 100
# 
# 
# #join
# dat$sheet_barcode <- as.character(dat$sheet_barcode)
# countries_check$sheet_barcode <- as.character(countries_check$sheet_barcode)
# 
# dat_countries <- dplyr::left_join(dat[!is.na(dat$dd_lat_ccr),], countries_check, by = "sheet_barcode")
# 
# 
# #country mismatch?
# #not match
# countries_match_no <- dplyr::filter(dat_countries, country!=country_ccr)
# #countries_match_no <- cbind(countries_match_no, reason = NA)
# cat(paste(round((dim(countries_match_no)[1]/dim(dat_countries)[1])*100, 2), "% rows didn't match the country.\n\n Checking for inverted direction."))
# 
# 
# 
# ##############
# #Parallel
# ##############
# # Number of cores, leaving 1 out
# no_cores <- detectCores() - 1
# 
# #set cluster
# debug_file <- paste("logs/", format(Sys.time(), "%Y%m%d_%H%M%S"), ".txt", sep = "")
# cl <- makeCluster(no_cores, outfile = debug_file, type = "PSOCK")
# 
# #execute function on cluster
# res <- parLapply(cl, countries_match_no$sheet_barcode, check_dd_dir, data = dat, gadm = gadm_0)
# #28 mins in 3 cores
# 
# #stop cluster
# stopCluster(cl)
# 
# res.df <- do.call(rbind.data.frame, res)
# 
# names(res.df) <- c("sheet_barcode", "reason")
# 
# res.df$sheet_barcode <- as.character(res.df$sheet_barcode)
# res.df$reason <- as.character(res.df$reason)
# 
# countries_match_no_res <- dplyr::left_join(countries_match_no, res.df, by = "sheet_barcode")
# 
# write.csv(countries_match_no_res, file = "countries_match_no.csv", quote = TRUE, row.names = FALSE, fileEncoding = "UTF-8")
# 
# res_unknown <- dplyr::filter(countries_match_no_res, reason == "Country was Unknown")
# res_lat <- dplyr::filter(countries_match_no_res, reason == "Latitude direction inverted")
# res_lon <- dplyr::filter(countries_match_no_res, reason == "Longitude direction inverted")
# 
# cat(paste("Unknown country: ", dim(res_unknown)[1], " (", round((dim(res_unknown)[1] / dim(countries_match_no_res)[1]) * 100, 2), "%)", sep = ""))
# cat(paste("Lat inverted: ", dim(res_lat)[1], " (", round((dim(res_lat)[1] / dim(countries_match_no_res)[1]) * 100, 2), "%)", sep = ""))
# cat(paste("Lon inverted: ", dim(res_lon)[1], " (", round((dim(res_lon)[1] / dim(countries_match_no_res)[1]) * 100, 2), "%)", sep = ""))
# 
# 
# #match
# countries_match_yes <- filter(dat_countries, country==country_ccr)
# cat(paste("Rows with countries matched: ", prettyNum(dim(countries_match_yes)[1], big.mark = ","), " (", round((dim(countries_match_yes)[1]/dim(dat_countries)[1])*100, 2), "%)", sep = ""))
#############################























#############################
#Collectors
#############################
#? represent when not sure if the string belongs to the field
collectors_known <- sqlQuery(ch, paste("SELECT collector_1, irn_1 FROM ",dataTable, " where irn_1 IS NOT NULL GROUP BY collector_1, irn_1"), stringsAsFactors = FALSE)

collectors_noirn <- sqlQuery(ch, paste("SELECT distinct collector_1 FROM ",dataTable, " where irn_1 IS NULL AND collector_1 IS NOT NULL"), stringsAsFactors = FALSE)


sink('a.csv', append = FALSE)
for(i in 1:dim(collectors_noirn)[1]){
  this_collector <- collectors_noirn[i,1]
  if(this_collector == "-" || this_collector == "NA" || nchar(this_collector)<8){
    next
  }
  this_collector <- gsub("[?!*]", "", as.character(this_collector))
  dist.name <- as.data.frame(stringdist(this_collector, collectors_known$collector_1, nthread = 4))
  
  #cat(paste(this_collector, "|", min(dist.name), "\n", sep = ""))
  dist_threshold <- 4
  col_matches <- cbind(collectors_known[which(dist.name < dist_threshold),], dist.name[which(dist.name < dist_threshold),1])
  
  for(j in 1:dim(col_matches)[1]){
    cat(paste(this_collector, "|", col_matches[j,1], "|", col_matches[j,3], "|", col_matches[j,2], "\n", sep = ""))
  }
}
sink()



dist.name <- stringdist(collectors_known, collectors_noirn)


#for (i in 1:dim(collectors_noirn)[1]){
for (i in 1:100){
  name_to_check <- gsub("[?!*]", "", as.character(collectors_noirn[i,1]))
  results_HUH <- findCollectorsBotany(name_to_check)
  print(results_HUH)
}

#rbhl
bhl_res <- findCollectorsBHL(name_to_check, bhl_key = bhl_api_key)
































#check coords
#library(googleway)


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




























odbcCloseAll()
