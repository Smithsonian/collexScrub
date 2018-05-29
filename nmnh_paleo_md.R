#############################
#Load required packages
#############################
library(worrms)
library(rgbif)
library(dplyr)
library(parallel)
#library(paleobioDB)





#############################
#SETTINGS FILE
#############################

source("settings.R")

#############################





excelfile <- file.choose()

paleo_data <- read.csv(excelfile, header = TRUE, sep=",", stringsAsFactors = FALSE)

no_rows <- dim(paleo_data)[1]
cat(paste("There are", prettyNum(no_rows, big.mark = ","), "rows in this database."))

#unique names
taxa_names <- data.frame(unique(paleo_data$Taxonomy), stringsAsFactors = FALSE)
no_names <- dim(taxa_names)[1]
cat(paste("There are", prettyNum(no_names, big.mark = ","), "unique names."))

##############
#Parallel
##############
# Number of cores, leaving 1 out
no_cores <- detectCores() - 1

#set cluster
debug_file <- paste("logs/", format(Sys.time(), "%Y%m%d_%H%M%S"), ".txt", sep = "")
cl <- makeCluster(no_cores, outfile = debug_file, type = "PSOCK")

#execute function on cluster
res <- parLapply(cl, taxa_names[,1], check_taxa_paleo)
#- mins in 3 cores

#stop cluster
stopCluster(cl)

res.df <- do.call(rbind.data.frame, res)

names(res.df) <- c("verbatim_name", "worms_ccr", "gbif_ccr")

res.df$sheet_barcode <- as.character(res.df$sheet_barcode)
res.df$reason <- as.character(res.df$reason)















##location
library(googleway)

locs <- data.frame(unique(paleo_data$Location.Description), stringsAsFactors = FALSE)
no_locs <- dim(locs)[1]
cat(paste("There are", prettyNum(no_locs, big.mark = ","), "unique locations."))

geo_check <- data.frame(matrix(data = NA, ncol = 3, nrow = 0))

#for (i in 1:dim(dat)[2]){
for (i in 1:50){
  if(locs[i, 1] == ""){
    next
  }
  g_geo <- google_geocode(locs[i, 1], key = google_maps_api)

  if (g_geo$status == "OK"){
    g_geo_lat <- g_geo$results$geometry$location$lat
    g_geo_lon <- g_geo$results$geometry$location$lng
  }else{
    g_geo_lat <- NA
    g_geo_lon <- NA
  }
  geo_check <- rbind(geo_check, cbind(location = locs[i, 1], dd_lat = g_geo_lat, dd_lon = g_geo_lon), stringsAsFactors = FALSE)
}

###################





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
