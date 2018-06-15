

#' @export
round2 = function(x, n) {
  #round function from https://stackoverflow.com/a/12688836
  posneg = sign(x)
  z = abs(x)*10^n
  z = z + 0.5
  z = trunc(z)
  z = z/10^n
  z*posneg
}


#' @export
check_dd_dir <- function(sheet_barcode, data, gadm){
  library(sp)
  this_row <- data[data$sheet_barcode == sheet_barcode,]
  
  #check for 'Unknown' country
  if (this_row$country == "Unknown"){
    return(c(sheet_barcode, "Country was Unknown"))
  }
  
  #check if lon is inverted
  this_row <- data[data$sheet_barcode == sheet_barcode,]
  this_row$dd_lon_ccr <- as.numeric(this_row$dd_lon_ccr)
  this_row$dd_lat_ccr <- as.numeric(this_row$dd_lat_ccr)
  
  this_row$dd_lon_ccr <- (this_row$dd_lon_ccr * -1)
  coordinates(this_row) <- ~ dd_lon_ccr + dd_lat_ccr
  pts <- SpatialPoints(this_row, CRS("+init=epsg:4326"))
  location_name <- over(pts, gadm)
  
  if (is.na(location_name$NAME_0)){
    return(c(sheet_barcode, NA))
  }else if (location_name$NAME_0 == this_row$country){
    #countries_match_no$reason[i] = "Longitude direction inverted"
    return(c(sheet_barcode, "Longitude direction inverted"))
  }
  
  #check if lat is inverted
  this_row <- data[data$sheet_barcode == sheet_barcode,]
  this_row$dd_lon_ccr <- as.numeric(this_row$dd_lon_ccr)
  this_row$dd_lat_ccr <- as.numeric(this_row$dd_lat_ccr)
  
  this_row$dd_lat_ccr <- (this_row$dd_lat_ccr * -1)
  coordinates(this_row) <- ~ dd_lon_ccr + dd_lat_ccr
  pts <- SpatialPoints(this_row, CRS("+init=epsg:4326"))
  location_name <- over(pts, gadm)
  
  if (is.na(location_name$NAME_0)){
    return(c(sheet_barcode, NA))
  }else if (location_name$NAME_0 == this_row$country){
    #countries_match_no$reason[i] = "Latitude direction inverted"
    return(c(sheet_barcode, "Latitude direction inverted"))
  }
  
  return(c(sheet_barcode, NA))
}


