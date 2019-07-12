round2 = function(x, n) {
  #round function from https://stackoverflow.com/a/12688836
  posneg = sign(x)
  z = abs(x)*10^n
  z = z + 0.5
  z = trunc(z)
  z = z/10^n
  z*posneg
}


#' @importFrom sp SpatialPoints
#' @importFrom sp coordinates
#' @importFrom sp over
#' @importFrom sp CRS
check_dd_dir <- function(sheet_barcode, data, gadm){
  
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
  sp::coordinates(this_row) <- ~ dd_lon_ccr + dd_lat_ccr
  pts <- sp::SpatialPoints(this_row, sp::CRS("+init=epsg:4326"))
  location_name <- sp::over(pts, gadm)
  
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
  sp::coordinates(this_row) <- ~ dd_lon_ccr + dd_lat_ccr
  pts <- sp::SpatialPoints(this_row, sp::CRS("+init=epsg:4326"))
  location_name <- sp::over(pts, gadm)
  
  if (is.na(location_name$NAME_0)){
    return(c(sheet_barcode, NA))
  }else if (location_name$NAME_0 == this_row$country){
    #countries_match_no$reason[i] = "Latitude direction inverted"
    return(c(sheet_barcode, "Latitude direction inverted"))
  }
  
  return(c(sheet_barcode, NA))
}



check.integer <- function(x) {
  #from https://stackoverflow.com/q/3476782
  x <- suppressWarnings(as.numeric(x))
  
  if (is.na(x)){
    return(FALSE)
  }else if (!(is.numeric(x))){
    return(FALSE)
  }else{
    x == round(x)
  }
}