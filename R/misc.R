#' @export

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

#' @export
dms2dd <- function(deg = 0, min = 0, sec = 0, direction = "N", roundto = 5){
  if (is.na(deg)){
    deg = 0
  }
  if (is.na(min)){
    min = 0
  }
  if (is.na(sec)){
    sec = 0
  }
  
  if (!(tolower(direction) %in% c("n", "e", "s", "w"))){
    return(NA)
  }else{
    #Protocol specifies seconds can be decimal
    if (!(check.integer(deg) && check.integer(min) && is.numeric(sec))){
      return(NA)
    }else{
      
      dd = as.numeric(deg) + ((as.numeric(min) / 60) + (as.numeric(sec) / 3600))
      
      #check if valid coords
      if (tolower(direction) %in% c("n", "s")){
        if (dd > 90 || dd < 0){
          return(NA)
        }
      }else{
        if (dd > 180 || dd < 0){
          return(NA)
        }
      }
      
      #set negative dd for south or west
      if (tolower(direction) %in% c("s", "w")){
        dd = dd * -1
      }
      
      
      
      return(round2(dd, roundto))
      
    }    
  }
}

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





#' @export
check_taxa_paleo <- function(name_check){
  library(worrms)
  library(rgbif)

  if (name_check == ""){
    return(verbatim_name = name_check, worms_ccr = NA, gbif_ccr = NA)
  }
  
  w <- try(worrms::wm_records_taxamatch(name = name_check), silent = TRUE)
  if (class(w)=="try-error"){
    w_sp <- NA
  }else{
    w_sp <- w[[1]]$scientificname
  }
  
  # g <- try(rgbif::name_lookup(query = name_check), silent = TRUE)
  # if (class(g)=="try-error"){
  #   g_sp <- NA
  # }else{
  #   #g_sp <- unlist(unique(g$data$scientificName[1]))
  #   g_sp <- g$data$scientificName[1]
  # }
  
  #return(list(verbatim_name = name_check, worms_ccr = w_sp, gbif_ccr = g_sp))
  return(c(name_check, w_sp))
}