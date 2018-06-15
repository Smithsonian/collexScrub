#' Query a pair of lat/lon values against a spatial database
#' 
#' @return A dataframe of matched geometries.
#'
#' @importFrom sp coordinates
#' @importFrom rgdal readOGR
#' @importFrom sp over
#' @importFrom sp CRS
#' @importFrom sp proj4string
#' @export
#' 
query_coords <- function(coords = NA, database = "GADM", db_shp = "data", geom = FALSE){
  
  if (any(database %in% c("GADM", "WDPA")) == FALSE){
    stop("Unknown database")
  }
  
  if (class(coords) != "data.frame"){
    stop("coords has to be a data frame")
  }
  
  sp::coordinates(coords) <- ~ longitude + latitude
  
  pts <- sp::SpatialPoints(coords, sp::CRS("+init=epsg:4326"))

  #results data.frame  
  results <- list()
  
  if (database == "GADM"){
    
    #iterate over levels
    for (level in seq(0, 5)){
      suppressMessages(gadm <- rgdal::readOGR(paste0(db_shp, "/gadm/gadm_", level, ".shp"), encoding = "UTF-8", use_iconv = TRUE, stringsAsFactors = FALSE))
      
      #Hack so that CRS match with points
      suppressWarnings(sp::proj4string(gadm) <- sp::CRS("+init=epsg:4326"))
      
      #Do spatial query
      location_name <- sp::over(pts, gadm)
      
      if (level == 0){
        location_results <- location_name$NAME_0
      }else if (level == 1){
        location_results <- location_name$NAME_1
      }else if (level == 2){
        location_results <- location_name$NAME_2
      }else if (level == 3){
        location_results <- location_name$NAME_3
      }else if (level == 4){
        location_results <- location_name$NAME_4
      }else if (level == 5){
        location_results <- location_name$NAME_5
      }
      
      
      if (sum(!is.na(location_results)) == 0){
        return(results)
      }else{
        for (j in 1:length(coords$id)){
          results[[j]] <- cbind(id = coords$id[j], layer = paste0("GADM_level", level), value = location_results[j])
        }
      }
      
      
    }
  }
}
