#' Convert lat/long from DMS to DD
#' 
#' Function that converts a messy DMS to a DD format.
#'
#' @param deg Value of the degrees, \code{integer}
#' @param min Value of the minutes, \code{integer}
#' @param sec Value of the seconds, \code{float}
#' @param direction Which direction, \code{N, S, E, or W}
#' @param roundto How many decimal places to round to, default 5
#' 
#'
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
    #Only seconds can be decimal
    if (!(check.integer(deg) && check.integer(min) && is.numeric(sec))){
      return(NA)
    }else{
      
      dd = abs(as.numeric(deg)) + abs(as.numeric(min) / 60) + abs(as.numeric(sec) / 3600)
      
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
