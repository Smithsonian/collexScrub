

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


round2 = function(x, n) {
  #round function from https://stackoverflow.com/a/12688836
  posneg = sign(x)
  z = abs(x)*10^n
  z = z + 0.5
  z = trunc(z)
  z = z/10^n
  z*posneg
}