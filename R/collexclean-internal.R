

dms2dd <- function(deg = 0, min = 0, sec = 0, direction = "N"){
  dd = deg + ((min / 60) + (sec / 3600))
  if (tolower(direction) %in% c("s", "w")){
    dd = dd * -1
  }
  return(dd)
  }
