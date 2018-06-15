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