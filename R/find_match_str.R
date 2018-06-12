#' @export
#' @importFrom stringdist stringdist
#' @importFrom dplyr filter
#' @importFrom dplyr select

find_match_str <- function(str_to_check, database, method = "osa", threshold = 10, no_cores = 2, year_limits = FALSE, database_strings = NA, str_to_check_col = NA){
  
  #check if str_to_check is a df with more than one columns
  if (dim(str_to_check)[2]>1 && is.na(str_to_check_col)){
    stop("str_to_check_col can't be NA when the str_to_check has more than 1 columns.")
  }
  
  #check if database is a df with more than one columns
  if (dim(database)[2]>1 && is.na(database_strings)){
    stop("database_strings can't be NA when the database provided has more than 1 columns.")
  }

  if (!is.na(str_to_check_col)){
    this_str <- dplyr::select(str_to_check, str_to_check_col)[1]
  }else{
    this_str <- str_to_check[1]
  }
  
  
    
  if (this_str == "" || this_str == "-" || this_str == "NA" || is.na(this_str)){
    ret_data <- as.data.frame(cbind(this_str, NA, NA))
    names(ret_data) <- c("str_to_check", "match", "score")
    return(ret_data)
  }

  #results <- data.frame(matrix(data = NA, ncol = 3, nrow = 0))
  
  this_str <- gsub("[?!*]", "", as.character(this_str))
  
  if (year_limits && !is.na(str_to_check$year)){
    database <- dplyr::filter(database, year == str_to_check$year)
  }
  
  if (dim(database)[[1]]==0){
    ret_data <- as.data.frame(cbind(this_str, NA, NA))
    names(ret_data) <- c("str_to_check", "match", "score")
    return(ret_data)
  }
  
  if (!is.na(database_strings)){
    database <- dplyr::select(database, database_strings)
  }
  
  str_matches <- as.data.frame(stringdist::stringdist(this_str, database[,1], nthread = no_cores, method = method))
  
  this_data <- cbind(database[which(str_matches < threshold),], str_matches[which(str_matches < threshold),1])
  
  results <- as.data.frame(cbind(this_str, this_data[,1], this_data[,2]))
  
  if (dim(results)[1] == 1){
    ret_data <- as.data.frame(cbind(this_str, NA, NA))
    names(ret_data) <- c("str_to_check", "match", "score")
    return(ret_data)
  }else{
    names(results) <- c("str_to_check", "match", "score")
    return(results)
  }
}
