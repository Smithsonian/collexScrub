#' @export
#' @importFrom stringdist stringdist
#' @importFrom dplyr filter

find_match_str <- function(str_to_check, database, method = "osa", threshold = 10, no_cores = 2, year_limits = NA, database_strings = NA, str_to_check_col = NA){

  #check if str_to_check is a df with more than one columns
  if (dim(str_to_check)[2]>1 && is.na(str_to_check_col)){
    stop("str_to_check_col can't be NA when the str_to_check has more than 1 columns.")
  }
  
  #check if database is a df with more than one columns
  if (dim(database)[2]>1 && is.na(database_strings)){
    stop("database_strings can't be NA when the database provided has more than 1 columns.")
  }
  
  if (str_to_check == "" || str_to_check == "-" || str_to_check == "NA" || is.na(str_to_check)){
    return(data.frame(str_to_check = str_to_check, match = NA, score = NA))
  }

  results <- data.frame(matrix(data = NA, ncol = 3, nrow = 0))
  
  this_str <- gsub("[?!*]", "", as.character(str_to_check))
  
  if (!is.na(year_limits)){
    database <- dplyr::filter(database, year = str_to_check$year)
  }
  
  str_matches <- as.data.frame(stringdist::stringdist(this_str, database, nthread = no_cores, method = method))
  
  this_data <- cbind(database[which(str_matches < threshold)], str_matches[which(str_matches < threshold),1])
  
  results <- rbind(results, cbind(str_to_check = str_to_check, match = this_data[,1], score = this_data[,2]), stringsAsFactors = FALSE)
  
  return(results)
}
