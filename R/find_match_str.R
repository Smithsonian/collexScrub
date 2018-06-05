#' @export
find_match_str <- function(str_to_check, database, method = "osa", threshold = 10, no_cores = 2){
  library(stringdist)
  
  if (str_to_check == "" || str_to_check == "-" || str_to_check == "NA"){
    return(data.frame(str_to_check = str_to_check, match = NA, score = NA))
  }
  
  results <- data.frame(matrix(data = NA, ncol = 3, nrow = 0))
  
  this_str <- gsub("[?!*]", "", as.character(str_to_check))
  
  str_matches <- as.data.frame(stringdist(this_str, database, nthread = no_cores, method = method))
  
  this_data <- cbind(database[which(str_matches < threshold)], str_matches[which(str_matches < threshold),1])
  
  results <- rbind(results, cbind(str_to_check = str_to_check, match = this_data[,1], score = this_data[,2]), stringsAsFactors = FALSE)
  
  return(results)
}
