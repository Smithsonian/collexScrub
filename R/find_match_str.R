#' Find a string in a database of known strings
#' 
#'Function to check a string that has problems due to 
#' incomplete transcription, OCR problems, illegible 
#' words, and other problems. 
#'
#' @return A dataframe of \code{str_to_check} (the string provided),
#' \code{match} (the string matched from the database), \code{score} (the score for this string).
#'
#' @param str_to_check String to check against the database
#' @param database Database of known strings
#' @param method Which method to use to try to find a match, See \code{Details} below
#' @param no_cores How many cores to dedicate to this function execution
#' @param year_limits Boolean whether to use \code{year} to limit the strings available to match
#' @param country_limits Boolean whether to use \code{country} to limit the strings available to match
#' @param database_strings Which column to use for matching in \code{database}, only needed if it is more than one columns
#' @param str_to_check_col Which column to use for matching in \code{str_to_check}, only needed if it is more than one columns
#' 
#' @details The \code{method} arguments is passed to \code{stringdist::stringdist()}. To see the details of the methods available, see \code{?stringdist::`stringdist-metrics`}.
#'
#'
#'
#' @export
#' @importFrom stringdist stringdist
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' 
find_match_str <- function(str_to_check, database, method = "osa", no_cores = 2, year_limits = FALSE, country_limits = FALSE, database_strings = NA, str_to_check_col = NA){
  
  #check if str_to_check is a df with more than one columns
  if (dim(str_to_check)[2]>1 && is.na(str_to_check_col)){
    stop("str_to_check_col can't be NA when the str_to_check has more than 1 columns.")
  }
  
  #check if database is a df with more than one columns
  if (dim(database)[2]>1 && is.na(database_strings)){
    stop("database_strings can't be NA when the database provided has more than 1 columns.")
  }
cat(1)
  if (!is.na(str_to_check_col)){
    this_str <- dplyr::select(as.data.frame(str_to_check), as.character(str_to_check_col))[1]
  }else{
    this_str <- str_to_check[1]
  }
cat(2)  
  this_str_original <- as.character(this_str)
  
  #if the string to find is empty, return NA
  if (this_str == "" || this_str == "-" || this_str == "NA" || is.na(this_str)){
    cat("Empty string, returning NAs.")
    ret_data <- as.data.frame(cbind(this_str_original, NA, NA))
    names(ret_data) <- c("str_to_check", "match", "score")
    return(ret_data)
  }

  #remove these chars
  this_str <- gsub("[?!*]", "", as.character(this_str))
  
  #if limiting by year, use only locations with that year
  if (year_limits && !is.na(str_to_check$year)){
    database <- dplyr::filter(database, year == str_to_check$year)
  }
  
  #if limiting by country, use only locations with that country
  if (country_limits && !is.na(str_to_check$country)){
    database <- dplyr::filter(database, country == str_to_check$country)
  }
  
  if (dim(database)[[1]]==0){
    ret_data <- as.data.frame(cbind(this_str_original, NA, NA))
    names(ret_data) <- c("str_to_check", "match", "score")
    return(ret_data)
  }else{
    
    if (!is.na(database_strings)){
      database <- dplyr::select(database, database_strings)
    }
    
    if (method == "jw"){
      #Jaro distance
      str_matches <- as.data.frame(stringdist::stringdist(this_str, database[,1], nthread = no_cores, method = method, p = 0))
    }else{
      str_matches <- as.data.frame(stringdist::stringdist(this_str, database[,1], nthread = no_cores, method = method))
    }
    
    
    #Don't apply threshold since they depend on the method
    #this_data <- cbind(database[which(str_matches < threshold),], str_matches[which(str_matches < threshold),1])
    this_data <- cbind(database, str_matches)
    
    results <- as.data.frame(cbind(this_str_original, this_data[,1], this_data[,2]))
    
    if (dim(results)[1] == 1){
      ret_data <- as.data.frame(cbind(this_str_original, NA, NA))
      names(ret_data) <- c("str_to_check", "match", "score")
      return(ret_data)
    }else{
      names(results) <- c("str_to_check", "match", "score")
      return(results)
    }
  }
  
}
