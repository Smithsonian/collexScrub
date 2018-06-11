#'Find the matches of a name in both the PaleoDB and WoRMS databases

#' 
#' @param input_name A scientific name to check against PaleoDB and WoRMS
#' @param sep_char Separating character for strings of \code{input_name}, the function will search only the last value. Default is pipe (|)
#' @return A dataframe of species matched.
#
#' @export
#' @importFrom taxize gnr_resolve
#' 
find_sciname_paleo <- function(input_name, sep_char = "|"){
  
  input_name_original <- input_name
  
  if (nchar(input_name) == 0){
    return(cbind(input_name = input_name_original, paleodb_matched_name = NA, paleodb_score = NA, paleodb_classification = NA, paleodb_name_id = NA, worms_matched_name = NA, worms_score = NA, worms_classification = NA, worms_name_id = NA))
  }
  
  #names with pipes, use last
  input_name_spl <- strsplit(input_name, "[|]")[[1]]
  input_name <- input_name_spl[length(input_name_spl)]

  #remove names in parenthesis
  #from https://stackoverflow.com/a/24173271
  input_name <- gsub("\\s*\\([^\\)]+\\)","", as.character(input_name))

  #remove other characters
  input_name <- gsub("[^A-Za-z0-9 ]", "", input_name)

  #trim whitespace
  input_name <- trimws(input_name)
  
  
  library(taxize)
  
  #paleo
  res_paleodb <- taxize::gnr_resolve(input_name, data_source_ids = c(172), best_match_only = TRUE, fields = "all")
  #worms
  res_worms <- taxize::gnr_resolve(input_name, data_source_ids = c(9), best_match_only = TRUE, fields = "all")

  #Any matches from PaleoDB?
  if (exists("matched_name", res_paleodb)){
    paleodb_matched_name <- res_paleodb$matched_name
    paleodb_score <- res_paleodb$score
    paleodb_classification <- res_paleodb$classification_path
    paleodb_name_id <- res_paleodb$taxon_id
  }else{
    paleodb_matched_name <- NA
    paleodb_score <- NA
    paleodb_classification <- NA
    paleodb_name_id <- NA
  }
  
  #Any matches from WoRMS?
  if (exists("matched_name", res_worms)){
    worms_matched_name <- res_worms$matched_name
    worms_score <- res_worms$score
    worms_classification <- res_worms$classification_path
    worms_name_id <- res_worms$taxon_id
  }else{
    worms_matched_name <- NA
    worms_score <- NA
    worms_classification <- NA
    worms_name_id <- NA
  }
  
  
  return(cbind(input_name = input_name_original, paleodb_matched_name = paleodb_matched_name, paleodb_score = paleodb_score, paleodb_classification = paleodb_classification, paleodb_name_id = paleodb_name_id, worms_matched_name = worms_matched_name, worms_score = worms_score, worms_classification = worms_classification, worms_name_id = worms_name_id))
}
