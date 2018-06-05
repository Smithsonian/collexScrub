#' @export

find_sciname_paleo <- function(input_name){
  
  input_name_original <- input_name
  
  if (nchar(input_name) == 0){
    return(cbind(input_name = input_name_original, matched_name = NA, data_source = NA, score = NA))
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
  
  res <- resolve(input_name)
  
  return(cbind(input_name = input_name_original, matched_name = res$gnr$matched_name, data_source = res$gnr$data_source_title, score = res$gnr$score))
  
  # library(worrms)
  # library(rgbif)
  # 
  # name_check <- gsub("\"", "", name_check)
  # 
  # if (name_check == ""){
  #   return(verbatim_name = name_check, worms_ccr = NA, gbif_ccr = NA)
  # }
  # 
  # w <- try(worrms::wm_records_taxamatch(name = name_check), silent = TRUE)
  # if (class(w)=="try-error"){
  #   w_sp <- NA
  #   w_sp_id <- NA
  #   w_sp_status <- NA
  #   w_sp_accepted <- NA
  # }else{
  #   w_sp <- w[[1]]$scientificname
  #   w_sp_id <- w[[1]]$AphiaID
  #   w_sp_status <- w[[1]]$status
  #   w_sp_accepted <- w[[1]]$valid_name
  # }
  # 
  # g <- try(rgbif::name_backbone(name = name_check), silent = TRUE)
  # if (class(g)=="try-error"){
  #   g_sp <- NA
  #   g_sp_id <- NA
  #   g_sp_status <- NA
  #   g_sp_accepted <- NA
  # }else{
  #   #g_sp <- unlist(unique(g$data$scientificName[1]))
  #   g_sp <- g$canonicalName
  #   g_sp_id <- g$usageKey
  #   g_sp_status <- g$status
  #   g_sp_accepted <- g$species
  # }
  # 
  # #return(list(verbatim_name = name_check, worms_ccr = w_sp, gbif_ccr = g_sp))
  # return(c(name_check = name_check, w_sp = w_sp, w_sp_id = w_sp_id, w_sp_status = w_sp_status, w_sp_accepted = w_sp_accepted, g_sp = g_sp, g_sp_id = g_sp_id, g_sp_status = g_sp_status, g_sp_accepted = g_sp_accepted))
}
