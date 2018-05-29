#Function to search authors in the Biodiversity Heritage Library
# Just a wrapper around the rbhl package
#' @export
findCollectorsBHL <- function(collector, year = NA, bhl_key = NA){
  if (is.na(bhl_key)){
    stop("bhl_key is missing")
  }
  
  bhl_res <- try(bhl_authorsearch(name = collector, key = bhl_key), silent = TRUE)
  
  #df with results
  list_of_bots <- data.frame(matrix(ncol = 8, nrow = 0, data = NA))
  names(list_of_bots) <- c("url", "name", "from_date", "to_date", "countries", "notes", "othernames", "collexfields") 
  
  #suppress warning if not NA
  # from https://stackoverflow.com/a/32719422
  oldw <- getOption("warn")
  options(warn = -1)
  
  if (class(bhl_res) != "try-error"){
    for (i in 1:dim(bhl_res)[1]){
      colname <- bhl_res[i,]$Name
      bot_detail_page <- bhl_res[i,]$CreatorUrl
      
      othernames <- bhl_res[i,]$FullerForm
      
      creator_dates <- bhl_res[i,]$Dates
      
      if (creator_dates != ""){
        dates <- strsplit(creator_dates, "-")[[1]]
        dob = dates[1]
        dod = dates[2]
      }else{
        dob = NA
        dod = NA
      }
      
      colfields = NA
      geoc = NA
      colnotes = NA
      
      list_of_bots <- rbind(list_of_bots, cbind(bot_detail_page, colname, dob, dod, geoc, colnotes, othernames, colfields), stringsAsFactors = FALSE)
      
    }
    
    #filter by year  
    if (!is.na(year)){
      list_of_bots <- list_of_bots[ which(list_of_bots$start <= year & list_of_bots$end >= year), ]
    }
    
  }
  options(warn = oldw)
  
  return(list_of_bots)
  
}
