#Function to search collectors in Harvard University Herbaria & Libraries
#' @export

findCollectorsBotany <- function(collector, year = NA, country = NA){
  url_check <- paste("http://kiki.huh.harvard.edu/databases/botanist_search.php?individual=on&json=y&name=", collector, sep = "")
  bot_search_page <- paste("http://kiki.huh.harvard.edu/databases/botanist_search.php?individual=on&name=", collector, sep = "")
  bot_info_page <- 'http://kiki.huh.harvard.edu/databases/botanist_search.php?mode=details&json=y&id='
  
  if (!is.na(country)){
    url_check <- paste(url_check, "&country=", country, sep = "")
  }
  
  url_content <- curl(url_check)
  suppressWarnings(check_res <- readLines(url_content, n = 1))
  
  #df with results
  list_of_bots <- data.frame(matrix(ncol = 8, nrow = 0, data = NA))
  names(list_of_bots) <- c("url", "name", "start", "end", "countries", "notes", "othernames", "collexfields") 
  
  #If nothing found, the system doesn't return json
  if (check_res != '<strong>Error: No matching results. </strong>'){
    if (!is.na(country)){
      bot_search_page <- paste(bot_search_page, "&country=", country, sep = "")
    }
    
    results = htmlParse(getURL(bot_search_page))
    
    bots_count <- length(xpathSApply(results, '//*[@id="main_text_wide"]/form/div/a', xmlValue, resolveNamespaces = FALSE, trim=TRUE))
    
    #iterate all results
    
    for (i in 1:bots_count){
      bot_id <- xpathSApply(results, paste('//*[@id="main_text_wide"]/form/div/input[', i, ']', sep=""), xmlGetAttr, 'value')
      bot_name <- xpathSApply(results, paste('//*[@id="main_text_wide"]/form/div/a[', i, ']', sep = ""), xmlValue, resolveNamespaces = FALSE, trim=TRUE)
      bot_detail_page <- paste(bot_info_page, bot_id, sep = "")
      
      bot_details <- getURL(bot_detail_page)
      bot_details_table <- readLines(tc <- textConnection(bot_details)); close(tc)
      
      #Get table of the person's data
      # from https://stackoverflow.com/a/1401367
      pagetree <- htmlTreeParse(bot_details_table, error=function(...){}, useInternalNodes = TRUE)
      
      # Extract table header and contents
      bot_table <- xpathSApply(pagetree, "/html/body/table/tr/td", xmlValue)
      
      # Convert character vector to dataframe
      content <- as.data.frame(matrix(bot_table, ncol = 2, byrow = TRUE))
      
      #make sure it is in utf8, from https://stat.ethz.ch/pipermail/r-help/2011-May/277209.html
      #not working
      # for (n in names(content)){
      #   Encoding(levels(content[[n]])) <-"UTF-8"
      # } 
      
      if ( length(as.character( content[content$V1 == "Name", 2] )) == 0 ){
        colname = NA
      }else
      {
        colname = as.character( content[content$V1 == "Name", 2] )
      }
      
      if ( length(as.character( content[content$V1 == "Variant name", 2] )) == 0 ){
        othernames = NA
      }else
      {
        othernames = paste(as.character( content[content$V1 == "Variant name", 2] ), collapse="|",sep="")
      }
      
      if ( length(as.character( content[content$V1 == "Date of birth", 2] )) == 0 ){
        dob = NA
      }else
      {
        dob = as.character( content[content$V1 == "Date of birth", 2] )
      }
      
      if ( length(as.character( content[content$V1 == "Date of death", 2] )) == 0 ){
        dod = NA
      }else
      {
        dod = as.character( content[content$V1 == "Date of death", 2] )
      }
      
      if ( length(as.character( content[content$V1 == "Specialty Collector", 2] )) == 0 ){
        colfields = NA
      }else
      {
        colfields = paste(as.character( content[content$V1 == "Specialty Collector", 2] ), collapse="|",sep="")
      }
      
      if ( length(as.character( content[content$V1 == "Geography Collector", 2] )) == 0 ){
        geoc = NA
      }else
      {
        geoc = as.character( content[content$V1 == "Geography Collector", 2] )
      }
      
      if ( length(as.character( content[content$V1 == "Remarks", 2] )) == 0 ){
        colnotes = NA
      }else
      {
        colnotes = as.character( content[content$V1 == "Remarks", 2] )
      }
      
      list_of_bots <- rbind(list_of_bots, cbind(bot_detail_page, colname, dob, dod, geoc, colnotes, othernames, colfields), stringsAsFactors = FALSE)
      
      }
    names(list_of_bots) <- c("url", "name", "from_date", "to_date", "countries", "notes", "othernames", "collexfields") 
    #filter by year  
    if (!is.na(year)){
      list_of_bots <- list_of_bots[ which(list_of_bots$start <= year & list_of_bots$end >= year), ]
    }
  }
  return(list_of_bots)
}
