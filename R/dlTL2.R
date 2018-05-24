#' Download the Taxonomic Literature data dump
#' 
#'Function to download and load to a data.frame the 
#' list of authors in: 
#' Taxonomic Literature: A selective guide to botanical 
#' publications and collections with dates, commentaries
#' and types (Stafleu et al.).
#'
#' @details http://www.sil.si.edu/DigitalCollections/tl-2/index.cfm
#' @return A dataframe of \code{authors}.

#' @export
#' @importFrom curl curl_download
#' @importFrom XML xmlParse
#' @importFrom XML xmlToList
#' 
#' 
dlTL2 <- function(){
  
  #check if data file already exists
  authors_data_file <- "data/TL2data.RData"
  if(file.exists(authors_data_file)){
    load(file = "data/TL2data.RData")
  }else{
  
    curl::curl_download(url = "http://www.sil.si.edu/DigitalCollections/tl-2/zip/tl2-data-v1.2.zip", "tl2-data-v1.2.zip", quiet = FALSE, mode = "wb")
    unzip("tl2-data-v1.2.zip")
    unlink("tl2-data-v1.2.zip")
    
    tl2_folder <- "tl2-data-v1.2/xml/"
    
    file.names <- dir(tl2_folder, pattern =".xml")
    
    authors <- data.frame(matrix(data = NA, ncol = 3, nrow = 0))
    
    for(b in 1:length(file.names)){
      print(file.names[b])
      
      #This file is broken for some reason
      if (file.names[b] == "TL_2_Vol_2.xml"){
        next
      }
      
      xml_file <- paste(tl2_folder, file.names[b], sep = "")
      xmlfile <- XML::xmlParse(xml_file)
      xml_data <- XML::xmlToList(xmlfile)
      #get pages
      xml_pages <- xml_data[["Volume"]][["Pages"]]
      
      #iterate over each page
      for (i in 1:length(xml_pages)){
        print(i)
        xml_authors <- xml_pages[i]$Page
        for (j in 1:length(xml_authors)){
          #iterate over each author
          this_author <- xml_authors$Authors[j]
          if (this_author != "NULL"){
            this_author_data <- this_author$Author$Name$text
            this_author_data <- strsplit(this_author_data, '[)],')[[1]][1]
            this_author_data1 <- strsplit(this_author_data, ' [(]')[[1]][2]
            
            if (is.integer(substr(this_author_data1, 1, 4))){
              this_author_name <- strsplit(this_author_data, ' [(]')[[1]][1]
              this_author_dates <- strsplit(this_author_data, ' [(]')[[1]][2]
              this_author_dates <- strsplit(this_author_dates, '[)]')[[1]][1]
              
            }else{
              this_author_name <- strsplit(this_author_data, ' [(]')[[1]][1]
              this_author_dates <- strsplit(this_author_data, ' [(]')[[1]][2]
              this_author_dates <- strsplit(this_author_dates, '[)]')[[1]][1]
            }
  
            this_author_dob <- strsplit(this_author_dates, '[-]')[[1]][1]
            this_author_dod <- strsplit(this_author_dates, '[-]')[[1]][2]
            authors <- rbind(authors, cbind(name = this_author_name, dob = this_author_dob, dod = this_author_dod))
          }
  
        }
      }
    }
    save(authors, file = "data/TL2data.RData")
  }
  return(authors)
}
