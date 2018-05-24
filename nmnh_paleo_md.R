library(worrms)
library(rgbif)
library(paleobioDB)

excelfile <- file.choose()

paleo_data <- read.csv(excelfile, header = TRUE, sep=",", stringsAsFactors = FALSE)



#search worms using fuzzy matching

taxa_check <- data.frame(matrix(data = NA, ncol = 3, nrow = 0))

for(i in 1:dim(paleo_data)[1]){
  w <- try(wm_records_taxamatch(name = paleo_data$Taxonomy_Verbatim[i]), silent = TRUE)
  if (class(w)=="try-error"){
    w_sp <- NA
  }else{
    w_sp <- w[[1]]$scientificname
  }

  g <- try(name_lookup(query = paleo_data$Taxonomy_Verbatim[i]), silent = TRUE)
  if (class(w)=="try-error"){
    g_sp <- NA
  }else{
    g_sp <- unlist(unique(g$data$scientificName))
  }
  
  taxa_check <- rbind(taxa_check, cbind(verbatim_name = paleo_data$Taxonomy_Verbatim[i], worms_ccr = w_sp, gbif_ccr = g_sp))
}




#check gbif backbone
a <- name_backbone('Pseudocardium densatum', strict = FALSE, verbose = TRUE)


#check all taxonomies in gbif
a <- name_lookup('Pseudocardium densatum', rank = "species", verbose = TRUE)


