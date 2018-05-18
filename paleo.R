library(worrms)
library(rgbif)
library(paleobioDB)
library(CoordinateCleaner)



#search worms using fuzzy matching
b <- wm_records_taxamatch(name = 'Platanista gangeticus')



#check gbif backbone
a <- name_backbone('Pseudocardium densatum', strict = FALSE, verbose = TRUE)


#check all taxonomies in gbif
a <- name_lookup('Pseudocardium densatum', rank = "species", verbose = TRUE)


