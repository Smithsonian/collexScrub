library(RODBC)

dbfile <- file.choose()

ch <- odbcConnectAccess2007(dbfile)

tables <- sqlTables(ch, tableType = "TABLE")

dataTable <- select.list(tables$TABLE_NAME)

#? represent when not sure if the string belongs to the field
no_rows <- sqlQuery(ch, paste("SELECT count(*) FROM ",dataTable))

collectors_all <- sqlQuery(ch, paste("SELECT distinct collector_1 FROM ",dataTable))

collectors_noirn <- sqlQuery(ch, paste("SELECT distinct collector_1 FROM ",dataTable, " where irn_1 IS NULL"))

#for (i in 1:dim(collectors_noirn)[1]){
for (i in 1:100){
  name_to_check <- gsub("[?!*]", "", as.character(collectors_noirn[i,1]))
  results_HUH <- findCollectorsBotany(name_to_check)
  print(results_HUH)
}

#rbhl
bhl_res <- findCollectorsBHL(name_to_check, bhl_key = bhl_api_key)








library(plantlist)


























#check coords
library(CoordinateCleaner)
#library(sp)

##DMS
coords <- sqlQuery(ch, paste("SELECT id, sheet_barcode, country, dms_lat_degrees, dms_lat_minutes, dms_lat_seconds, dms_lat_ns, dms_long_degrees, dms_long_minutes, dms_long_seconds, dms_long_ew FROM ",dataTable, "WHERE coord_unit = 'DMS'"), stringsAsFactors = FALSE)

print(paste("  There are", dim(coords)[1], "location records"))

dat <- matrix(c(coords$id, coords$sheet_barcode, coords$country, coords$dms_lat_degrees, coords$dms_lat_minutes, coords$dms_lat_seconds, coords$dms_lat_ns, coords$dms_long_degrees, coords$dms_long_minutes, coords$dms_long_seconds, coords$dms_long_ew), 
              byrow = TRUE, nrow = 11)

for (i in 1:length(dat)){
  dms2dd <- function(deg = 0, min = 0, sec = 0, direction = "N")  
}








odbcCloseAll()
