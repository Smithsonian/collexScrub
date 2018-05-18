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
library(CoordinateCleaner)









odbcCloseAll()
