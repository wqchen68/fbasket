# 每天更新 fantasy 名單增加進 biodata
rm(list=ls())
source("/home/chengil/R/fbasket/f_dbconnect.R")

# sprlist <- dbGetQuery(con, "SELECT * FROM syncplayerlist WHERE datarange='FULL'")
# biodata <- dbGetQuery(con, "SELECT * FROM biodata")

# dbGetQuery(con, "DELETE FROM biodata WHERE fbido in ('5598','5599')")

sprlist_id <- dbGetQuery(con, "SELECT fbido FROM syncplayerlist WHERE datarange='FULL'")
biodata_id <- dbGetQuery(con, "SELECT fbido FROM biodata")

diff_bio   <- setdiff(biodata_id, sprlist_id)

source("/home/chengil/R/fbasket/f_getbiodata.R")
biodata    <- do.call(rbind, lapply(diff_bio$fbido, f_getbiodata))

  
dbWriteTable(con, 'biodata', biodata, append = T, row.names = F, allow.keywords = T)
dbDisconnect(con)







# source("/home/chengil/R/fbasket/f_dbconnect.R")
# realtimeeff <- dbGetQuery(con, "SELECT * FROM realtimeeff")




