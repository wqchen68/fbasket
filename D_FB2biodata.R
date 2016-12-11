# 每天更新 fantasy 名單增加進 biodata
rm(list=ls())
source("/home/chengil/R/fbasket/f_dbconnect.R")

sprlist_id <- dbGetQuery(con, "SELECT fbido FROM syncplayerlist WHERE datarange='Full'")
biodata_id <- dbGetQuery(con, "SELECT fbido FROM biodata")
dbDisconnect(con)

diff_bio <- setdiff(sprlist_id$fbido, biodata_id$fbido)

source("/home/chengil/R/fbasket/f_getbiodata.R")
biodata <- do.call(rbind, lapply(diff_bio, f_getbiodata))

source("/home/chengil/R/fbasket/f_dbconnect.R")
dbWriteTable(con, 'biodata', biodata, append = T, row.names = F, allow.keywords = T)
dbDisconnect(con)



