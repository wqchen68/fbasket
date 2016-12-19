# 每天晚上 23:00 ~ 08:00 每小時更新 player news (rwtable)
rm(list=ls())
source("/home/chengil/R/fbasket/f_dbconnect.R")
prlist_id <- dbGetQuery(con, "SELECT fbido FROM syncplayerlist WHERE datarange='Full'")
rwlist_id <- dbGetQuery(con, "SELECT fbido, fbid, player, rwid FROM rwtable")
dbDisconnect(con)

rwlist_input <- rwlist_id %>%
  inner_join(prlist_id)

# check
# setdiff(prlist_id$fbido, rwlist_input$fbido)

# convert df to list by rows
rwlist_input <- split(rwlist_input, seq(nrow(rwlist_input)))

source("/home/chengil/R/fbasket/f_rotoworld.R")

update_rwtable <- function(x){
  op <- f_rotoworld(x$rwid)
  x$rwid   <- op$rwid
  x$report <- op$report
  x$impact <- op$impact
  x$date   <- op$date
  x$info   <- op$info
  
  source("/home/chengil/R/fbasket/f_dbconnect.R")
  dbGetQuery(con, paste0("DELETE FROM rwtable WHERE fbido = ", x$fbido))
  dbWriteTable(con, 'rwtable', x, append = T, row.names = F, allow.keywords = T)
  dbDisconnect(con)
  # return(x)
}

lapply(rwlist_input, update_rwtable)


