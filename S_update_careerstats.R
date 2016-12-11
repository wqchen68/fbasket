# once updated per season
rm(list=ls())
library(dplyr)
library(RMySQL)

source('/home/chengil/R/fbasket/f_dbconnect.R')
all_fbido <- dbGetQuery(con, "SELECT fbido FROM biodata")
dbDisconnect(con)

source("/home/chengil/R/fbasket/f_getcareerstats.R")

# careerstats <- lapply(all_fbido$fbido, function(x){
#   tryCatch({
#     op <- f_getcareerstats(x)
#   },error = function(e){
#   })
# }) %>% bind_rows()

careerstats <- lapply(all_fbido$fbido, function(x){
  tryCatch({
    f_getcareerstats(x)
  },error = function(e){
    print(x)
  })
})




# source("/home/chengil/R/fbasket/f_dbconnect.R")
# dbWriteTable(con, 'biodata_copy', biodata, append = T, row.names = F, allow.keywords = T)
# dbDisconnect(con)

