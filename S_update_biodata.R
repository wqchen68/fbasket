# once updated per season
rm(list=ls())
library(dplyr)
all_bio <- 1:5770 %>% as.character()
# all_bio <- 5760:5763 %>% as.character()
# all_bio <- 10:20 %>% as.character()

source("/home/chengil/R/fbasket/f_getbiodata.R")
# biodata <- lapply(all_bio, function(x){
#     tryCatch({
#       f_getbiodata(x)
#     },error = function(e){
#     })
#   }) %>% bind_rows()

biodata <- lapply(all_bio, function(x){
    tryCatch({
      f_getbiodata(x)
    },error = function(e){
    })
  })

# source("/home/chengil/R/fbasket/f_dbconnect.R")
# dbWriteTable(con, 'biodata_copy', biodata, append = T, row.names = F, allow.keywords = T)
# dbDisconnect(con)



# some sql scripts of adjustment for DB
# select *
# from biodata_copy
# where number = "\n"
# 
# UPDATE biodata_copy SET number=NULL WHERE number='\n';

# select *
#   from biodata_copy
# where length(position) > 2
# 
# UPDATE biodata_copy SET position=NULL WHERE length(position) > 2;

