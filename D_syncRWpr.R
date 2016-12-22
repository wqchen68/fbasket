rm(list=ls())
library(rvest)
library(dplyr)
library(stringr)
rw_wb <- NULL
htmlstr <- read_html('http://www.rotoworld.com/teams/depth-charts/nba.aspx')
# get playerlist from rotoworld
rw_wb$player <- htmlstr %>% 
  html_nodes(xpath = "//*[@id='cp1_tblDepthCharts']//tr//td//tr//td/a") %>%
  html_text()

rw_wb$rwid <- htmlstr %>% 
  html_nodes(xpath = "//*[@id='cp1_tblDepthCharts']//tr//td//tr//td/a") %>%
  html_attr("href") %>%
  str_split("/") %>% 
  lapply(function(x) x[4]) %>% unlist()

rw_wb <- data.frame(rw_wb)

### diff playerlist between rotoworld and DB
source('/home/chengil/R/fbasket/f_dbconnect.R')
rwid_db <- dbGetQuery(con, "SELECT rwid FROM rwtable")
dbDisconnect(con)

rw_wb_id_add <- setdiff(rw_wb$rwid, rwid_db$rwid)

df_rw_add <- filter(rw_wb, rwid %in% rw_wb_id_add)

# gen fbid
source('/home/chengil/R/fbasket/t_player2fbid.R')
df_rw_add$fbid <- t_player2fbid(df_rw_add$player)

# gen fbido
source('/home/chengil/R/fbasket/f_dbconnect.R')
syncplayerlist <- dbGetQuery(con, "SELECT fbido, fbid FROM syncplayerlist where datarange = 'Full'")
dbDisconnect(con)

df_rw_add_manual <- df_rw_add %>%
  left_join(syncplayerlist) %>%
  filter(is.na(fbido)==1)

df_rw_add_fn <- df_rw_add %>%
  left_join(syncplayerlist) %>%
  filter(is.na(fbido)!=1) %>%
  mutate(report=NA, impact=NA, date=NA, info=NA)

source("/home/chengil/R/fbasket/f_dbconnect.R")
dbWriteTable(con, 'rwtable', df_rw_add_fn, append = T, row.names = F, allow.keywords = T)
dbDisconnect(con)



