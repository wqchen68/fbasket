# once updated per season
rm(list=ls())
library(dplyr)
library(RMySQL)

source('/home/chengil/R/fbasket/f_dbconnect.R')
# all_fbido <- dbGetQuery(con, "SELECT fbido FROM biodata")
all_fbido <- dbGetQuery(con, "SELECT fbido FROM syncplayerlist WHERE datarange = 'Full'")
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

# ### SQL view for integrate careerstats_origin and careerstats_copy ###
# create view careerstats as
# select *
# from careerstats_origin
# union
# select `fbido`, `fbid`, `cseason`,
#   case cteam
#   when 'GS'  then 'GS'
#   when 'LAC' then 'LAC'
#   when 'LAL' then 'LAL'
#   when 'NO'  then 'NO'
#   when 'NY'  then 'NY'
#   when 'OKC' then 'OKC'
#   when 'SA'  then 'SA'
#   when 'GS\''  then 'GS\''
#   when 'LAC\'' then 'LAC\''
#   when 'LAL\'' then 'LAL\''
#   when 'NO\''  then 'NO\''
#   when 'NY\''  then 'NY\''
#   when 'OKC\'' then 'OKC\''
#   when 'SA\''  then 'SA\''		
#   else CONCAT(UCASE(LEFT(cteam, 1)), LCASE(SUBSTRING(cteam, 2)))
#   end as cteam, 
#   `cgame`,`cmin`,`cfgm`,`cfga`,`cfgp`,`c3ptm`,`c3pta`,`c3ptp`,`cftm`,`cfta`,`cftp`,`coreb`,`cdreb`,`ctreb`,`cast`,`cto`,`cst`,`cblk`,`cpf`,`cpts`,`catr`,`ceff`,
#   `ceff36`,`acmin`,`acfgm`,`acfga`,`ac3ptm`,`ac3pta`,`acftm`,`acfta`,`acoreb`,`acdreb`,`actreb`,`acast`,`acto`,`acst`,`acblk`,`acpf`,`acpts`,
#   `dataorder`,`updatetime`
# from careerstats_copy
# where cseason = '2016-17'


# SQL - check
# select a.fbido, a.fbid, b.fbido, b.fbid, b.cnt
# from biodata a
# left join (
#   select fbido, fbid, count(*) as cnt
#   from careerstats_copy
#   group by fbido
# ) b on a.fbido = b.fbido


# source("/home/chengil/R/fbasket/f_dbconnect.R")
# dbWriteTable(con, 'biodata_copy', biodata, append = T, row.names = F, allow.keywords = T)
# dbDisconnect(con)

