##### 每天更新 cate9ms #####
rm(list=ls())
library(dplyr)
source("/home/chengil/R/fbasket/f_dbconnect.R")
dbGetQuery(con, "TRUNCATE cate9ms")
query <- paste0("
  select
    AVG(bxfga) as pwfga,
    AVG(bxfta) as pwfta,
    SUM(bxfgm) as wfgm,  SUM(bxfga) as wfga,
    SUM(bxftm) as wftm,  SUM(bxfta) as wfta,
    SUM(bxfgm)/SUM(bxfga) as pwfgp,
    SUM(bxftm)/SUM(bxfta) as pwftp,
    AVG(bx3ptm) as pw3ptm,
    AVG(bxpts)  as pwpts,
    AVG(bxtreb) as pwtreb,
    AVG(bxast)  as pwast,
    AVG(bxst)   as pwst,
    AVG(bxblk)  as pwbxblk,
    AVG(bxto)   as pwbxto
  from allgamelog
  where season = ",
    toString(ifelse(as.integer(format(Sys.Date(), "%m"))<7,
           as.integer(format(Sys.Date(), "%Y"))-1,
           as.integer(format(Sys.Date(), "%Y"))))
  ," group by fbido
")

# query <- paste0("
#                 select 
#                 AVG(bxfga) as pwfga,
#                 AVG(bxfta) as pwfta,
#                 SUM(bxfgm) as wfgm,  SUM(bxfga) as wfga,
#                 SUM(bxftm) as wftm,  SUM(bxfta) as wfta,
#                 SUM(bxfgm)/SUM(bxfga) as pwfgp,
#                 SUM(bxftm)/SUM(bxfta) as pwftp,
#                 AVG(bx3ptm) as pw3ptm,
#                 AVG(bxpts)  as pwpts,
#                 AVG(bxtreb) as pwtreb,
#                 AVG(bxast)  as pwast,
#                 AVG(bxst)   as pwst,
#                 AVG(bxblk)  as pwbxblk,
#                 AVG(bxto)   as pwbxto
#                 from allgamelog
#                 where season = '2015' group by fbido")

agg_pr <- dbGetQuery(con, query)
agg_pr$pwfgp <- (agg_pr$pwfgp - sum(agg_pr$wfgm, na.rm = T)/sum(agg_pr$wfga, na.rm = T))*agg_pr$pwfga
agg_pr$pwftp <- (agg_pr$pwftp - sum(agg_pr$wftm, na.rm = T)/sum(agg_pr$wfta, na.rm = T))*agg_pr$pwfta

cate9ms <- NULL
cate9ms$mean <- group_by(agg_pr) %>% summarise_each_(funs(mean(., na.rm = TRUE)),names(agg_pr)[-1:-6]) %>% t()
cate9ms$std <- group_by(agg_pr) %>% summarise_each_(funs(sd(., na.rm = TRUE)),names(agg_pr)[-1:-6]) %>% t()
cate9ms$percentage <- rep(0,9)
cate9ms$percentage[1]  <- sum(agg_pr$wfgm, na.rm = T)/sum(agg_pr$wfga, na.rm = T)
cate9ms$percentage[2]  <- sum(agg_pr$wftm, na.rm = T)/sum(agg_pr$wfta, na.rm = T)

cate9ms <- data.frame(cate9ms)
dbWriteTable(con, 'cate9ms', cate9ms, append = T, row.names = F, allow.keywords = T)
dbDisconnect(con)





