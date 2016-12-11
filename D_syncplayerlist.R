##### 每天更新 syncplayerlist #####
f_syncplayerlist <- function(datarange){
  library(dplyr)
  library(XML)
  library(RCurl)
  FULLplayerlistO <- NULL
  PrList <- NULL
  # datarange <- "ALL"
  datarange_url <- switch(datarange, 
                          "ALL"  = "S_S_2016",
                          "Full" = "S_S_2016",
                          "D30"  = "S_L30",
                          "D14"  = "S_L14",
                          "D07"  = "S_L7",
                          "Y-1"  = "S_S_2015",
                          "Y-2"  = "S_S_2014")
  for (i in seq(0, 625, 25)){
    # i <- 0
    sethtml <- readLines(curl(paste0("http://basketball.fantasysports.yahoo.com/nba/8759/players?status=ALL&pos=P&cut_type=33&stat1=", datarange_url, "&myteam=0&sort=OR&sdir=1&count=", toString(i))), warn=F, encoding="UTF-8")
    pagetree <- htmlTreeParse(sethtml, useInternalNodes = TRUE, encoding='UTF-8')
  
    PrList$datarange <- datarange
    PrList$fbido   <- xpathSApply(pagetree,'//span[@class="player-status Grid-u Lh-xs"]//a[@target="_blank"]', xmlGetAttr,'data-ys-playerid')
    PrList$player  <- xpathSApply(pagetree,'//a[@class="Nowrap"]', xmlValue)
    tepo           <- xpathSApply(pagetree,'//span[@class="Fz-xxs"]', xmlValue)
    dd             <- data.frame(strsplit(tepo, " - ")) %>% t()
    row.names(dd)  <- NULL
    PrList$team    <- dd[,1]
    PrList$position<- dd[,2]
    PrList$injna   <- xpathSApply(pagetree,'//span[@class="ysf-player-status F-injury Fz-xxs Grid-u Lh-xs"]', xmlValue)
    PrList$orank   <- xpathSApply(pagetree,'//td[@class="Alt Ta-end Nowrap Selected"]', xmlValue) %>% as.numeric()
    arank          <- xpathSApply(pagetree,'//td[@class="Ta-end Bdrend"]', xmlValue)
    arank          <- matrix(arank,3,) %>% t()
    PrList$arank   <- as.numeric(arank[,1])
    owned          <- xpathSApply(pagetree,'//td[@class="Alt Ta-end Nowrap Bdrend"]', xmlValue)
    PrList$owned   <- sub("%","",owned)
    
    FULLplayerlistO  <- rbind(FULLplayerlistO, data.frame(PrList))
  }
  
  source("/home/chengil/R/fbasket/f_dbconnect.R")
  biodata <- dbGetQuery(con, "SELECT fbido,player,number,fbid FROM biodata")
  FULLplayerlist <- merge.data.frame(FULLplayerlistO, biodata, by="fbido", all.x = T)
  FULLplayerlist <- subset(FULLplayerlist, select = -c(player.y, number))
  colnames(FULLplayerlist)[colnames(FULLplayerlist)=="player.x"] <- "player"
  FULLplayerlist$fbid[which(is.na(FULLplayerlist$fbid))] <- "ERROR"
  
  dbGetQuery(con, paste0("DELETE FROM syncplayerlist WHERE datarange='", datarange, "'"))
  dbWriteTable(con, 'syncplayerlist', FULLplayerlist, append = T, row.names = F, allow.keywords = T)
  dbDisconnect(con)
}


