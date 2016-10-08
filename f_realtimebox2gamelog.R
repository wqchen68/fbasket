##### insert realtimebox to gamelog #####
rm(list=ls())
opendate <- as.Date("2015-10-27")
thisdate <- Sys.Date()-1
realdate <- as.Date(opendate:thisdate, origin = '1970-01-01')
source('/home/chengil/R/fansboard/f_dbconnect.R')
dbdate <- dbGetQuery(con, "SELECT distinct gamedate FROM allteamlog WHERE season = '2015' order by gamedate")
dbDisconnect(con)

adddate <- as.Date(setdiff(realdate, as.Date(dbdate$gamedate)), origin = '1970-01-01')
source('/home/chengil/R/fansboard/f_realtimebox.R')
for (i in 1:length(adddate)){
  sethtml <- readLines(paste0("http://sports.yahoo.com/nba/scoreboard/?date=",adddate[i]),warn=F,encoding="UTF-8")
  pagetree <- htmlTreeParse(sethtml, useInternalNodes = TRUE, encoding='UTF-8')
  gameid <- xpathSApply(pagetree,'//h4[@class="vs"]/a',xmlGetAttr,'href')
  gameid <- substr(gameid,6,nchar(gameid)-1)
  gameppd <- xpathSApply(pagetree,'//span[@class="time"]',xmlValue) #game ppd.

  if (length(gameid) != 0){ #skip no game day
    repeat{
      
      tryCatch({boxscroe <- lapply(gameid, f_realtimebox, 2)}, error = function(e){})
      BoxScroeALL <- do.call(rbind, boxscroe)
      BoxScroeTMALL <- do.call(rbind, BoxScroeALL[,1])
      BoxScroePRALL <- do.call(rbind, BoxScroeALL[,2])
      if (length(unique(BoxScroeTMALL$gameid)) == (length(gameid)-length(gameppd))){ #check games
        break
      }
    }
    colnames(BoxScroePRALL)[colnames(BoxScroePRALL)=="gamedate"] <- "gdate"
    colnames(BoxScroePRALL)[colnames(BoxScroePRALL)=="oppo"] <- "goppo"
    BoxScroePRALL <- subset(BoxScroePRALL, select = -c(szv, oncourt, livemark)) #allgamelog no need

    source('/home/chengil/R/fansboard/f_dbconnect.R')
    dbWriteTable(con, 'allteamlog', BoxScroeTMALL, append = T, row.names = F, allow.keywords = T)
    dbWriteTable(con, 'allgamelog', BoxScroePRALL, append = T, row.names = F, allow.keywords = T)
    dbDisconnect(con)
  }
}






