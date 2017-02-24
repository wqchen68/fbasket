##### insert realtimebox to gamelog #####
rm(list=ls())
library(dplyr)
library(rvest)
library(stringr)
opendate <- as.Date("2016-10-25")
thisdate <- Sys.Date()-1
realdate <- as.Date(opendate:thisdate, origin = '1970-01-01')
source('/home/chengil/R/fbasket/f_dbconnect.R')
dbdate <- dbGetQuery(con, "SELECT distinct gamedate FROM allteamlog WHERE season = '2016' order by gamedate")
dbdate <- c(dbdate$gamedate, as.Date('2017-02-19'))
dbDisconnect(con)

adddate <- as.Date(setdiff(realdate, as.Date(dbdate)), origin = '1970-01-01')
source('/home/chengil/R/fbasket/f_realtimebox.R')
for (i in 1:length(adddate)){
  # sethtml <- readLines(paste0("http://sports.yahoo.com/nba/scoreboard/?date=",adddate[i]),warn=F,encoding="UTF-8")
  # pagetree <- htmlTreeParse(sethtml, useInternalNodes = TRUE, encoding='UTF-8')
  # gameid <- xpathSApply(pagetree,'//h4[@class="vs"]/a',xmlGetAttr,'href')
  # gameid <- substr(gameid,6,nchar(gameid)-1)
  
  htmlstr <- read_html(paste0("http://sports.yahoo.com/nba/scoreboard/?dateRange=", adddate[i]))
  
  gameid <- htmlstr %>%
    html_nodes(xpath = "//*[@id='scoreboard-group-2']/div/ul/li/div/div[1]/a") %>%
    html_attr("href") %>%
    str_split("/") %>%  #"/nba/golden-state-warriors-toronto-raptors-2016100128/"
    sapply(function(x) x[3])
  
  game_status <- htmlstr %>%
    html_nodes(xpath = "//*[@id='scoreboard-group-2']/div/ul/li/div/div/a/div/div/div/div[1]/div[2]/div/div/div/span") %>%
    html_text()
  
  gameid <- gameid[which(game_status!='Ppd.')]
  
  # gameppd <- xpathSApply(pagetree,'//span[@class="time"]',xmlValue) #game ppd.
  gameppd <- NULL
  
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
    colnames(BoxScroePRALL)[colnames(BoxScroePRALL)=="gamedate"] <- "gdate" #just for html naming
    colnames(BoxScroePRALL)[colnames(BoxScroePRALL)=="oppo"] <- "goppo"
    BoxScroePRALL <- subset(BoxScroePRALL, select = -c(szv, oncourt, livemark)) #allgamelog no need
    
    source('/home/chengil/R/fbasket/f_dbconnect.R')
    dbWriteTable(con, 'allteamlog', BoxScroeTMALL, append = T, row.names = F, allow.keywords = T)
    dbWriteTable(con, 'allgamelog', BoxScroePRALL, append = T, row.names = F, allow.keywords = T)
    dbDisconnect(con)
  }
  
}






