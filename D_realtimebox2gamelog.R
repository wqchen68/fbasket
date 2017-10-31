##### insert realtimebox to gamelog #####
rm(list=ls())
library(dplyr)
library(rvest)
library(stringr)
opendate <- as.Date("2017-10-17") #2017-18 open
# opendate <- as.Date("2017-04-15") #2016-17 playoff open
# opendate <- as.Date("2016-10-25") #2016-17 open
# opendate <- as.Date("2016-04-16") #2015-16 playoff open
# opendate <- as.Date("2015-10-27") #2015-16 open
# opendate <- as.Date("2015-04-18") #2014-15 playoff open
# opendate <- as.Date("2014-10-28") #2014-15 open

thisdate <- Sys.Date()-1 #now # thisdate <- as.Date('2018-04-11') #2017 end
# thisdate <- as.Date('2017-06-12') #2016-17 playoff end, 2017-02-19 ASG
# thisdate <- as.Date('2017-04-12') #2016-17 end
# thisdate <- as.Date('2016-06-19') #2015-16 playoff end, 2016-02-14 ASG
# thisdate <- as.Date('2016-04-13') #2015-16 end
# thisdate <- as.Date('2015-06-16') #2014-15 playoff end, 2015-02-15 ASG
# thisdate <- as.Date('2015-04-15') #2014-15 end


realdate <- as.Date(opendate:thisdate, origin = '1970-01-01')
source('/home/chengil/R/fbasket/f_dbconnect.R')
dbdate <- dbGetQuery(con, "SELECT distinct gamedate FROM allteamlog WHERE season = '2017' or season = '2017asg' order by gamedate")
dbdate <- c(dbdate$gamedate) # 要排除的
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


# 同一天出現在兩場球賽（通常發生在交易後第一天）
# BoxScroePRALL <- BoxScroePRALL %>%
#   filter(fbid != 'Brandan-Wright' | gameid != 'boston-celtics-indiana-pacers-2015010911')

# check script
# select season, count(*)
# from allteamlog
# group by season
# 
# select season, count(*)
# from (
#   select distinct season, gameid
#   from allgamelog
#   group by gameid
# ) a
# group by season
# 
# select tt, count(*)
# from (
#   select gameid, round(sum(bxmin)/10,1) as tt
#   from allgamelog
#   where season = '2016'
#   group by gameid
# ) a
# group by tt

