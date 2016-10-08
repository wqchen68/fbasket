rm(list=ls())
library(XML)
library(RCurl)
library(RMySQL)

sethtml <- readLines(paste0("http://sports.yahoo.com/nba/scoreboard/?date=",Sys.Date()-1),warn=F,encoding = "UTF-8")
pagetree <- htmlTreeParse(sethtml, useInternalNodes = TRUE, encoding='UTF-8')
gameid <- xpathSApply(pagetree,'//h4[@class="vs"]/a',xmlGetAttr,'href')
gameid <- substr(gameid,6,nchar(gameid)-1)

source('/home/chengil/R/fansboard/f_realtimebox.R')
i=0
repeat{
  if (as.integer(as.numeric(Sys.time())) %% 24 == 0){
    i = i+1
    print(i)
    source('/home/chengil/R/fansboard/f_dbconnect.R')
    gamefinal <- dbGetQuery(con, 'SELECT gameid FROM realtimeeff WHERE livemark="Final" GROUP BY gameid')
    dbDisconnect(con)
    
    if (nrow(gamefinal)==length(gameid)){
      break
    }
    
    BoxScroePRALL <- NULL 
    for (g in 1:length(gameid)){
      if (length(grep(gameid[g],gamefinal))==0){
        
        tryCatch({
          gameop <- f_realtimebox(gameid[g], 1)
          print('------------------------------------------------------------')
        },error = function(e){
          print('error!!!!!!!!!!!!!!!!!!')
        })
  
        BoxScroePR <- gameop$BoxScroePR
        print(gameid[g])
        BoxScroePRALL <- rbind(BoxScroePRALL,BoxScroePR)
      }
    }
  }
  Sys.sleep(1)
}





# source('/home/chengil/R/fansboard/f_dbconnect.R')
# rtb <- dbGetQuery(con, 'TRUNCATE table realtimeeff')
# dbDisconnect(con)


