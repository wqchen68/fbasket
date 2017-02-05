rm(list=ls())
library(dplyr)
library(RMySQL)
library(rvest)
library(stringr)

source('/home/chengil/R/fbasket/f_dbconnect.R')
dbGetQuery(con, 'DELETE FROM realtimeeff')
dbDisconnect(con)

htmlstr <- read_html(paste0("http://sports.yahoo.com/nba/scoreboard/?dateRange=",Sys.Date()-1))

gameid <- htmlstr %>%
  html_nodes(xpath = "//*[@id='scoreboard-group-2']/div/ul/li/div/div[1]/a") %>%
  html_attr("href") %>%
  str_split("/") %>%  #"/nba/golden-state-warriors-toronto-raptors-2016100128/"
  sapply(function(x) x[3])

source('/home/chengil/R/fbasket/f_realtimebox.R')
repeat{
  if (as.integer(as.numeric(Sys.time())) %% 24 == 0){
    # print(Sys.time())
    
    source('/home/chengil/R/fbasket/f_dbconnect.R')
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
          BoxScroePR <- gameop$BoxScroePR
          BoxScroePRALL <- rbind(BoxScroePRALL,BoxScroePR)
        },error = function(e){
          print('error!!!!!!!!!!!!!!!!!!')
        })
      }
    }
  
    if (is.null(BoxScroePRALL)==0) {
      source('/home/chengil/R/fbasket/f_dbconnect.R')
      dbGetQuery(con, "DELETE FROM realtimeeff WHERE livemark != 'Final'")
      dbWriteTable(con, 'realtimeeff', BoxScroePRALL, append = T, row.names = F, allow.keywords = T)
      dbDisconnect(con)  
    }
  }
  Sys.sleep(1)
}

source('/home/chengil/R/fbasket/f_dbconnect.R')
dbGetQuery(con, "update realtimeeff set oncourt = ''") # clear on-court, because timimg different
dbDisconnect(con)  

fileConn<-file("/home/chengil/R/fbasket/update_cron.txt")
writeLines("10 0 * * * Rscript /home/chengil/R/fbasket/f_checkFirstGame.R\n0 22-23,0-10 * * * Rscript /home/chengil/R/fbasket/D_updateRWtable.R", fileConn)
close(fileConn)
system("crontab /home/chengil/R/fbasket/update_cron.txt")


Sys.sleep(3); source("/home/chengil/R/fbasket/D_realtimebox2gamelog.R")
Sys.sleep(3); source("/home/chengil/R/fbasket/D_gamelog2dataframe.R")
Sys.sleep(3); source("/home/chengil/R/fbasket/D_syncplayerlist.R")
Sys.sleep(3); source("/home/chengil/R/fbasket/D_cate9ms.R")
Sys.sleep(3); source("/home/chengil/R/fbasket/D_FB2biodata.R")
Sys.sleep(3); source("/home/chengil/R/fbasket/D_syncRWpr.R")



# # temp query
# source('/home/chengil/R/fbasket/f_dbconnect.R')
# dd <- dbGetQuery(con, paste0("SELECT pwftm, pwfta, wftp FROM syncdataframe where datarange='ALL'"))
# dbDisconnect(con)


# ### timer
# repeat{
#   if (as.integer(as.numeric(Sys.time())) %% 10 == 0){
#     Sys.sleep(3)
#     print(Sys.time())
#   }
#   Sys.sleep(1)
# }

# rate issue
# d <- NULL
# d$ftm <- c(10,1,10,2)
# d$fta <- c(20,3,12,2)
# d <- data.frame(d)
# 
# d$ftp <- d$ftm/d$fta
# 
# d$adj1 <- (d$ftp-sum(d$ftm)/sum(d$fta))*d$fta
# d$adj2 <- (d$ftp-sum(d$ftm)/sum(d$fta))*log(d$fta)



