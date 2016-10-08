f_realtimebox = function(gameid, type){
  # type 1: realtimebox, 2:gamelog # for website from DB: score
  # gameid <- "washington-wizards-philadelphia-76ers-2016100620"
  # type <- 1
  library(rvest)
  library(RMySQL)
  library(magrittr)
  BoxScroePR <- NULL
  BoxScroeTM <- NULL
  htmlstr <- paste0('http://sports.yahoo.com/nba/', gameid) %>% read_html()

  htmltable <- htmlstr %>% html_table()
  
  box_s1 <- htmltable[[5]]; box_s2 <- htmltable[[7]]
  box_b1 <- htmltable[[6]]; box_b2 <- htmltable[[8]]
  
  boxA <- rbind(box_s1[,2:ncol(box_s1)], box_b1[1:(nrow(box_b1)-2),2:ncol(box_b1)])
  boxB <- rbind(box_s2[,2:ncol(box_s2)], box_b2[1:(nrow(box_b2)-2),2:ncol(box_b2)])
  
  boxtm <- rbind(box_b1[which(box_b1$Bench=="Totals"),3:ncol(box_b1)],
                 box_b2[which(box_b2$Bench=="Totals"),3:ncol(box_b2)])
  boxpr <- rbind(boxA, boxB)

  ##### preparation #####
  gamedate <- str_split(gameid, "-") %>% unlist() %>% tail(1)
  season   <- ifelse(as.integer(substr(gamedate,5,6)) < 7,as.character(as.integer(substr(gamedate,1,4))-1),substr(gamedate,1,4))
  source("/home/chengil/R/fansboard/t_teamName2id.R")
  team <- htmlstr %>%
    html_nodes(xpath = "//*[@id='Col1-0-Boxscore']/div[4]/div/div/div/div[1]/h3/span") %>%
    html_text() %>% lapply(t_teamName2id) %>% unlist()
  
  oppo     <- c(paste0('@',team[2]),team[1])
  score1    <- c(paste0(boxtm$Pts[1], "-", boxtm$Pts[2])) # game score, LIVE!
  score2    <- c(paste0(boxtm$Pts[2], "-", boxtm$Pts[1])) # -1 score, LIVE!
  
  livemark <- htmlstr %>%
    html_nodes(xpath = "//*[@id='Col1-0-Boxscore']/div[1]/div[3]/div/div/div[3]/div/div[1]/div") %>%
    html_text() %>% substr(1,5)

  ##### BoxScroeTM #####
  BoxScroeTM$team <- htmlstr %>%
    html_nodes(xpath = "//*[@id='Col1-0-Boxscore']/div[4]/div/div/div/div[1]/h3/span") %>%
    html_text() %>% lapply(t_teamName2id) %>% unlist()
  
  BoxScroeTM$team    <- team
  BoxScroeTM$season  <- ifelse(as.integer(substr(gamedate,5,6)) < 7,as.character(as.integer(substr(gamedate,1,4))-1),substr(gamedate,1,4))
  BoxScroeTM$gamedate<- str_split(gameid, "-") %>% unlist() %>% tail(1)
  BoxScroeTM$gameid  <- gameid
  BoxScroeTM$oppo    <- c(paste0("@", BoxScroeTM$team[2]), BoxScroeTM$team[1])
  
  if (type == 2){
    if (as.numeric(boxtm$Pts[1])-as.numeric(boxtm$Pts[2]) < 0){
      BoxScroeTM$score <- c(paste0("L ", score1), paste0("W ", score2))
    }else{
      BoxScroeTM$score <- c(paste0("W ", score1), paste0("L ", score2))
    }
  }else{
    BoxScroeTM$score <- c(score1, score1)
  }

  parsePtg_n <- function(x) strsplit(x, "-") %>% unlist() %>% head(1) %>% as.numeric() # numerator
  parsePtg_d <- function(x) strsplit(x, "-") %>% unlist() %>% tail(1) %>% as.numeric() # denominator
  
  BoxScroeTM$bxfgm  <- sapply(boxtm$FG, parsePtg_n)
  BoxScroeTM$bxfga  <- sapply(boxtm$FG, parsePtg_d)
  BoxScroeTM$bxfgp  <- BoxScroeTM$bxfgm/BoxScroeTM$bxfga*100
  BoxScroeTM$bx3ptm <- sapply(boxtm$`3pt`, parsePtg_n)
  BoxScroeTM$bx3pta <- sapply(boxtm$`3pt`, parsePtg_d)
  BoxScroeTM$bx3ptp <- BoxScroeTM$bx3ptm/BoxScroeTM$bx3pta*100
  BoxScroeTM$bxftm  <- sapply(boxtm$FT, parsePtg_n)
  BoxScroeTM$bxfta  <- sapply(boxtm$FT, parsePtg_n)
  BoxScroeTM$bxftp  <- BoxScroeTM$bxftm/BoxScroeTM$bxfta*100
  BoxScroeTM$bxoreb <- as.integer(boxtm$Off)
  BoxScroeTM$bxdreb <- as.integer(boxtm$Def)
  BoxScroeTM$bxtreb <- as.integer(boxtm$Reb)
  BoxScroeTM$bxast  <- as.integer(boxtm$Ast)
  BoxScroeTM$bxto   <- as.integer(boxtm$TO)
  BoxScroeTM$bxatr  <- as.integer(boxtm$Ast)/as.integer(boxtm$TO)
  BoxScroeTM$bxst   <- as.integer(boxtm$Stl)
  BoxScroeTM$bxblk  <- as.integer(boxtm$Blk)
  BoxScroeTM$bxba   <- as.integer(boxtm$BA)
  BoxScroeTM$bxpf   <- as.integer(boxtm$PF)
  BoxScroeTM$bxpts  <- as.integer(boxtm$Pts)
  BoxScroeTM$bxcreb <- c(box_b1 %>% filter(Bench=="Percentages") %>% select(Off) %>% str_split(":") %>% unlist() %>% tail(1) %>% as.numeric()
                        ,box_b2 %>% filter(Bench=="Percentages") %>% select(Off) %>% str_split(":") %>% unlist() %>% tail(1) %>% as.numeric())
  #TeamLog=[team season date gameid oppo score FGM FGA FGP 3PTM 3PTA 3PTP FTM FTA FTP OREB DREB TREB AST TO A/T ST BLK BA PF PTS CREB]
  #           1     2     3     4      5    6   7   8   9   10   11   12   13  14  15  16   17   18   19 20  21 22  23 24 25  26  27
  
  ##### BoxScroePR #####
  ppridlist <- htmlstr %>% 
    html_nodes(xpath = "//*[@id='Col1-0-Boxscore']/div[4]/div/div/div/div/div/table/tbody/tr/th/div/a") %>%
    html_attr("href") %>% strsplit('/')

  BoxScroePR$fbido <- sapply(ppridlist, last)
  
  fbid0parse <- function(x) substr(x,1,nchar(x)-1) # remove position behind player name GFC
  
  fbid0 <- c(unlist(lapply(box_s1$Starters, fbid0parse)), box_b1$Bench[1:(nrow(box_b1)-2)]
            ,unlist(lapply(box_s2$Starters, fbid0parse)), box_b2$Bench[1:(nrow(box_b2)-2)])
  BoxScroePR$fbid <- gsub(' ','-',gsub('\'','',gsub('\\.','',fbid0)))
  
  box1 <- htmlstr %>% 
    html_nodes("#Col1-0-Boxscore > div.player-stats > div > div > div:nth-child(1) td:nth-child(2)") %>%
    html_text()
  box1 <- boxA %>% select(Min)
  
  box2 <- htmlstr %>% 
    html_nodes("#Col1-0-Boxscore > div.player-stats > div > div > div:nth-child(2) td:nth-child(2)") %>%
    html_text()
  box2 <- boxB %>% select(Min)
  
  startfive1 <- rep("DNP", nrow(box1))
  startfive1[which(box1 != "-")] <- "BN"
  startfive1[1:5] <- c('F','F','C','G','G')
  startfive2 <- rep("DNP", nrow(box2))
  startfive2[which(box2 != "-")] <- "BN"
  startfive2[1:5] <- c('F','F','C','G','G')
  
  BoxScroePR$season  <- season
  BoxScroePR$gamedate<- gamedate
  BoxScroePR$gameid  <- gameid
  BoxScroePR$team    <- rep(team,c(nrow(boxA),nrow(boxB)))
  BoxScroePR$oppo    <- rep(oppo,c(nrow(boxA),nrow(boxB)))
  BoxScroePR$score   <- c(rep(as.character(BoxScroeTM$score[1]), nrow(boxA)),rep(as.character(BoxScroeTM$score[2]), nrow(boxB)))
  BoxScroePR$startfive <- c(startfive1,startfive2)
  BoxScroePR$bxgs <- rep(1,nrow(boxpr))
  BoxScroePR$bxgs[which(BoxScroePR$startfive == 'BN')] <- 0
  BoxScroePR$bxgs[which(BoxScroePR$startfive == 'DNP')] <- NA
  BoxScroePR$bxmin <- sapply(boxpr$Min,
                             function(x){
                               ifelse(nchar(as.character(x))<=5,
                                      as.integer(strsplit(as.character(x),':')[[1]][1])+as.integer(strsplit(as.character(x),':')[[1]][2])/60,NA)
                             }
  )
  BoxScroePR$bxfgm   <- sapply(boxpr$FG, parsePtg_n)
  BoxScroePR$bxfga   <- sapply(boxpr$FG, parsePtg_d)
  BoxScroePR$bxfgp   <- BoxScroePR$bxfgm/BoxScroePR$bxfga*100
  BoxScroePR$bx3ptm  <- sapply(boxpr$`3pt`, parsePtg_n)
  BoxScroePR$bx3pta  <- sapply(boxpr$`3pt`, parsePtg_d)
  BoxScroePR$bx3ptp  <- BoxScroePR$bx3ptm/BoxScroePR$bx3pta*100
  BoxScroePR$bxftm   <- sapply(boxpr$FT, parsePtg_n)
  BoxScroePR$bxfta   <- sapply(boxpr$FT, parsePtg_d)
  BoxScroePR$bxftp   <- BoxScroePR$bxftm/BoxScroePR$bxfta*100
  BoxScroePR$bxoreb  <- as.integer(boxpr$Off)
  BoxScroePR$bxdreb  <- as.integer(boxpr$Def)
  BoxScroePR$bxtreb  <- as.integer(boxpr$Reb)
  BoxScroePR$bxast   <- as.integer(boxpr$Ast)
  BoxScroePR$bxto    <- as.integer(boxpr$TO)
  BoxScroePR$bxatr   <- as.integer(boxpr$Ast)/as.integer(boxpr$TO)
  BoxScroePR$bxst    <- as.integer(boxpr$Stl)
  BoxScroePR$bxblk   <- as.integer(boxpr$Blk)
  BoxScroePR$bxpf    <- as.integer(boxpr$PF)
  BoxScroePR$bxpts   <- as.integer(boxpr$Pts)
  BoxScroePR$bxeff   <- (as.integer(boxpr$Pts)+as.integer(boxpr$Reb)+as.integer(boxpr$Ast)+as.integer(boxpr$Stl)+as.integer(boxpr$Blk))-((as.integer(BoxScroePR$bxfga)-as.integer(BoxScroePR$bxfgm))+(as.integer(BoxScroePR$bxfta)-as.integer(BoxScroePR$bxftm))+as.integer(boxpr$TO))
  BoxScroePR$bxeff36 <- BoxScroePR$bxeff*(36/as.integer(BoxScroePR$bxmin))
  BoxScroePR$bxba    <- as.integer(boxpr$BA)
  BoxScroePR$bxporn  <- as.integer(as.character(boxpr$`+/-`))
  BoxScroePR$livemark<- livemark
  
  # oncourt0 <- xpathSApply(pagetree,'//div[@class="data-container"]/table/tbody/tr/th',xmlGetAttr,'class')
  # oncourt_vec <- regexpr('athlete',xpathSApply(pagetree,'//*[@class="data-container"]/table/tbody/tr/th',xmlGetAttr,'class'))
  # oncourt <- gsub('athlete','',oncourt0[which(oncourt_vec==1)])
  if (grep("Final", livemark)){ #always let on-court = '' if Final
    BoxScroePR$oncourt <- ''
  }else{
    BoxScroePR$oncourt <- gsub(' ','',oncourt)
  }
  
  repeat{
    source('/home/chengil/R/fansboard/f_dbconnect.R')
    if (is.null(con)==0){break}
  }
  cate9ms <- dbGetQuery(con, 'SELECT mean, std, percentage FROM cate9ms')
  cate9ms$mean <- cate9ms$mean*c(1,1,1,1,1,1,1,1,-1)
  dbDisconnect(con)
  
  adjfg <- (BoxScroePR$bxfgm/BoxScroePR$bxfga-cate9ms$percentage[1])*BoxScroePR$bxfga
  adjft <- (BoxScroePR$bxftm/BoxScroePR$bxfta-cate9ms$percentage[2])*BoxScroePR$bxfta
  boxcate9 <- cbind(adjfg,adjft,BoxScroePR$bx3ptm,BoxScroePR$bxpts,BoxScroePR$bxtreb,BoxScroePR$bxast,BoxScroePR$bxst,BoxScroePR$bxblk,-BoxScroePR$bxto)
  BoxScroePR$szv <- colSums((t(boxcate9)-cate9ms$mean)/cate9ms$std, na.rm=T)
  
  return(list(BoxScroeTM = data.frame(BoxScroeTM), BoxScroePR = data.frame(BoxScroePR)))
  
  # rm(list=ls())
  # data <- fromJSON("https://api-secure.sports.yahoo.com/v1/editorial/s/boxscore/nba.g.2016061909?lang=en-US&region=US&tz=America%2FLos_Angeles&ysp_redesign=1&mode=&v=4&ysp_enable_last_update=1&polling=1")
  # 
  # data_pr_i <- lapply(data$service$boxscore$player_stats, unlist)
  # data_tm_i <- lapply(data$service$boxscore$team_stats, unlist)
  # 
  # data_pr <- do.call(rbind, data_pr_i)
  # data_tm <- do.call(rbind, data_tm_i)
  # 
  # 
  # library(rvest)
  # htmlstr <- html("http://sports.yahoo.com/nba/cleveland-cavaliers-golden-state-warriors-2016061909/")
  # 
  # htmltable <- htmlstr %>% 
  #   html_table()
  # 
  # boxA <- htmltable[[6]]
  # boxB <- htmltable[[8]]
  # 
  # rbind(boxA[which(boxA$Bench=="Totals"),3:ncol(boxA)],
  #       boxB[which(boxB$Bench=="Totals"),3:ncol(boxB)])
  # boxpr <- rbind(boxA[1:(nrow(boxA)-2),2:ncol(boxA)],
  #                boxB[1:(nrow(boxB)-2),2:ncol(boxB)])
  
  
  #   BoxScroePR$szv<- 
  #   # http://www.r-bloggers.com/htmltotext-extracting-text-from-html-via-xpath/
  #   
  #   xpathSApply(pagetree,'//*[@class="box-score-notes"]//span',xmlValue)
  #   xpathSApply(pagetree,'//*[@class="box-score-notes"]//dd//span',xmlValue)
  #   xpathSApply(pagetree,'//*[@class="box-score-notes"]//span',xmlValue)
  #   
  #   Flagrant Fouls: 
  #   Technical Fouls: 
  
  #   BoxScroePR$prname <- xpathSApply(pagetree,'//*[@class="athlete"]//*[@class="daily-fantasy-place-holder"]',xmlGetAttr,'data-entity-display-name')
  #   
  #   BoxScroe <- cbind(BoxScroe,box)
  #   
  #   BoxScroe <- data.frame(BoxScroe)
  #   boxscore=[fbid season date gameid team oppo score START GS Min FGM FGA FGP 3PTM 3PTA 3PTP FTM FTA FTP OREB DREB TREB AST TO A/T ST BLK BA PF PTS EFF EFF36 +/- LIVE oncourt]
  #             1     2     3     4     5    6    7     8    9  10  11  12  13  14   15   16   17  18  19  20   21   22   23 24  25 26  27 28 29  30  31 32   33  34     35

}
