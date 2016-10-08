f_realtimebox = function(gameid, type){
  # type 1: realtimebox, 2:gamelog
  # gameid <- 'cleveland-cavaliers-los-angeles-lakers-2016031013'
  # type <- 1
  library(XML)
  library(RCurl)
  library(RMySQL)
  library(magrittr)
  BoxScroePR <- NULL
  BoxScroeTM <- NULL
  sethtml <- readLines(paste0('http://sports.yahoo.com/nba/', gameid), warn=F, encoding="UTF-8")
  check <- regexpr('<colgroup><col><col><col><col><col><col><col><col><col><col><col><col><col><col><col><col></colgroup>',sethtml)
  if (sum(check>0)==2){ #game start
    pagetree <- htmlTreeParse(sethtml, useInternalNodes = TRUE, encoding='UTF-8')
    livemark <- ifelse(sub('nav-list ','',xpathSApply(pagetree,'//div[@id="Main"]/div[@id="mediamodulematchheadergrandslam"]/div[@class="hd"]/ul',xmlGetAttr,'class')) == 'final','Final','LIVE!')
    #basic
    team0    <- xpathSApply(pagetree,'//div[@class="team-info"]/div[@class="name"]/a',xmlGetAttr,'href')
    team     <- substr(team0,nchar(team0)-3,nchar(team0)-1)
    gamedate <- substr(gameid,nchar(gameid)-9,nchar(gameid)-2)
    season   <- ifelse(as.integer(substr(gamedate,5,6)) < 7,as.character(as.integer(substr(gamedate,1,4))-1),substr(gamedate,1,4))
    oppo     <- c(paste0('@',team[2]),team[1])
    
    score0   <- xpathSApply(pagetree,'//*[@class="boxscore"]/span',xmlValue)
    if (type == 1){
      score <- rep(paste0(score0,collapse = ''),2)
    }else{
      if (as.integer(score0[1])-as.integer(score0[3]) < 0){ #road team lose
        score <- c(paste0('L ', score0[3],'-',score0[1]),paste0('W ', score0[1],'-',score0[3]))
      }else{
        score <- c(paste0('W ', score0[1],'-',score0[3]),paste0('L ', score0[3],'-',score0[1]))
      }
    }
    BoxScroeTM$team    <- team
    BoxScroeTM$season  <- season
    BoxScroeTM$gamedate<- gamedate
    BoxScroeTM$gameid  <- gameid
    BoxScroeTM$oppo    <- oppo
    BoxScroeTM$score   <- score
    tbindex = ifelse(livemark=='Final',5,3) #table index from list
    # print(livemark)
    
    boxA <- readHTMLTable(sethtml, colClasses = c(rep('character',6),rep('integer',10)), which = tbindex)
    boxB <- readHTMLTable(sethtml, colClasses = c(rep('character',6),rep('integer',10)), which = tbindex+1)
    boxtm <- rbind(boxA[which(boxA$Players=="Totals"),3:ncol(boxA)],
                   boxB[which(boxB$Players=="Totals"),3:ncol(boxB)])
    boxpr <- rbind(boxA[1:(nrow(boxA)-2),2:ncol(boxA)],
                   boxB[1:(nrow(boxB)-2),2:ncol(boxB)])
    
    BoxScroeTM$bxfgm  <- as.integer(sapply(boxtm$FG,function(x){as.integer(strsplit(as.character(x),'-')[[1]][1])}))
    BoxScroeTM$bxfga  <- as.integer(sapply(boxtm$FG,function(x){as.integer(strsplit(as.character(x),'-')[[1]][2])}))
    BoxScroeTM$bxfgp  <- BoxScroeTM$bxfgm/BoxScroeTM$bxfga*100
    BoxScroeTM$bx3ptm <- as.integer(sapply(boxtm$`3pt`,function(x){as.integer(strsplit(as.character(x),'-')[[1]][1])}))
    BoxScroeTM$bx3pta <- as.integer(sapply(boxtm$`3pt`,function(x){as.integer(strsplit(as.character(x),'-')[[1]][2])}))
    BoxScroeTM$bx3ptp <- BoxScroeTM$bx3ptm/BoxScroeTM$bx3pta*100
    BoxScroeTM$bxftm  <- as.integer(sapply(boxtm$FT,function(x){as.integer(strsplit(as.character(x),'-')[[1]][1])}))
    BoxScroeTM$bxfta  <- as.integer(sapply(boxtm$FT,function(x){as.integer(strsplit(as.character(x),'-')[[1]][2])}))
    BoxScroeTM$bxftp  <- BoxScroeTM$bxftm/BoxScroeTM$bxfta*100
    BoxScroeTM$bxoreb <- boxtm$Off
    BoxScroeTM$bxdreb <- boxtm$Def
    BoxScroeTM$bxtreb <- boxtm$Reb
    BoxScroeTM$bxast  <- boxtm$Ast
    BoxScroeTM$bxto   <- boxtm$TO
    BoxScroeTM$bxatr  <- boxtm$Ast/boxtm$TO
    BoxScroeTM$bxst   <- boxtm$Stl
    BoxScroeTM$bxblk  <- boxtm$Blk
    BoxScroeTM$bxba   <- boxtm$BA
    BoxScroeTM$bxpf   <- boxtm$PF
    BoxScroeTM$bxpts  <- boxtm$Pts
    BoxScroeTM$bxcreb <- as.integer(gsub('Team Rebounds: ','',xpathSApply(pagetree,'//*[@class="teamrebounds stat-total"]',xmlValue)))
    #TeamLog=[team season date gameid oppo score FGM FGA FGP 3PTM 3PTA 3PTP FTM FTA FTP OREB DREB TREB AST TO A/T ST BLK BA PF PTS CREB]
    #           1     2     3     4      5    6   7   8   9   10   11   12   13  14  15  16   17   18   19 20  21 22  23 24 25  26  27
    
    BoxScroePR$fbido   <- xpathSApply(pagetree,'//th[@scope="row"]//div[@class="daily-fantasy-place-holder"]',xmlGetAttr,'data-entity-url-id')
    fbid0 <- gsub(' ','-',xpathSApply(pagetree,'//th[@scope="row"]//div[@class="daily-fantasy-place-holder"]',xmlGetAttr,'data-entity-display-name'))
    BoxScroePR$fbid    <- gsub('\'','',gsub('\\.','',fbid0)) #twice escape
    # source('/home/chengil/R/fansboard/t_player2fbid.R')
    # BoxScroePR$fbid <- xpathSApply(pagetree,'//th[@scope="row"]//div[@class="daily-fantasy-place-holder"]'
    #               ,xmlGetAttr,'data-entity-display-name') %>% t_player2fbid()
    BoxScroePR$season  <- season
    BoxScroePR$gamedate<- gamedate
    BoxScroePR$gameid  <- gameid
    BoxScroePR$team    <- rep(team,c(nrow(boxA)-2,nrow(boxB)-2))
    BoxScroePR$oppo    <- rep(oppo,c(nrow(boxA)-2,nrow(boxB)-2))
    BoxScroePR$score   <- c(rep(score[1], nrow(boxA)-2),rep(score[2], nrow(boxB)-2))
    BoxScroePR$startfive <- xpathSApply(pagetree,'//th[@scope="row"]//span[@class="position"]',xmlValue)
    BoxScroePR$startfive[which(is.na(boxpr$FG))] <- 'DNP'
    BoxScroePR$startfive[which(BoxScroePR$startfive=='')] <- 'BN'
    BoxScroePR$bxgs <- rep(1,nrow(boxpr))
    BoxScroePR$bxgs[which(BoxScroePR$startfive == 'BN')] <- 0
    BoxScroePR$bxgs[which(BoxScroePR$startfive == 'DNP')] <- NA
    BoxScroePR$bxmin <- sapply(boxpr$Min,
      function(x){
        ifelse(nchar(as.character(x))<=5,
          as.integer(strsplit(as.character(x),':')[[1]][1])+as.integer(strsplit(as.character(x),':')[[1]][2])/60,NA)
        }
    )
    BoxScroePR$bxfgm   <- as.integer(sapply(boxpr$FG,function(x){as.integer(strsplit(as.character(x),'-')[[1]][1])}))
    BoxScroePR$bxfga   <- as.integer(sapply(boxpr$FG,function(x){as.integer(strsplit(as.character(x),'-')[[1]][2])}))
    BoxScroePR$bxfgp   <- BoxScroePR$bxfgm/BoxScroePR$bxfga*100
    BoxScroePR$bx3ptm  <- as.integer(sapply(boxpr$`3pt`,function(x){as.integer(strsplit(as.character(x),'-')[[1]][1])}))
    BoxScroePR$bx3pta  <- as.integer(sapply(boxpr$`3pt`,function(x){as.integer(strsplit(as.character(x),'-')[[1]][2])}))
    BoxScroePR$bx3ptp  <- BoxScroePR$bx3ptm/BoxScroePR$bx3pta*100
    BoxScroePR$bxftm   <- as.integer(sapply(boxpr$FT,function(x){as.integer(strsplit(as.character(x),'-')[[1]][1])}))
    BoxScroePR$bxfta   <- as.integer(sapply(boxpr$FT,function(x){as.integer(strsplit(as.character(x),'-')[[1]][2])}))
    BoxScroePR$bxftp   <- BoxScroePR$bxftm/BoxScroePR$bxfta*100
    BoxScroePR$bxoreb  <- boxpr$Off
    BoxScroePR$bxdreb  <- boxpr$Def
    BoxScroePR$bxtreb  <- boxpr$Reb
    BoxScroePR$bxast   <- boxpr$Ast
    BoxScroePR$bxto    <- boxpr$TO
    BoxScroePR$bxatr   <- boxpr$Ast/boxpr$TO
    BoxScroePR$bxst    <- boxpr$Stl
    BoxScroePR$bxblk   <- boxpr$Blk
    BoxScroePR$bxpf    <- boxpr$PF
    BoxScroePR$bxpts   <- boxpr$Pts
    BoxScroePR$bxeff   <- (boxpr$Pts +boxpr$Reb +boxpr$Ast +boxpr$Stl +boxpr$Blk)-((BoxScroePR$bxfga-BoxScroePR$bxfgm)+(BoxScroePR$bxfta-BoxScroePR$bxftm)+boxpr$TO)
    BoxScroePR$bxeff36 <- BoxScroePR$bxeff*(36/as.integer(BoxScroePR$bxmin))
    BoxScroePR$bxba    <- boxpr$BA
    BoxScroePR$bxporn  <- as.integer(as.character(boxpr$`+/-`))
    BoxScroePR$livemark<- livemark
    
    oncourt0 <- xpathSApply(pagetree,'//div[@class="data-container"]/table/tbody/tr/th',xmlGetAttr,'class')
    oncourt_vec <- regexpr('athlete',xpathSApply(pagetree,'//*[@class="data-container"]/table/tbody/tr/th',xmlGetAttr,'class'))
    oncourt <- gsub('athlete','',oncourt0[which(oncourt_vec==1)])
    if (livemark == 'Final'){ #always let on-court = '' if Final
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
}
