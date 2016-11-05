f_realtimebox <- function(gameid, type){
  # gameid <- "houston-rockets-cleveland-cavaliers-2016110105"
  # type <- 2
  library(dplyr)
  library(tibble)
  library(magrittr)
  library(stringr)
  library(jsonlite)
  # library(rvest)
  BoxScroePR <- NULL
  BoxScroeTM <- NULL
  ### get api
  gameid_short <- str_split(gameid, "-")[[1]] %>% last()
  df <- fromJSON(paste0("https://api-secure.sports.yahoo.com/v1/editorial/s/boxscore/nba.g.", gameid_short, "?lang=en-US&region=US&tz=Asia%2FTaipei&ysp_redesign=1&mode=&v=4&ysp_enable_last_update=1&polling=1"))

  status_type_str <- paste0("df$service$boxscore$games$nba.g.", gameid_short, "$status_type")
  status_type <- eval(parse(text = status_type_str))
  
  if (status_type == "status.type.pregame"){
    return(list(BoxScroeTM = data.frame(BoxScroeTM), BoxScroePR = data.frame(BoxScroePR)))
    # print(status_type)
  }else{
    ### stat_type mapper
    stat_type_map <- sapply(df$service$boxscore$stat_types, "[[", 2) %>% data.frame(stringsAsFactors=FALSE) %>%
      set_colnames("stat_name") %>% rownames_to_column() %>% rename(stat_type=rowname)
    
    ############################################# team
    # stat_type_all_map
    box_col_tm <- sapply(df$service$boxscore$team_stats, "[[", 1) %>% t() %>% data.frame(stringsAsFactors=FALSE) %>%
      colnames() %>% data.frame(stringsAsFactors=FALSE) %>% set_colnames("stat_type")
    # stat_type_box_name
    stat_name_tm <- inner_join(box_col_tm, stat_type_map, "stat_type") %>% select(stat_name)
    # boxscore
    boxtm <- sapply(df$service$boxscore$team_stats, "[[", 1) %>% t() %>% data.frame(stringsAsFactors=FALSE) %>% 
      set_colnames(stat_name_tm$stat_name) %>% rownames_to_column()
    
    ############################################# player
    # stat_type_box
    box_col_pr <- sapply(df$service$boxscore$player_stats, "[[", 1) %>% t() %>% data.frame(stringsAsFactors=FALSE) %>% 
      colnames() %>% data.frame(stringsAsFactors=FALSE) %>% set_colnames("stat_type")
    # stat_type_box_name
    stat_name_pr <- inner_join(box_col_pr, stat_type_map, "stat_type") %>% select(stat_name)
    # boxscore
    boxpr0 <- sapply(df$service$boxscore$player_stats, "[[", 1) %>% t() %>% data.frame(stringsAsFactors=FALSE) %>% 
      set_colnames(stat_name_pr$stat_name) %>% rownames_to_column()
    # player
    df_listA <- paste0("df$service$boxscore$gamelineups$nba.g.", gameid_short, "$away_lineup_order$all")
    df_listB <- paste0("df$service$boxscore$gamelineups$nba.g.", gameid_short, "$home_lineup_order$all")
    boxA <- eval(parse(text=df_listA)) %>% data.frame(stringsAsFactors=FALSE) %>% set_colnames("rowname") %>% 
      left_join(boxpr0, "rowname") %>% sapply(as.character) %>% data.frame(stringsAsFactors=FALSE)
    boxB <- eval(parse(text=df_listB)) %>% data.frame(stringsAsFactors=FALSE) %>% set_colnames("rowname") %>% 
      left_join(boxpr0, "rowname") %>% sapply(as.character) %>% data.frame(stringsAsFactors=FALSE)
    boxpr <- rbind(boxA, boxB)
    oncourtA <- sapply(eval(parse(text=paste0("df$service$boxscore$gamelineups$nba.g.", gameid_short, "$away_lineup$all"))), "[[", 3)
    oncourtB <- sapply(eval(parse(text=paste0("df$service$boxscore$gamelineups$nba.g.", gameid_short, "$home_lineup$all"))), "[[", 3)
    oncourt <- c(oncourtA, oncourtB)
    oncourt <- ifelse(oncourt==1, "on-court", "")
    
    ### get html
    # htmlstr <- paste0('http://sports.yahoo.com/nba/', gameid) %>% read_html()
    
    gamedate <- str_split(gameid, "-") %>% unlist() %>% tail(1) %>% substr(1,8)
    # gamedate2 <- as.Date(gamedate, "%Y%m%d")
    season   <- ifelse(as.integer(substr(gamedate,5,6)) < 7,as.character(as.integer(substr(gamedate,1,4))-1),substr(gamedate,1,4))
    source("/home/chengil/R/fbasket/t_teamName2id.R")
  
    # team <- htmlstr %>%
    #   html_nodes(xpath = "//*[@id='Col1-0-Boxscore']/div[1]/div[3]/div/div/div/div/div[2]/div[1]/a/span") %>%
    #   html_text() %>% lapply(t_teamName2id) %>% unlist()

    team <- c(eval(parse(text=paste0("df$service$boxscore$games$nba.g.", gameid_short, "$away_team_id"))),
              eval(parse(text=paste0("df$service$boxscore$games$nba.g.", gameid_short, "$home_team_id")))) %>% lapply(t_teamName2id) %>% unlist()
    
    score <- c(eval(parse(text=paste0("df$service$boxscore$games$nba.g.", gameid_short, "$total_away_points"))),
               eval(parse(text=paste0("df$service$boxscore$games$nba.g.", gameid_short, "$total_home_points"))))
    
    oppo     <- c(paste0('@',team[2]),team[1])
    score_12 <- c(paste0(score[1], "-", score[2])) # for win/lose gamelog
    score_21 <- c(paste0(score[2], "-", score[1]))
    
    # livemark <- htmlstr %>%
    #   html_nodes(xpath = "//*[@id='Col1-0-Boxscore']/div[1]/div[3]/div/div/div[3]/div/div[1]/div") %>%
    #   html_text() %>% substr(1,5)
    
    # livemark <- ifelse(livemark != "Final", "LIVE!", "Final")
    livemark <- status_type
    livemark <- ifelse(livemark != "status.type.final", "LIVE!", "Final")
    
    ##### BoxScroeTM #####
    BoxScroeTM$team    <- team
    BoxScroeTM$season  <- ifelse(as.integer(substr(gamedate,5,6)) < 7,as.character(as.integer(substr(gamedate,1,4))-1),substr(gamedate,1,4))
    BoxScroeTM$gamedate<- gamedate
    BoxScroeTM$gameid  <- gameid
    BoxScroeTM$oppo    <- c(paste0("@", BoxScroeTM$team[2]), BoxScroeTM$team[1])
    
    if (type == 2){
      if (as.numeric(score[1])-as.numeric(score[2]) < 0){ # home win
        BoxScroeTM$score <- c(paste0("L ", score_12), paste0("W ", score_21))
      }else{
        BoxScroeTM$score <- c(paste0("W ", score_12), paste0("L ", score_21))
      }
    }else{
      BoxScroeTM$score <- c(score_12, score_12) # just for realtimebox
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
    BoxScroeTM$bxoreb <- as.integer(boxtm$ORPG)
    BoxScroeTM$bxdreb <- as.integer(boxtm$DRPG)
    BoxScroeTM$bxtreb <- as.integer(boxtm$RPG)
    BoxScroeTM$bxast  <- as.integer(boxtm$APG)
    BoxScroeTM$bxto   <- as.integer(boxtm$TOPG)
    BoxScroeTM$bxatr  <- as.integer(boxtm$APG)/as.integer(boxtm$TOPG)
    BoxScroeTM$bxst   <- as.integer(boxtm$SPG)
    BoxScroeTM$bxblk  <- as.integer(boxtm$BPG)
    BoxScroeTM$bxba   <- as.integer(boxtm$BA)
    BoxScroeTM$bxpf   <- as.integer(boxtm$PFPG)
    BoxScroeTM$bxpts  <- as.integer(boxtm$PPG)
    BoxScroeTM$bxcreb <- as.integer(boxtm$TReb)
    #TeamLog=[team season date gameid oppo score FGM FGA FGP 3PTM 3PTA 3PTP FTM FTA FTP OREB DREB TREB AST TO A/T ST BLK BA PF PTS CREB]
    #           1     2     3     4      5    6   7   8   9   10   11   12   13  14  15  16   17   18   19 20  21 22  23 24 25  26  27
    
    ##### BoxScroePR #####
    BoxScroePR$fbido <- c(eval(parse(text=df_listA)),
                          eval(parse(text=df_listB))) %>% str_split("\\.") %>% sapply("[[", 3)
    source('/home/chengil/R/fbasket/t_fbido2fbid.R')
    BoxScroePR$fbid <- lapply(BoxScroePR$fbido, t_fbido2fbid) %>% unlist()
    
    startfive1 <- rep("DNP", nrow(boxA))
    startfive1[boxA$Min != "NULL"] <- "BN"
    startfive1[1:5] <- c('F','F','C','G','G')
    startfive2 <- rep("DNP", nrow(boxB))
    startfive2[boxB$Min != "NULL"] <- "BN"
    startfive2[1:5] <- c('F','F','C','G','G')
    
    BoxScroePR$season    <- season
    BoxScroePR$gamedate  <- gamedate
    BoxScroePR$gameid    <- gameid
    BoxScroePR$team      <- rep(team,c(nrow(boxA),nrow(boxB)))
    BoxScroePR$oppo      <- rep(oppo,c(nrow(boxA),nrow(boxB)))
    BoxScroePR$score     <- c(rep(as.character(BoxScroeTM$score[1]), nrow(boxA)),rep(as.character(BoxScroeTM$score[2]), nrow(boxB)))
    BoxScroePR$startfive <- c(startfive1,startfive2)
    BoxScroePR$bxgs      <- rep(1,nrow(boxpr))
    BoxScroePR$bxgs[which(BoxScroePR$startfive == 'BN')] <- 0
    BoxScroePR$bxgs[which(BoxScroePR$startfive == 'DNP')] <- NA
    BoxScroePR$bxmin     <- sapply(unlist(boxpr$Min),
                               function(x){
                                 ifelse(nchar(as.character(x))<=5,
                                        as.integer(strsplit(as.character(x),':')[[1]][1])+as.integer(strsplit(as.character(x),':')[[1]][2])/60,NA)
                               }
    )
    BoxScroePR$bxfgm   <- sapply(boxpr$FG, parsePtg_n)
    BoxScroePR$bxfga   <- sapply(boxpr$FG, parsePtg_d)
    BoxScroePR$bxfgp   <- BoxScroePR$bxfgm/BoxScroePR$bxfga*100
    BoxScroePR$bx3ptm  <- sapply(boxpr$X3pt, parsePtg_n)
    BoxScroePR$bx3pta  <- sapply(boxpr$X3pt, parsePtg_d)
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
    BoxScroePR$bxporn  <- as.integer(as.character(boxpr$X...))
    BoxScroePR$livemark<- livemark
    BoxScroePR$oncourt <- oncourt
    
    repeat{
      source('/home/chengil/R/fbasket/f_dbconnect.R')
      if (is.null(con)==0){break}
    }
    cate9ms <- dbGetQuery(con, 'SELECT mean, std, percentage FROM cate9ms')
    cate9ms$mean <- cate9ms$mean*c(1,1,1,1,1,1,1,1,-1)
    dbDisconnect(con)
    
    adjfg <- (BoxScroePR$bxfgm/BoxScroePR$bxfga-cate9ms$percentage[1])*BoxScroePR$bxfga
    adjft <- (BoxScroePR$bxftm/BoxScroePR$bxfta-cate9ms$percentage[2])*BoxScroePR$bxfta
    boxcate9 <- cbind(adjfg,adjft,BoxScroePR$bx3ptm,BoxScroePR$bxpts,BoxScroePR$bxtreb,BoxScroePR$bxast,BoxScroePR$bxst,BoxScroePR$bxblk,-BoxScroePR$bxto)
    BoxScroePR$szv <- colSums((t(boxcate9)-cate9ms$mean)/cate9ms$std, na.rm=T)
    
    # print("progress")
    return(list(BoxScroeTM = data.frame(BoxScroeTM), BoxScroePR = data.frame(BoxScroePR)))
  }
}
