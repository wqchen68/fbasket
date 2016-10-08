f_gamelog2dataframe = function(datarange){
  
  library(dplyr)
  # datarange <- "D07"
  days <- switch(datarange,
         "ALL" = 365,
         "D30" = 30,
         "D14" = 14,
         "D07" = 7)
  
  source('/home/chengil/R/fansboard/f_dbconnect.R')
  gamelog <- dbGetQuery(con, paste0("SELECT * FROM allgamelog
                         WHERE season='2015' AND bxgs IS NOT NULL
                         AND TO_DAYS( NOW( ) ) - TO_DAYS( gdate ) <=",days))
  # gamelog <- dbGetQuery(con, paste0("SELECT * FROM allgamelog 
  #                        WHERE season='2015' AND bxgs IS NOT NULL 
  #                        AND gdate <= '2016-03-01'"))  
  dbDisconnect(con)

  # source('/home/chengil/R/fansboard/f_dbconnect.R')
  # syncdataframe <- dbGetQuery(con, paste0("SELECT * FROM syncdataframe where datarange='D30'"))
  # dbDisconnect(con)
    
  df_basic <- group_by(gamelog, fbido) %>% 
    summarise(datarange = unique(datarange),
      fbid  = unique(fbid),
      wgp   = length(bxgs),
      wgs   = sum(bxgs),
      wtech = NA, pwtech= NA, zwtech= NA, swtech= NA, 
      wdisq = NA, wejct = NA, wff = NA
    )
  ###### w
  df_w_o <- group_by(gamelog, fbido) %>%
    summarise(wmin = sum(bxmin),
              wfgm = sum(bxfgm),  wfga = sum(bxfga),
              w3ptm= sum(bx3ptm), w3pta= sum(bx3pta),
              wftm = sum(bxftm),  wfta = sum(bxfta),
              woreb= sum(bxoreb), wdreb= sum(bxdreb), wtreb= sum(bxoreb)+sum(bxdreb),
              wast = sum(bxast),  wto  = sum(bxto),   
              wst  = sum(bxst),   wblk = sum(bxblk),  wpf  = sum(bxpf), wpts = sum(bxpts)
    )
  df_w_r <- group_by(gamelog, fbido) %>% 
    summarise(
      wfgp = ifelse(sum(bxfga)==0, 0, sum(bxfgm)/sum(bxfga)),
      w3ptp= ifelse(sum(bx3pta)==0, 0, sum(bx3ptm)/sum(bx3pta)),
      wftp = ifelse(sum(bxfta)==0, 0, sum(bxftm)/sum(bxfta)),
      watr = ifelse(sum(bxto)==0, 0, sum(bxast)/sum(bxto))
    )
  ###### pw 
  df_pw_o <- group_by(gamelog, fbido) %>%
    summarise(pwmin = mean(bxmin),
              pwfgm = mean(bxfgm),  pwfga = mean(bxfga),
              pw3ptm= mean(bx3ptm), pw3pta= mean(bx3pta),
              pwftm = mean(bxftm),  pwfta = mean(bxfta),
              pworeb= mean(bxoreb), pwdreb= mean(bxdreb), pwtreb= mean(bxoreb+bxdreb),
              pwast = mean(bxast),  pwto  = mean(bxto),
              pwst  = mean(bxst),   pwblk = mean(bxblk),  pwpf  = mean(bxpf), pwpts = mean(bxpts)
    )
  
  allfgp  <- sum(gamelog$bxfgm)/sum(gamelog$bxfga)
  all3ptp <- sum(gamelog$bx3ptm)/sum(gamelog$bx3pta)
  allftp  <- sum(gamelog$bxftm)/sum(gamelog$bxfta)
  allatr  <- sum(gamelog$bxast)/sum(gamelog$bxto)
  df_pw_r <- group_by(gamelog, fbido) %>% 
    summarise(
      pwfgp = (ifelse(sum(bxfga)==0, 0, sum(bxfgm)/sum(bxfga)) - allfgp) * ifelse(sum(bxfga)==0, 0, mean(bxfga)),
      pw3ptp= (ifelse(sum(bx3pta)==0, 0, sum(bx3ptm)/sum(bx3pta)) - all3ptp) * ifelse(sum(bx3pta)==0, 0, mean(bx3pta)),
      pwftp = (ifelse(sum(bxfta)==0, 0, sum(bxftm)/sum(bxfta)) - allftp) * ifelse(sum(bxfta)==0, 0, mean(bxfta)),
      pwatr = (ifelse(sum(bxto)==0, 0, sum(bxast)/sum(bxto)) - allatr) * ifelse(sum(bxto)==0, 0, mean(bxto))
    )
  ###### zw
  df_zw_o <- subset(df_pw_o, select = -fbido)
  df_zw_o$pwto <- -df_zw_o$pwto
  df_zw_o$pwpf <- -df_zw_o$pwpf
  df_zw_o <- data.frame(scale(df_zw_o))
  colnames(df_zw_o) <- sub("pw", "zw", colnames(df_pw_o)[-1])
  
  df_zw_r <- subset(df_pw_r, select = -fbido)
  df_zw_r <- data.frame(scale(df_zw_r))
  colnames(df_zw_r) <- sub("pw", "zw", colnames(df_pw_r)[-1])
  ###### sw
  df_sw_o <- lapply(df_zw_o, function(x){(x - min(x))/max(x - min(x))*5}) %>% data.frame()
  colnames(df_sw_o) <- sub("pw", "sw", colnames(df_pw_o)[-1])
  df_sw_r <- lapply(df_zw_r, function(x){(x - min(x))/max(x - min(x))*5}) %>% data.frame()
  colnames(df_sw_r) <- sub("pw", "sw", colnames(df_pw_r)[-1])
  
  df_all <- cbind(merge.data.frame(df_basic, 
                  merge.data.frame(merge.data.frame(df_w_o, df_pw_o), merge.data.frame(df_w_r, df_pw_r))), 
                  df_zw_o, df_zw_r, df_sw_o, df_sw_r)
  
  df_all$pweff <- (df_all$pwpts + df_all$pwtreb + df_all$pwast + df_all$pwst + df_all$pwblk) - ((df_all$pwfga - df_all$pwfgm) + (df_all$pwfta-df_all$pwftm) + df_all$pwto)
  df_all$pweff36 <- df_all$pweff * (36/df_all$pwmin)
  
  r_d <- df_all$swfga  + df_all$swfta
  g_d <- df_all$swast  + df_all$swst
  b_d <- df_all$swtreb + df_all$swblk
  rgb_d <- r_d + g_d + b_d
  df_all$rr <- r_d/rgb_d
  df_all$gg <- g_d/rgb_d
  df_all$bb <- b_d/rgb_d
  
  source("/home/chengil/R/fansboard/f_dbconnect.R")
  dbGetQuery(con, paste0("DELETE FROM syncdataframe WHERE datarange='", datarange, "'"))
  dbWriteTable(con, 'syncdataframe', df_all, append = T, row.names = F, allow.keywords = T)
  dbDisconnect(con)

}







