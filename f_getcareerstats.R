f_getcareerstats <- function(fbido){
  
  library(dplyr)
  library(RMySQL)
  library(rvest)
  library(stringr)
  
  # fbido <- '113'
  # fbido <- '3331'
  # fbido <- '3705'
  # fbido <- '3248'
  htmlstr <- read_html(paste0("http://sports.yahoo.com/nba/players/", fbido))
  
  source('/home/chengil/R/fbasket/t_fbido2fbid.R')
  fbid = t_fbido2fbid(fbido)
  
  parse_time = function(x){
    x = strsplit(x, ":")
    unlist(lapply(x, function(y){(as.numeric(y[1])*60+as.numeric(y[2]))/60}))
  }
  
  careerStats0 <- htmlstr %>%
    html_table(header=FALSE, fill=T) %>%
    tail(1) %>%
    .[[1]] %>%
    filter(grepl("-", X1)) %>%
    'colnames<-'(c('cseason','cteam','cgame','acmin',
                   'acfgm','acfga','acfgp',
                   'ac3ptm','ac3pta','ac3ptp',
                   'acftm','acfta','acftp',
                   'acoreb','acdreb','actreb','acast','acto','acst','acblk','acpf','acpts')) %>%
    select(-acfgp, -ac3ptp, -acftp) %>%
    mutate(fbido  = fbido, fbid = fbid,
           cgame  = as.numeric(gsub(',','',cgame)),  acmin  = parse_time(gsub(',','',acmin)),
           acfgm  = as.numeric(gsub(',','',acfgm)),  acfga  = as.numeric(gsub(',','',acfga)),
           ac3ptm = as.numeric(gsub(',','',ac3ptm)), ac3pta = as.numeric(gsub(',','',ac3pta)),
           acftm  = as.numeric(gsub(',','',acftm)),  acfta  = as.numeric(gsub(',','',acfta)),
           acoreb = as.numeric(gsub(',','',acoreb)), acdreb = as.numeric(gsub(',','',acdreb)), actreb = as.numeric(gsub(',','',actreb)),
           acast  = as.numeric(gsub(',','',acast)),  acto   = as.numeric(gsub(',','',acto)),
           acst   = as.numeric(gsub(',','',acst)),   acblk  = as.numeric(gsub(',','',acblk)),  acpf   = as.numeric(gsub(',','',acpf)),
           acpts  = as.numeric(gsub(',','',acpts))) %>%
    mutate(cmin  = acmin/cgame,
           cfgm  = acfgm/cgame,  cfga  = acfga/cgame,  cfgp  = acfgm/acfga,
           c3ptm = ac3ptm/cgame, c3pta = ac3pta/cgame, c3ptp = ac3ptm/ac3pta,
           cftm  = acftm/cgame,  cfta  = acfta/cgame,  cftp  = acftm/acfta,
           coreb = acoreb/cgame, cdreb = acdreb/cgame, ctreb = actreb/cgame,
           cast  = acast/cgame,  cto   = acto/cgame, 
           cst   = acst/cgame,   cblk  = acblk/cgame,  cpf   = acpf/cgame,
           cpts  = acpts/cgame,  catr  = acast/acto) %>%
    mutate(ceff  = cpts + ctreb + cast + cst + cblk - (cfga-cfgm) - (cfta-cftm) - cto) %>%
    mutate(ceff36 = ceff/cmin*36)
  
  new_order <- 1:nrow(careerStats0)
  dataorder <- 1:nrow(careerStats0)
  if (nrow(careerStats0) > 2){ # 至少要 3 rows，因為下面有個條件會檢查前兩年、有個條件會檢查後一年
    for (i in 2:(nrow(careerStats0))){
      # print(i)
      if (careerStats0$cseason[i] == careerStats0$cseason[i-1]){ # 年份相同

        if (i==2){ # 檢查第二列
          if (careerStats0$cteam[i-1] == careerStats0$cteam[i+1]){ # 只能一個條件
            new_order[i] <- dataorder[i-1]
            new_order[i-1] <- dataorder[i]
          }
        }else if (i == nrow(careerStats0)) { # 檢查最後一列
          if (careerStats0$cteam[i] == careerStats0$cteam[i-2]){ # 只能一個條件 case: "3727"
            new_order[i] <- dataorder[i-1]
            new_order[i-1] <- dataorder[i]
          }
        }else{
          if (careerStats0$cteam[i-1] == careerStats0$cteam[i+1] | careerStats0$cteam[i] == careerStats0$cteam[i-2]){ # 兩個條件
            new_order[i] <- dataorder[i-1]
            new_order[i-1] <- dataorder[i]
          }          
        }
        
      }
    }
  }
  careerStats <- careerStats0[new_order,] %>%
    mutate(dataorder = 1:length(cseason))
  
  # dont use loop, but its always has bug.
  # careerStats1 <- careerStats0[order(careerStats0$cseason, factor(careerStats0$cteam, levels = unique(careerStats0$cteam))),]
  # careerStats <- careerStats1[order(careerStats1$cseason, factor(careerStats1$cteam, levels = rev(unique(rev(careerStats1$cteam))))),]
  
  add_prime <- function(x){
    rle_len <- rle(x)[1]$lengths
    rle_val <- rle(x)[2]$values
    dup_tf <- duplicated(rle_val)
    return(rep(ifelse(dup_tf==1, paste0(rle_val, "'"), rle_val), rle_len))
  }
  
  careerStats$cteam <- careerStats$cteam %>% 
    add_prime() %>% 
    add_prime()

  careerStats <- careerStats %>%
    filter(cseason %in% c('2015-16', '2016-17'))
    
  source("/home/chengil/R/fbasket/f_dbconnect.R")
  dbWriteTable(con, 'careerstats_copy', careerStats, append = T, row.names = F, allow.keywords = T)
  dbDisconnect(con)
  
  # return(careerStats)
  
}



# bb <- group_by(careerStats) %>%
#   mutate(aa = diff(as.numeric(careerStats$cgame)))
