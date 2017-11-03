f_getbiodata = function(fbido){
  # library(XML)
  # library(RCurl)
  library(RMySQL)
  library(rvest)
  bio_data <- NULL
  # fbido <- '4176' #normal
  # fbido <- '1250', '4176' #no number
  # fbido <- '2874' #no nmber, position
  # fbido <- '5852'

  # sethtml  <- readLines(paste0("https://sports.yahoo.com/nba/players/", fbido),warn=F,encoding = "UTF-8")
  # pagetree <- htmlTreeParse(sethtml, useInternalNodes = TRUE, encoding='UTF-8')
  # bio_data$fbido  <- fbido
  # bio_data$player <- xpathSApply(pagetree,'//*[@id="Col1-0-Player-Proxy"]/div/div[1]/div[2]/h1/span',xmlValue)
  # source('/home/chengil/R/fbasket/t_player2fbid.R')
  # bio_data$fbid <- t_player2fbid(bio_data$player)
  # numpos <- xpathSApply(pagetree,'//*[@id="Col1-0-Player-Proxy"]/div/div[1]/div[2]/div/div[1]/div',xmlValue) %>% strsplit(",") %>% unlist()
  # if (length(numpos)==3){
  #   bio_data$number   <- numpos[1] %>% strsplit("#") %>% unlist() %>% tail(1)
  #   bio_data$position <- numpos[2] %>% trimws("left")
  # }else if(length(numpos)==2){
  #   bio_data$number   <- NULL
  #   bio_data$position <- numpos[1] %>% strsplit("#") %>% unlist() %>% tail(1)
  # }else{
  #   bio_data$number   <- NULL
  #   bio_data$position <- NULL
  # }
  # fbidnp <- as.data.frame(bio_data, stringsAsFactors = F)

  # new version: use rvest
  bio_data$fbido  <- fbido
  
  setUrl <- paste0('https://sports.yahoo.com/nba/players/', fbido)
  html_str <- setUrl %>% read_html()

  bio_data$player <- html_str %>%
    html_nodes(xpath = "//*[@class='Fw(b) Mbot(4px) Fz(30px)']") %>% 
    html_text()
  
  source('/home/chengil/R/fbasket/t_player2fbid.R')
  bio_data$fbid <- t_player2fbid(bio_data$player)
    
  numpos <- html_str %>%
    html_nodes(xpath = "//*[@class='Row Mb(15px) Fz(14px)']") %>% 
    html_text() %>%
    strsplit(",") %>% 
    unlist()
  
  if (length(numpos)==3){
    bio_data$number   <- numpos[1] %>% strsplit("#") %>% unlist() %>% tail(1)
    bio_data$position <- substring(numpos[2], 2) #there is a freaky string front position code
  }else if(length(numpos)==2){ #just only (position, team)
    bio_data$number   <- NULL
    bio_data$position <- numpos[1] #there is no freaky string
  }else{
    bio_data$number   <- NULL
    bio_data$position <- NULL
  }
  fbidnp <- as.data.frame(bio_data, stringsAsFactors = F)
  
  hwbcd <- html_str %>%
    html_nodes(xpath = "//*[@class='Fz(13px) Lh(1.8) Maw(600px)']//*[@class='IbBox Pend(40px) Miw(35%)']") %>% 
    html_text() %>%
    strsplit(., split=": ") %>%
    sapply(.,function(x){
      df <- as.data.frame(t(x[2]), stringsAsFactors = F)
      names(df) <- x[1]
      return(df)
    })
  hwbcd <- as.data.frame(hwbcd, stringsAsFactors = F)
  colnames(hwbcd)[which(colnames(hwbcd) == "Height")] <- 'height'
  colnames(hwbcd)[which(colnames(hwbcd) == "Weight")] <- 'weight'
  colnames(hwbcd)[which(colnames(hwbcd) == "Born")] <- 'bornD'
  colnames(hwbcd)[which(colnames(hwbcd) == "Birth.Place")]  <- 'bplace'
  colnames(hwbcd)[which(colnames(hwbcd) == "College")]  <- 'college'
  colnames(hwbcd)[which(colnames(hwbcd) == "Draft")]  <- 'draft'

  # bio_data$height  <- xpathSApply(pagetree,'//*[@id="Col1-0-Player-Proxy"]/div/div[1]/div[2]/div/div[3]/div[1]/span[2]/span',xmlValue)
  # bio_data$weight  <- xpathSApply(pagetree,'//*[@id="Col1-0-Player-Proxy"]/div/div[1]/div[2]/div/div[3]/div[2]/span[2]/span',xmlValue)
  # bio_data$bornD   <- xpathSApply(pagetree,'//*[@id="Col1-0-Player-Proxy"]/div/div[1]/div[2]/div/div[3]/div[3]/span[2]/span',xmlValue)
  # bio_data$bplace  <- xpathSApply(pagetree,'//*[@id="Col1-0-Player-Proxy"]/div/div[1]/div[2]/div/div[3]/div[5]/span[2]',xmlValue)
  # bio_data$college <- xpathSApply(pagetree,'//*[@id="Col1-0-Player-Proxy"]/div/div[1]/div[2]/div/div[3]/div[4]/span[2]',xmlValue)
  # draft <- xpathSApply(pagetree,'//*[@id="Col1-0-Player-Proxy"]/div/div[1]/div[2]/div/div[3]/div[6]/span[2]/span',xmlValue)
  if (hwbcd$draft == "Undrafted") {
    hwbcd$draftY <- NA
    hwbcd$draftR <- NA
    hwbcd$draftP <- NA
    hwbcd$draftT <- NA
  }else{
    drafttime <- strsplit(hwbcd$draft, " by the ") %>% unlist %>% head(1) %>% strsplit(" ") %>% unlist
      hwbcd$draftY <- drafttime[1]
      hwbcd$draftR <- drafttime[2] %>% substr(1,1)
      hwbcd$draftP <- stringr::str_sub(drafttime[4], 2, nchar(drafttime[4])-2)
      hwbcd$draftT <- strsplit(hwbcd$draft, " by the ") %>% unlist %>% tail(1)
  }
  hwbcd <- hwbcd[ , names(hwbcd)!='draft']
  hwbcd$fbido2  <- as.numeric(fbido)
  
  biodata <- cbind(fbidnp, hwbcd)
  # biodata <- data.frame(bio_data)
  
  # source("/home/chengil/R/fbasket/f_dbconnect.R")
  # dbWriteTable(con, 'biodata', biodata, append = T, row.names = F, allow.keywords = T)
  # dbDisconnect(con)
  return(biodata)
}

# BioData=[fbido player number fbid position height weight bornD bplace college draftY draftR draftP draftT]
#            1      2      3     4      5      6      7      8      9     10      11     12     13     14
