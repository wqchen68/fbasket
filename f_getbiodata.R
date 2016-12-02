f_getbiodata = function(fbido){
  library(XML)
  library(RCurl)
  library(RMySQL)
  bio_data <- NULL
  # fbido <- '3704' #normal
  # fbido <- '1250' #no number
  # fbido <- '2874' #no nmber, position
  sethtml  <- readLines(paste0("http://sports.yahoo.com/nba/players/", fbido),warn=F,encoding = "UTF-8")
  pagetree <- htmlTreeParse(sethtml, useInternalNodes = TRUE, encoding='UTF-8')
  bio_data$fbido  <- fbido
  bio_data$player <- xpathSApply(pagetree,'//div[@class="player-info"]/h1',xmlValue)
  source('/home/chengil/R/fbasket/t_player2fbid.R')
  bio_data$fbid <- t_player2fbid(bio_data$player)
  numpos <- xpathSApply(pagetree,'//span[@class="team-info"]',xmlValue) %>% strsplit(",") %>% unlist()
  if (length(numpos)==3){
    bio_data$number   <- numpos[1] %>% strsplit("#") %>% unlist() %>% tail(1)
    bio_data$position <- numpos[2] %>% trimws("left")
  }else if(length(numpos)==2){
    bio_data$number   <- NULL
    bio_data$position <- numpos[1] %>% strsplit("#") %>% unlist() %>% tail(1)
  }else{
    bio_data$number   <- NULL
    bio_data$position <- NULL
  }
  bio_data$height  <- xpathSApply(pagetree,'//li[@class="height"]//dd',xmlValue)
  bio_data$weight  <- xpathSApply(pagetree,'//li[@class="weight"]//dd',xmlValue)
  bio_data$bornD   <- xpathSApply(pagetree,'//li[@class="born"]//dd',xmlValue)
  bio_data$bplace  <- xpathSApply(pagetree,'//li[@class="birthplace"]//dd',xmlValue)
  bio_data$college <- xpathSApply(pagetree,'//li[@class="college"]//dd',xmlValue)
  draft <- xpathSApply(pagetree,'//li[@class="draft"]//dd',xmlValue)
  if (draft == "Undrafted") {
    bio_data$draftY <- NA
    bio_data$draftR <- NA
    bio_data$draftP <- NA
    bio_data$draftT <- NA
  }else{
    drafttime <- strsplit(draft, " by the ") %>% unlist %>% head(1) %>% strsplit(" ") %>% unlist
      bio_data$draftY <- drafttime[1]
      bio_data$draftR <- drafttime[2] %>% substr(1,1)
      bio_data$draftP <- stringr::str_sub(drafttime[4], 2, nchar(drafttime[4])-2)
    bio_data$draftT <- strsplit(draft, " by the ") %>% unlist %>% tail(1)
  }
  bio_data$fbido2  <- as.numeric(fbido)
  # return(data.frame(bio_data))
  
  biodata <- data.frame(bio_data)
  source("/home/chengil/R/fbasket/f_dbconnect.R")
  dbWriteTable(con, 'biodata_copy', biodata, append = T, row.names = F, allow.keywords = T)
  dbDisconnect(con)
  
}

# BioData=[fbido player number fbid position height weight bornD bplace college draftY draftR draftP draftT]
#            1      2      3     4      5      6      7      8      9     10      11     12     13     14
