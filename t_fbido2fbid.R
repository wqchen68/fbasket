# Convert fbido (origin) to fbid (DB)
# Example:
# > t_fbido2fbid('3704')
# [1] "LeBron-James"
t_fbido2fbid = function(fbido_in){
  # fbido_in <- "5253"
  # fbido_in <- "900012"
  source('/home/chengil/R/fbasket/f_dbconnect.R')
  fbid_map <- dbGetQuery(con, "SELECT fbido, fbid FROM biodata")
  dbDisconnect(con)
  
  fbid <- fbid_map %>%
    filter(fbido==fbido_in) %>%
    select(fbid)
  
  ifelse(length(fbid$fbid)==0, "ERROR", fbid) %>% as.character()
  
}


