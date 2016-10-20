rm(list=ls())
library(dplyr)
library(lubridate)
library(jsonlite)
gamedate <- Sys.Date()-1
html_list <- fromJSON(paste0("https://api-secure.sports.yahoo.com/v1/editorial/s/scoreboard?lang=en-US&region=US&tz=America%2FLos_Angeles&ysp_redesign=1&leagues=nba&date=", gamedate, "&v=2&ysp_enable_last_update=1"))
Sys.setlocale("LC_TIME", "C")
gametime <- sapply(html_list$service$scoreboard$games, function(x) x$start_time[[1]]) %>%
  strptime("%a, %d %b %Y %H:%M:%S %z") %>% min()

hours <- hour(gametime)
mins  <- minute(gametime)
hours <- ifelse(hours==12,0,hours)

fileConn <- file("/home/chengil/R/fbasket/update_cron.txt")
writeLines(paste0(toString(mins)," ",toString(hours)," ","* * * Rscript /home/chengil/R/fbasket/realtimebox.R &> /home/chengil/R/fbasket/realtimebox.log"), fileConn)
close(fileConn)

system("crontab /home/chengil/R/fbasket/update_cron.txt")


