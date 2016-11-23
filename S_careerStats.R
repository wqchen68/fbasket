rm(list=ls())
library(dplyr)
library(RMySQL)
library(rvest)
library(stringr)

fbido <- '3331'
htmlstr <- read_html(paste0("http://sports.yahoo.com/nba/players/", fbido))

careerStats <- htmlstr %>%
  html_table(header=FALSE, fill=T) %>% .[[5]] %>% 
  filter(grepl("-", X1)) %>%
  'colnames<-'(c('cseason','cteam','cgame','acmin','acfgm','acfga','ac3ptm','ac3pta','acftm','acfta','acoreb','acdreb','actreb','acast','acto','acst','acblk','acpf','acpts')) %>%
  mutate(fbido=fbido)
