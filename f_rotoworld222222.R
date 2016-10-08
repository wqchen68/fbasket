rm(list=ls())
library(rvest)
htmlstr <- read_html('http://www.rotoworld.com/teams/depth-charts/nba.aspx')

pr_name <- htmlstr %>% 
  html_nodes(xpath = "//*[@id='cp1_tblDepthCharts']//tr//td//tr//td/a") %>%
  html_text()

pr_id <- htmlstr %>% 
  html_nodes(xpath = "//*[@id='cp1_tblDepthCharts']//tr//td//tr//td/a") %>%
  html_attr("href") %>%
  str_split("/") %>% 
  lapply(function(x) x[4]) %>% unlist()

