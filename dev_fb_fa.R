rm(list=ls())
library(dplyr)
library(data.table)
library(stringr)
library(rvest)

f_getfbdata <- function(page, league_id){
  # page <- 0
  htmlstr <- read_html(paste0("http://basketball.fantasysports.yahoo.com/nba/", league_id, "/players?status=A&pos=P&cut_type=33&stat1=S_SDL14&myteam=0&sort=OR&sdir=1&count=", toString(page)))
  # htmlstr <- read_html(paste0("http://basketball.fantasysports.yahoo.com/nba/5547/players?status=A&pos=P&cut_type=33&stat1=S_SDL14&myteam=0&sort=OR&sdir=1&count=50"))

  pr_name <- htmlstr %>%
    html_nodes(xpath = "//*[@id='players-table']/div[2]/table/tbody/tr/td[2]/div/div/div[1]/div/a") %>%
    html_text()
  
  pr_tm_pos <- htmlstr %>%
    html_nodes(xpath = "//*[@id='players-table']/div[2]/table/tbody/tr/td[2]/div/div/div[1]/div/span") %>%
    html_text()
  
  pr_data <- NULL
  if (length(pr_name)!=0){
    
    htmltable <- htmlstr %>% html_table(header = FALSE)
    df_o <- htmltable[[2]]
    # df <- df_o[3:27,10:20]
    df <- df_o[3:(length(pr_name)+2), 10:(ncol(df_o)-2)]
    
    colnames(df) <- switch(
      league_id,
      '2827'  = c('FGMA', 'FG', 'FTMA', 'FT', '3PTA', 'OREB', 'DREB', 'ST', 'BLK', 'ATR', 'TECH'),
      '5547'  = c('FGMA', 'FG', 'FTMA', 'FT', '3PTM', 'PTS', 'REB', 'AST', 'ST', 'BLK', 'TO')
    )
    
    df_f <- tbl_df(df) %>% select(-FGMA, -FTMA) %>% sapply(as.numeric) %>% as.data.frame()
    
    pr_data <- cbind(pr_name, pr_tm_pos, df_f)
    return(pr_data)
  }else{
    return(pr_data)
  }
}



f_getfbdata_pr <- function(l_id, cates){
  # l_id <- '2827'
  # cates <- c('OREB','DREB')
  cates_sum_str = paste(paste('all_pr_data$', cates),sep="+")
  
  page <- seq(0,500,25) # depend on deep of your league
  all_pr_data <- lapply(page, f_getfbdata, league_id=l_id) %>% bind_rows()
  
  all_pr_data_score <- mutate(all_pr_data, score = eval(parse(text=cates_sum_str))) %>%
    arrange(desc(score)) %>% head(10)
  
  return(all_pr_data_score)
}

# Load the function above, and Execute the script below
l_id <- '2827'
cates <- c('OREB','DREB','ST')
# please refer to ...
# '2827'  = c('FG', 'FT', '3PTA', 'OREB', 'DREB', 'ST', 'BLK', 'ATR', 'TECH')
# '5547'  = c('FG', 'FT', '3PTM', 'PTS', 'REB', 'AST', 'ST', 'BLK', 'TO')

f_getfbdata_pr(l_id, cates)















