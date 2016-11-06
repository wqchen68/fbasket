f_rotoworld <- function(rwid){
  # rwid <- "1772"
  library(rvest)
  htmlstr <- read_html(paste0("http://www.rotoworld.com/player/nba/", rwid))
  
  pr_news <- NULL
  pr_news$rwid <- rwid
  pr_news$report <- htmlstr %>% 
    html_nodes(xpath = "//*[@class='playernews']//*[@class='report']") %>%
    html_text() %>% .[1]

  pr_news$impact <- htmlstr %>% 
    html_nodes(xpath = "//*[@class='playernews']//*[@class='impact']") %>%
    html_text() %>%  .[1]

  pr_news$date <- htmlstr %>% 
    html_nodes(xpath = "//*[@class='playernews']//*[@class='impact']//*[@class='date']") %>%
    html_text() %>% .[1]
  
  pr_news$info <- htmlstr %>% 
    html_nodes(xpath = "//*[@class='playernews']//*[@class='source']") %>%
    html_text() %>% .[1]
  
  return(data.frame(pr_news))

}