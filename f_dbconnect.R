# connect to db
library(RMySQL)
con <- dbConnect(MySQL(),user = 'wqchen',password = '289391',host = 'localhost',dbname='fbasket')
