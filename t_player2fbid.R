t_player2fbid = function(player){
  fbid <- gsub('\\.','',gsub('\'','',gsub(' ','-',player)))
  return(fbid)
}


