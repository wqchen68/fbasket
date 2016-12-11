# Convert player name (biodata) to fbid (DB)
# Example:
# > t_player2fbid("LeBron James")
# [1] "LeBron-James"
# > t_player2fbid("J.J. Redick")
# [1] "JJ-Redick"
t_player2fbid = function(player){
  fbid <- gsub('\\.','',gsub('\'','',gsub(' ','-',player)))
  return(fbid)
}


