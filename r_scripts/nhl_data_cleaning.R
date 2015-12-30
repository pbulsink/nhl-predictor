# This cleans nhl data for use in the modified Opisthonka 
# Dixon-Coles predicting by removing SO/OT scores.

ot.tagger<-function(row) {
  if (row['X'] != '') {
    if(row['G']>row['G.1']) {
      return("V")
    }
    else{
      return("H")
    }
  }
  else {
    return("")
  }
}

ot.fixer<-function(row) {
  H<-as.integer(row['G.1'])
  V<-as.integer(row['G'])
  if(row['OT.Win']=='H') {
    H<-H-1
  }
  else if (row['OT.Win']=='V') {
    V<-V-1
  }
  return(data.frame('G.1'=H,'G'=V))
}

nhl.data.prep<-function(df) {
  df<-df[,c('Date','Visitor','G','Home','G.1','X')]
  df$OT.Win<-apply(df,1,ot.tagger)
  scores<-do.call("rbind",apply(df,1,ot.fixer))
  df$G<-scores$G
  df$G.1<-scores$G.1
  df$Date<-as.Date(df$Date)
  colnames(df)<-c("Date", "AwayTeam","AG","HomeTeam","HG","OT.SO","OT.Win")
  return(df)
}

OT.Stats<-function(df) {
    stats<-data.frame("Team"=unique(clean.data$AwayTeam), "Appearances"=rep(0), "Wins"=rep(0))
    for (i in 1:nrow(df)) {
        if (df[i,"OT.Win"]=="V") {
            stats[stats$Team %in% df[i,"AwayTeam"],]$Appearances<-stats[stats$Team %in% df[i,"AwayTeam"],]$Appearances + 1
            stats[stats$Team %in% df[i,"HomeTeam"],]$Appearances<-stats[stats$Team %in% df[i,"HomeTeam"],]$Appearances + 1
            stats[stats$Team %in% df[i,"AwayTeam"],]$Wins<-stats[stats$Team %in% df[i,"AwayTeam"],]$Wins + 1
        }  
        else if (df[i,"OT.Win"]=="H") {
            stats[stats$Team %in% df[i,"AwayTeam"],]$Appearances<-stats[stats$Team %in% df[i,"AwayTeam"],]$Appearances + 1
            stats[stats$Team %in% df[i,"HomeTeam"],]$Appearances<-stats[stats$Team %in% df[i,"HomeTeam"],]$Appearances + 1
            stats[stats$Team %in% df[i,"HomeTeam"],]$Wins<-stats[stats$Team %in% df[i,"HomeTeam"],]$Wins + 1
        }
    }
    stats$Win.Percent<-stats$Wins/stats$Appearances
    return(stats)
}