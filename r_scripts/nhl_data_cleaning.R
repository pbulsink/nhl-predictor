# This cleans nhl data for use in the modified Opisthonka 
# Dixon-Coles predicting by removing SO/OT scores.

_ot.tagger<-function(row){
  if (row$X != ''){
    if(row$G>row$G.1){
      return("V")}
    else{
      return("H")}
  }
  else{
    return("")
  }
}

_ot.fixer<-function(row){
  H<-as.integer(df['G.1'])
  V<-as.integer(df['G'])
  if(df['OT.Win']=='H'){
    H<-H-1
  }
  else if (df['OT.Win']=='V'){
    V<-V-1
  }
  return(data.frame('G.1'=H,'G'=V))
}

nhl.data.prep<-function(df){
  df<-df[,c('Date','Visitor','G','Home','G.1','X')]
  df$OT.Win<-apply(df,1,_ot.tagger)
  scores<-do.call("rbind",apply(df,1,_ot.fixer))
  df$G<-scores$G
  df$G.1<-scores$G.1
  df$Date<-as.Date(df$Date)
  colnames(df)<-c("Date", "AwayTeam","AG","HomeTeam","HG","OT.SO","OT.Win")
  return df
}
