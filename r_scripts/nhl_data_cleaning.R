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
  df<-df[!is.na(df$G),]
  df$OT.Win<-apply(df,1,ot.tagger)
  scores<-do.call("rbind",apply(df,1,ot.fixer))
  df$G<-scores$G
  df$G.1<-scores$G.1
  df$Date<-as.Date(df$Date)
  colnames(df)<-c("Date", "AwayTeam","AG","HomeTeam","HG","OT.SO","OT.Win")
  return(df)
}

make.stats.table<-function(df) {
    stats<-data.frame("Team"=unique(df$AwayTeam),"GP"=rep(0),"W"=rep(0),"L"=rep(0),"OTL"=rep(0),"P"=rep(0),"ROW"=rep(0),"GF"=rep(0),"GA"=rep(0),"DIFF"=rep(0), "Win.Percent"=rep(0), "OT.SO.Appear"=rep(0), "OT.SO.Win"=rep(0), "OT.Win.Percent"=rep(0))
    for (i in 1:nrow(df)) {
        if (!is.na(df[i,"AG"]) | !is.na(df[i,"HG"])){
            if (df[i,"OT.Win"]=="V") {
                stats[stats$Team %in% df[i,"AwayTeam"],]$W<-stats[stats$Team %in% df[i,"AwayTeam"],]$W + 1
                stats[stats$Team %in% df[i,"HomeTeam"],]$OTL<-stats[stats$Team %in% df[i,"HomeTeam"],]$OTL + 1
                if (df[i,"OT.SO"]=="OT"){
                    stats[stats$Team %in% df[i,"AwayTeam"],]$ROW<-stats[stats$Team %in% df[i,"AwayTeam"],]$ROW + 1
                }
                stats[stats$Team %in% df[i,"AwayTeam"],]$OT.SO.Appear<-stats[stats$Team %in% df[i,"AwayTeam"],]$OT.SO.Appear + 1
                stats[stats$Team %in% df[i,"HomeTeam"],]$OT.SO.Appear<-stats[stats$Team %in% df[i,"HomeTeam"],]$OT.SO.Appear + 1
                stats[stats$Team %in% df[i,"AwayTeam"],]$OT.SO.Win<-stats[stats$Team %in% df[i,"AwayTeam"],]$OT.SO.Win + 1
            }  
            else if (df[i,"OT.Win"]=="H") {
                stats[stats$Team %in% df[i,"AwayTeam"],]$OTL<-stats[stats$Team %in% df[i,"AwayTeam"],]$OTL + 1
                stats[stats$Team %in% df[i,"HomeTeam"],]$W<-stats[stats$Team %in% df[i,"HomeTeam"],]$W + 1
                if (df[i,"OT.SO"]=="OT"){
                    stats[stats$Team %in% df[i,"HomeTeam"],]$ROW<-stats[stats$Team %in% df[i,"HomeTeam"],]$ROW + 1
                }
                stats[stats$Team %in% df[i,"AwayTeam"],]$OT.SO.Appear<-stats[stats$Team %in% df[i,"AwayTeam"],]$OT.SO.Appear + 1
                stats[stats$Team %in% df[i,"HomeTeam"],]$OT.SO.Appear<-stats[stats$Team %in% df[i,"HomeTeam"],]$OT.SO.Appear + 1
                stats[stats$Team %in% df[i,"HomeTeam"],]$OT.SO.Win<-stats[stats$Team %in% df[i,"HomeTeam"],]$OT.SO.Win + 1
            }
            else
                if (df[i,"AG"]>df[i,"HG"]) {
                    stats[stats$Team %in% df[i,"AwayTeam"],]$W<-stats[stats$Team %in% df[i,"AwayTeam"],]$W + 1
                    stats[stats$Team %in% df[i,"AwayTeam"],]$ROW<-stats[stats$Team %in% df[i,"AwayTeam"],]$ROW + 1
                    stats[stats$Team %in% df[i,"HomeTeam"],]$L<-stats[stats$Team %in% df[i,"HomeTeam"],]$L + 1
                }
                else {
                    stats[stats$Team %in% df[i,"AwayTeam"],]$L<-stats[stats$Team %in% df[i,"AwayTeam"],]$L + 1
                    stats[stats$Team %in% df[i,"HomeTeam"],]$ROW<-stats[stats$Team %in% df[i,"HomeTeam"],]$ROW + 1
                    stats[stats$Team %in% df[i,"HomeTeam"],]$W<-stats[stats$Team %in% df[i,"HomeTeam"],]$W + 1
                }
            
            stats[stats$Team %in% df[i,"AwayTeam"],]$GF<-stats[stats$Team %in% df[i,"AwayTeam"],]$GF + df[i,"AG"]
            stats[stats$Team %in% df[i,"AwayTeam"],]$GA<-stats[stats$Team %in% df[i,"AwayTeam"],]$GA + df[i,"HG"]
            stats[stats$Team %in% df[i,"HomeTeam"],]$GA<-stats[stats$Team %in% df[i,"HomeTeam"],]$GA + df[i,"AG"]
            stats[stats$Team %in% df[i,"HomeTeam"],]$GF<-stats[stats$Team %in% df[i,"HomeTeam"],]$GF + df[i,"HG"]
            stats[stats$Team %in% df[i,"AwayTeam"],]$GP<-stats[stats$Team %in% df[i,"AwayTeam"],]$GP + 1
            stats[stats$Team %in% df[i,"HomeTeam"],]$GP<-stats[stats$Team %in% df[i,"HomeTeam"],]$GP + 1
        }
    }
    stats$P<-2*stats$W+stats$OTL
    stats$DIFF<-stats$GF-stats$GA
    stats$Win.Percent<-stats$W/stats$GP
    stats$OT.Win.Percent<-stats$OT.SO.Win/stats$OT.SO.Appear
    
    stats<-stats[order(-stats$P, -stats$ROW, -stats$DIFF),]
    
    return(stats)
}