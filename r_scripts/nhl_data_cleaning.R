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
  colnames(df)<-c("Date", "AwayTeam","AG","HomeTeam","HG","OT.SO")
  df<-df[!is.na(df$AG),]
  df$OT.Win<-apply(df,1,ot.tagger)
  scores<-do.call("rbind",apply(df,1,ot.fixer))
  df$AG<-scores$AG
  df$HG<-scores$HG
  df$Date<-as.Date(df$Date)
  return(df)
}

nhl.to.play<-function(df){
    df<-df[,c('Date','Visitor','G','Home','G.1')]
    df$Date<-as.Date(df$Date)
    df<-df[!(df$Date < Sys.Date()),]
    df$OT.SO<-""
    df$OT.Win<-""
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
                stats[stats$Team %in% df[i,"AwayTeam"],]$GF<-stats[stats$Team %in% df[i,"AwayTeam"],]$GF + 1
                stats[stats$Team %in% df[i,"HomeTeam"],]$GA<-stats[stats$Team %in% df[i,"HomeTeam"],]$GA + 1
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
                stats[stats$Team %in% df[i,"HomeTeam"],]$GF<-stats[stats$Team %in% df[i,"HomeTeam"],]$GF + 1
                stats[stats$Team %in% df[i,"AwayTeam"],]$GA<-stats[stats$Team %in% df[i,"AwayTeam"],]$GA + 1
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

standings_table<-function(df){
    #at the limit of optimization it's still 3.some seconds per season. 
    #try melting, working, then df calling?
    stats<-data.frame("Team"=unique(df$AwayTeam),"GP"=rep(0),"W"=rep(0),"L"=rep(0),"OTL"=rep(0),"P"=rep(0),"ROW"=rep(0),"DIFF"=rep(0), "SOW"=rep(0), "PP"=rep(0))
    for (i in 1:nrow(df)) {
        hometeam<-as.character(df[i,"HomeTeam"])
        awayteam<-as.character(df[i,"AwayTeam"])
        homegoals<-df[i,"HG"]
        awaygoals<-df[i,"AG"]
        if (df[i,"OT.Win"]=="V") {
            stats[stats$Team %in% awayteam,]$W<-stats[stats$Team %in% awayteam,]$W + 1
            stats[stats$Team %in% hometeam,]$OTL<-stats[stats$Team %in% hometeam,]$OTL + 1
            stats[stats$Team %in% awayteam,]$DIFF<-stats[stats$Team %in% awayteam,]$DIFF + 1
            if (df[i,"OT.SO"]=="SO"){
                stats[stats$Team %in% awayteam,]$SOW<-stats[stats$Team %in% awayteam,]$SOW + 1
            }
        }  
        else if (df[i,"OT.Win"]=="H") {
            stats[stats$Team == awayteam,]$OTL<-stats[stats$Team == awayteam,]$OTL + 1
            stats[stats$Team %in% hometeam,]$W<-stats[stats$Team %in% hometeam,]$W + 1
            stats[stats$Team %in% hometeam,]$DIFF<-stats[stats$Team %in% hometeam,]$DIFF + 1
            if (df[i,"OT.SO"]=="OT"){
                stats[stats$Team %in% hometeam,]$SOW<-stats[stats$Team %in% hometeam,]$SOW + 1
            }
        }
        else{
            if (awaygoals>homegoals) {
                stats[stats$Team %in% awayteam,]$W<-stats[stats$Team %in% awayteam,]$W + 1
            }
            else {
                stats[stats$Team %in% hometeam,]$W<-stats[stats$Team %in% hometeam,]$W + 1
            }
        }
        stats[stats$Team %in% awayteam,]$DIFF<-stats[stats$Team %in% awayteam,]$DIFF + (awaygoals-homegoals)
        stats[stats$Team %in% hometeam,]$DIFF<-stats[stats$Team %in% hometeam,]$DIFF + (homegoals-awaygoals)
        stats[stats$Team %in% c(awayteam, hometeam),]$GP<-stats[stats$Team %in% c(awayteam,hometeam),]$GP + 1
    }
    
    stats$P<-2*stats$W+stats$OTL
    stats$L<-stats$GP-stats$W-stats$OTL
    stats$ROW<-stats$W-stats$SOW
    stats$PP<-stats$P/stats$GP
    stats<-stats[order(-stats$P, -stats$PP, -stats$ROW, -stats$DIFF),]
    
    return(stats)
    
}

standings_alt<-function(df){
    tmpTable = data.frame(Team = sort(unique(df$AwayTeam)),
                          GP = 0, W = 0, OTL = 0, L = 0, ROW=0,
                          HomeGames = 0, HomeWin = 0, HomeOTW = 0, HomeSOW = 0, HomeOTL = 0, HomeLoss = 0,
                          AwayGames = 0, AwayWin = 0, AwayOTW = 0, AwaySOW = 0, AwayOTL = 0, AwayLoss = 0,
                          P = 0,
                          HomeFor = 0, HomeAgainst = 0,
                          AwayFor = 0, AwayAgainst = 0,
                          GF = 0, GA = 0, DIFF = 0, PP=0, OT.Win.Percent=0)
    
    # Games Played
    tmpTable$HomeGames = as.numeric(table(df$HomeTeam))
    tmpTable$AwayGames = as.numeric(table(df$AwayTeam))
    
    #Wins
    tmpTable$HomeWin = as.numeric(table(df$HomeTeam[df$HG > df$AG]))
    tmpTable$AwayWin = as.numeric(table(df$AwayTeam[df$AG > df$HG]))
    
    #Losses
    tmpTable$HomeLoss = as.numeric(table(df$HomeTeam[df$AG > df$HG]))
    tmpTable$AwayLoss = as.numeric(table(df$AwayTeam[df$HG > df$AG]))
    
    #OT Wins
    tmpTable$HomeOTW = as.numeric(table(df$HomeTeam[(df$OT.Win == "H") & (df$OT.SO == "OT")]))
    tmpTable$HomeSOW = as.numeric(table(df$HomeTeam[(df$OT.Win == "H") & (df$OT.SO == "SO")]))

    tmpTable$AwayOTW = as.numeric(table(df$AwayTeam[(df$OT.Win == "V") & (df$OT.SO == "OT")]))
    tmpTable$AwaySOW = as.numeric(table(df$AwayTeam[(df$OT.Win == "V") & (df$OT.SO == "SO")]))
    
    #OT Losses
    tmpTable$HomeOTL = as.numeric(table(df$HomeTeam[(df$OT.Win == "V")]))
    tmpTable$AwayOTL = as.numeric(table(df$AwayTeam[(df$OT.Win == "H")]))
    
    #W/L/OTL/ROW
    tmpTable$GP = tmpTable$HomeGames + tmpTable$AwayGames
    tmpTable$W = tmpTable$HomeWin + tmpTable$AwayWin + tmpTable$HomeOTW + tmpTable$HomeSOW + tmpTable$AwayOTW + tmpTable$AwaySOW
    tmpTable$OTL = tmpTable$HomeOTL + tmpTable$AwayOTL
    tmpTable$L = tmpTable$HomeLoss + tmpTable$AwayLoss
    tmpTable$ROW = tmpTable$W - (tmpTable$HomeSOW + tmpTable$AwaySOW)
    
    #Goal Diffs (includes OT scores)
    tmpTable$HomeFor = as.numeric(tapply(df$HG, df$HomeTeam, sum, na.rm = TRUE)) + tmpTable$HomeOTW + tmpTable$HomeSOW
    tmpTable$HomeAgainst = as.numeric(tapply(df$AG, df$HomeTeam, sum, na.rm = TRUE)) + tmpTable$HomeOTL
    
    tmpTable$AwayFor = as.numeric(tapply(df$AG, df$AwayTeam, sum, na.rm = TRUE)) + tmpTable$AwayOTW + tmpTable$AwaySOW
    tmpTable$AwayAgainst = as.numeric(tapply(df$HG, df$AwayTeam, sum, na.rm = TRUE)) + tmpTable$AwayOTL
    
    
    tmpTable$GF = ifelse(is.na(tmpTable$HomeFor), 0, tmpTable$HomeFor) + ifelse(is.na(tmpTable$AwayFor), 0, tmpTable$AwayFor)
    tmpTable$GA = ifelse(is.na(tmpTable$HomeAgainst), 0, tmpTable$HomeAgainst) + ifelse(is.na(tmpTable$AwayAgainst), 0, tmpTable$AwayAgainst)
    
    tmpTable$DIFF = tmpTable$GF - tmpTable$GA
    
    #Additional Stats
    tmpTable$P = 2 * tmpTable$W + tmpTable$OTL
    tmpTable$PP = tmpTable$P/tmpTable$GP
    tmpTable$OT.Win.Percent = (tmpTable$HomeOTW + tmpTable$HomeSOW + tmpTable$AwayOTW + tmpTable$AwaySOW)/(tmpTable$HomeOTW + tmpTable$HomeSOW + tmpTable$AwayOTW + tmpTable$AwayOTL + tmpTable$OTL)
    tmpTable<-tmpTable[,c("Team","GP", "W", "OTL", "L", "ROW", "P", "GF", "GA", "DIFF", "PP", "OT.Win.Percent")]
    tmpTable<-tmpTable[order(-tmpTable$P, -tmpTable$PP, -tmpTable$ROW, -tmpTable$DIFF),]
    
    rownames(tmpTable)<-1:nrow(tmpTable)
    
    return(tmpTable)
}

build_standings_table<-function(stats, standings=NA){
    if (is.na(standings)){
        standings<-matrix(0, nrow=length(unique(stats$Team)), ncol=length(unique(stats$Team)))
        rownames(standings)<-sort(unique(stats$Team))
        colnames(standings)<-c(1:ncol(standings))
    }
    
    
    for(t in 1:nrow(standings)){
        standings[stats[t, "Team"], t] <- standings[stats[t, "Team"], t]+1
    }
    return(standings)
}

get_and_prep_all_data<-function(yearlist=c(2009,2010,2011,2012,2013,2014,2015,2016)){
    df<-data.frame(Date=NULL, Visitor=NULL, G=NULL, Home=NULL, G.1=NULL, X=NULL)
    for (year in 1:length(yearlist)){
        df<-rbind(df, read.csv(paste('./practice_data/leagues_NHL_',yearlist[year],'_games_games.csv', sep=''))[c("Date", "Visitor", "G", "Home", "G.1", "X")])
    }
    df[df$Home == "Phoenix Coyotes",]$Home <- "Arizona Coyotes"
    df[df$Visitor == "Phoenix Coyotes",]$Visitor <- "Arizona Coyotes"
    df[df$Home == "Atlanta Thrashers",]$Home <- "Winnipeg Jets"
    df[df$Visitor == "Atlanta Thrashers",]$Visitor <- "Winnipeg Jets"
    df<-droplevels(df)
    return(df)
}

nhl_divisions<-list(
    "Atlantic" = c("Boston Bruins","Buffalo Sabres","Detroit Red Wings","Florida Panthers","Montreal Canadiens","Ottawa Senators","Tampa Bay Lightning","Toronto Maple Leafs"),
    "Metropolitan" = c("Washington Capitals", "New York Islanders", "New York Rangers", "New Jersey Devils", "Pittsburgh Penguins", "Philadelphia Flyers", "Carolina Hurricanes", "Columbus Blue Jackets"),
    "Central" = c("Dallas Stars", "St. Louis Blues", "Chicago Blackhawks", "Minnesota Wild", "Nashville Predators", "Colorado Avalanche", "Winnipeg Jets"),
    "Pacific" = c("Los Angeles Kings", "Arizona Coyotes", "Vancouver Canucks", "San Jose Sharks", "Anaheim Ducks", "Calgary Flames", "Edmonton Oilers")
)

nhl_conferences<-list(
    "East" = c(unlist(nhl_divisions["Atlantic"]), unlist(nhl_divisions["Metropolitan"])),
    "West" = c(unlist(nhl_divisions["Central"]), unlist(nhl_divisions["Pacific"]))
)


get_division_standings<-function(stats, division){
    return(stats[stats$Team %in% unlist(nhl_divisions[division]),])
}

get_conference_standings<-function(stats, conference){
    if (conference=="East"){
        a<-get_division_standings(stats, "Atlantic")
        b<-get_division_standings(stats, "Metropolitan")
    }
    else{
        a<-get_division_standings(stats, "Central")
        b<-get_division_standings(stats, "Pacific")
    }
    top6<-rbind(a[1:3], b[1:3])
    top6<-top6[order(-top6$P, -top6$PP, -top6$ROW, -top6$DIFF),]
    remainder<-rbind(a[4:nrow(a),], b[4:nrow(b),])
    remainder<-remainder[order(-remainder$P, -remainder$PP, -remainder$ROW, -remainder$DIFF)]
    
    return(rbind(top6, remainder))

}

