log5.OT.predictor<-function(stats, home, away){
    # reurns chances that home team wins in OT
    pa<-stats[stats$Team == home,]$OT.Win.Percent
    pb<-stats[stats$Team == away,]$OT.Win.Percent
    
    log5<-(pa-(pa*pb))/(pa+pb-(2*pa*pb))
    
    return(log5)
}


