log5.OT.predictor<-function(stats, home, away){
    # reurns chances that home team wins in OT
    pa<-stats[stats$Team == home,]$OT.Win.Percent
    pb<-stats[stats$Team == away,]$OT.Win.Percent
    
    log5<-(pa-(pa*pb))/(pa+pb-(2*pa*pb))
    
    return(log5)
}

build_score_matrix<-function(res, home, away, m=NULL, maxgoal=7){
    if (!is.null(m)){
        # Expected goals home
        lambda <- predict(m, data.frame(Home=1, Team=home, Opponent=away), type='response')
        
        # Expected goals away
        mu <- predict(m, data.frame(Home=0, Team=away, Opponent=home), type='response')
    }
    else {
        attack.home<-paste("Attack",home,sep=".")
        attack.away<-paste("Attack",away,sep=".")
        defence.home<-paste("Defence",home,sep=".")
        defence.away<-paste("Defence",away,sep=".")
        
        # Expected goals home
        lambda <- exp(res$par['HOME'] + res$par[attack.home] + res$par[defence.away])
        # Expected goals away
        mu <- exp(res$par[attack.away] + res$par[defence.home])
    }
    probability_matrix <- dpois(0:maxgoal, lambda) %*% t(dpois(0:maxgoal, mu))
    
    scaling_matrix <- matrix(tau(c(0,1,0,1), c(0,0,1,1), lambda, mu, res$par['RHO']), nrow=2)
    probability_matrix[1:2, 1:2] <- probability_matrix[1:2, 1:2] * scaling_matrix
    
    pmatrix<-matrix(nrow=nrow(probability_matrix), ncol=ncol(probability_matrix))
    current_p<-0
    for (i in 1:ncol(pmatrix)){
        for (j in 1:nrow(pmatrix)){
            pmatrix[j,i]<-current_p
            current_p<-current_p + probability_matrix[j,i]
        }
    }
    return(pmatrix)
}

predict_one_game<-function(pmatrix,stats,home,away){
    random<-runif(1)
    while (random > pmatrix[nrow(pmatrix), ncol(pmatrix)]){
        random<-runif(1)
    }
    score<-as.vector(which(pmatrix>random, arr.ind=T)[1,])
    # scores is matrix c(home away)
    score<-score-1
    score[3]<-NA
    if (score[1] == score[2]){
        if(log5.OT.predictor(stats,home,away)<runif(1)){
            score[1] <- score[1] + 1
            
        }
        else{
            score[2] <- score[2] + 1
        }
        if(runif(1) > 0.56){
            score[3]<-"OT"   
        }
        else{
            score[3]<-"SO"
        }
    }
    return(score)
}

predict_score<-function(res, stats, home, away, m=NULL, maxgoal=7){
    return(predict_one_game(build_score_matrix(res, home, away, m=m, maxgoal=maxgoal), stats=stats, home, away))
}

predict_one_game_often<-function(pmatrix, stats, home, away, npredictions = 1000){
    scorelist<-data.frame("HG" = rep(0), "AG" = rep(0), "OT.SO" = rep(NA))
    for (i in 1:npredictions){
        #score<-predict_one_game(pmatrix = pmatrix, stats = stats, home = home, away = away)
        scorelist[i,]<-predict_one_game(pmatrix = pmatrix, stats = stats, home = home, away = away)
    }
    scorelist$HG<-as.integer(scorelist$HG)
    scorelist$AG<-as.integer(scorelist$AG)
    scorelist$OT.SO<-as.factor(scorelist$OT.SO)
    return(scorelist)
}


predict_remainder_of_season<-function(res, sched, stats, m=NULL, maxgoal=7){
    for (game in 1:nrow(sched)){
        home<-as.character(sched[game, "HomeTeam"])
        away<-as.character(sched[game, "AwayTeam"])
        score<-predict_score(res, stats, home, away, m=m, maxgoal = maxgoal)
        if (!is.na(score[3])){
            sched[game,"OT.SO"]<-score[3]
            if(score[1]>score[2]){
                sched[game, "HG"]<-score[2]
                sched[game, "AG"]<-score[2]
                sched[game, "OT.Win"]<-"H"
            }
            else{
                sched[game, "HG"]<-score[1]
                sched[game, "AG"]<-score[1]
                sched[game, "OT.Win"]<-"V"
            }
        }
        else{
            sched[game, "HG"]<-score[1]
            sched[game, "AG"]<-score[2]
        }
    }
    sched$HG<-as.integer(sched$HG)
    sched$AG<-as.integer(sched$AG)
    return(sched)
}

simulate_season<-function(res, sched, stats, past, n=1000, m=NULL, maxgoal=7){
    standings<-matrix(0, nrow=length(unique(nhl.this.year.stats$Team)), ncol=length(unique(nhl.this.year.stats$Team)))
    rownames(standings)<-sort(unique(nhl.this.year.stats$Team))
    colnames(standings)<-c(1:ncol(standings))
    for (i in 1:n){
        scores<-predict_remainder_of_season(res=res, sched=sched, stats=stats)
        standing.table<-standings_alt(rbind(past, scores))
        standings<-build_standings_table(stats=standing.table, standings=standings)
    }
    return(standings)
}
