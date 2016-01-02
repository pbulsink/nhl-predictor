# This is the Dixon Coles method of score prediction from 
# http://opisthokonta.net/?p=890 & ff. 

tau.opt <- Vectorize(function(xx, yy, lambda, mu, rho){
    if (xx == 0 & yy == 0){return(1 - (lambda*mu*rho))
    } else if (xx == 0 & yy == 1){return(1 + (lambda*rho))
    } else if (xx == 1 & yy == 0){return(1 + (mu*rho))
    } else if (xx == 1 & yy == 1){return(1 - rho)
    } else {return(1)}
})

DClogLik.opt <- function(y1, y2, lambda, mu, rho=0){
    #rho=0, independence
    #y1: home goals
    #y2: away goals
    sum(log(tau.opt(y1, y2, lambda, mu, rho)) + log(dpois(y1, lambda)) + log(dpois(y2, mu)))
}

DCmodelData.opt <- function(df){
    
    team.names <- unique(c(levels(df$HomeTeam), levels(df$AwayTeam)))
    
    # attack, with sum-to-zero constraint
    ## home
    hm.a <- model.matrix(~ HomeTeam - 1, data=df)
    hm.a[df$HomeTeam == team.names[length(team.names)], ] <- -1
    hm.a <- hm.a[,1:(length(team.names)-1)]
    
    # away
    am.a <- model.matrix(~ AwayTeam -1, data=df)
    am.a[df$AwayTeam == team.names[length(team.names)], ] <- -1
    am.a <- am.a[,1:(length(team.names)-1)]
    
    # defence, same as before 
    hm.d <- model.matrix(~ HomeTeam - 1, data=df)
    am.d <- model.matrix(~ AwayTeam -1, data=df)
    
    return(list(homeTeamDMa=hm.a, homeTeamDMd=hm.d,
                awayTeamDMa=am.a, awayTeamDMd=am.d,
                homeGoals=df$HG, awayGoals=df$AG,
                teams=team.names)) 
}

DCoptimFn.opt <- function(params, DCm){
    
    home.p <- params[1]
    rho.p <- params[2]
    
    nteams <- length(DCm$teams)
    attack.p <- matrix(params[3:(nteams+1)], ncol=1) #one column less
    defence.p <- matrix(params[(nteams+2):length(params)], ncol=1) 
    
    # need to multiply with the correct matrices
    lambda <- exp(DCm$homeTeamDMa %*% attack.p + DCm$awayTeamDMd %*% defence.p + home.p)
    mu <- exp(DCm$awayTeamDMa %*% attack.p + DCm$homeTeamDMd %*% defence.p)
    
    return(
        DClogLik.opt(y1=DCm$homeGoals, y2=DCm$awayGoals, lambda, mu, rho.p) * -1
    )
}

do.Dixon.Coles.prediction.opt<-function(df){
    #Get a useful data set
    dcm<-DCmodelData.opt(df)
    nteams <- length(dcm$teams)
    
    #dummy fill parameters
    #initial parameter estimates
    attack.params <- rep(.1, times=nteams-1) # one less parameter
    defence.params <- rep(-0.8, times=nteams)
    home.param <- 0.06
    rho.init <- 0.03
    par.inits <- c(home.param, rho.init, attack.params, defence.params)
    
    #informative names
    #skip the last team
    names(par.inits) <- c('HOME', 'RHO', 
                          paste('Attack', dcm$teams[1:(nteams-1)], sep='.'),
                          paste('Defence', dcm$teams, sep='.'))
    
    
    print("Starting model fitting")
    res <- optim(par=par.inits, fn=DCoptimFn.opt, DCm=dcm, method='BFGS')
    
    print(res$par)
    
    parameters <- res$par
    
    #compute last team attack parameter
    missing.attack <- sum(parameters[3:(nteams+1)]) * -1
    
    #put it in the parameters vector
    parameters <- c(parameters[1:(nteams+1)], missing.attack, parameters[(nteams+2):length(parameters)])
    names(parameters)[nteams+2] <- paste('Attack.', dcm$teams[nteams], sep='')
    
    #increase attack by one
    parameters[3:(nteams+2)] <- parameters[3:(nteams+2)] + 1  
    
    #decrease defence by one
    parameters[(nteams+3):length(parameters)] <- parameters[(nteams+3):length(parameters)] - 1 
    
    res$par<-parameters
    return(res)
    
}

predict.result.opt<-function(res,home,away,maxgoal=7){
    attack.home<-paste("Attack",home,sep=".")
    attack.away<-paste("Attack",away,sep=".")
    defence.home<-paste("Defence",home,sep=".")
    defence.away<-paste("Defence",away,sep=".")
    
    # Expected goals home
    lambda <- exp(res$par['HOME'] + res$par[attack.home] + res$par[defence.away])
    # Expected goals away
    mu <- exp(res$par[attack.away] + res$par[defence.home])
    
    probability_matrix <- dpois(0:maxgoal, lambda) %*% t(dpois(0:maxgoal, mu))
    
    scaling_matrix <- matrix(tau.opt(c(0,1,0,1), c(0,0,1,1), lambda, mu, res$par['RHO']), nrow=2)
    probability_matrix[1:2, 1:2] <- probability_matrix[1:2, 1:2] * scaling_matrix
    
    HomeWinProbability <- sum(probability_matrix[lower.tri(probability_matrix)])
    DrawProbability <- sum(diag(probability_matrix))
    AwayWinProbability <- sum(probability_matrix[upper.tri(probability_matrix)])
    
    return(c(HomeWinProbability,DrawProbability,AwayWinProbability))
}
