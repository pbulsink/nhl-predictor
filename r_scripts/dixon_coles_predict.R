# This is the Dixon Coles method of score prediction from 
# http://opisthokonta.net/?p=890 & ff. 



tau <- Vectorize(function(xx, yy, lambda, mu, rho){
  if (xx == 0 & yy == 0){return(1 - (lambda*mu*rho))
  } else if (xx == 0 & yy == 1){return(1 + (lambda*rho))
  } else if (xx == 1 & yy == 0){return(1 + (mu*rho))
  } else if (xx == 1 & yy == 1){return(1 - rho)
  } else {return(1)}
})

DClogLik <- function(y1, y2, lambda, mu, rho=0){
  #rho=0, independence
  #y1: home goals
  #y2: away goals
  sum(log(tau(y1, y2, lambda, mu, rho)) + log(dpois(y1, lambda)) + log(dpois(y2, mu)))
}

DCmodelData <- function(df){
  
  hm <- model.matrix(~ HomeTeam - 1, data=df, contrasts.arg=list(HomeTeam='contr.treatment'))
  am <- model.matrix(~ AwayTeam -1, data=df)
  
  team.names <- unique(c(levels(df$HomeTeam), levels(df$AwayTeam)))
  
  return(list(
    homeTeamDM=hm,
    awayTeamDM=am,
    homeGoals=df$HG,
    awayGoals=df$AG,
    teams=team.names
  )) 
}

DCoptimFn <- function(params, DCm){
  
  home.p <- params[1]
  rho.p <- params[2]
  
  nteams <- length(DCm$teams)
  attack.p <- matrix(params[3:(nteams+2)], ncol=1)
  defence.p <- matrix(params[(nteams+3):length(params)], ncol=1)
  
  lambda <- exp(DCm$homeTeamDM %*% attack.p + DCm$awayTeamDM %*% defence.p + home.p)
  mu <- exp(DCm$awayTeamDM %*% attack.p + DCm$homeTeamDM %*% defence.p)
  
  return(
    DClogLik(y1=DCm$homeGoals, y2=DCm$awayGoals, lambda, mu, rho.p) * -1
  )
}

DCattackConstr <- function(params, DCm, ...){
  nteams <- length(DCm$teams)
  attack.p <- matrix(params[3:(nteams+2)], ncol=1)
  return((sum(attack.p) / nteams) - 1)
}

do.Dixon.Coles.prediction<-function(df){
  require('alabama')
  #Get a useful data set
  dcm<-DCmodelData(df)
  
  #dummy fill parameters
  attack.params <- rep(.01, times=nlevels(df$HomeTeam))
  defence.params <- rep(-0.08, times=nlevels(df$HomeTeam))
  home.param <- 0.06
  rho.init <- 0.03
  par.inits <- c(home.param, rho.init, attack.params, defence.params)
  #it is also usefull to give the parameters some informative names
  names(par.inits) <- c('HOME', 'RHO', paste('Attack', dcm$teams, sep='.'), paste('Defence', dcm$teams, sep='.'))

  library(alabama)
  print("Starting model fitting")
  res <- auglag(par=par.inits, fn=DCoptimFn, heq=DCattackConstr, DCm=dcm)
  
  print(res$par)
  return(res)
  
}

predict.result<-function(res,home,away,maxgoal=12){
  attack.home<-paste("Attack",home,sep=".")
  attack.away<-paste("Attack",away,sep=".")
  defence.home<-paste("Defence",home,sep=".")
  defence.away<-paste("Defence",away,sep=".")
  
  # Expected goals home
  lambda <- exp(res$par['HOME'] + res$par[attack.home] + res$par[defence.away])
  # Expected goals away
  mu <- exp(res$par[attack.away] + res$par[defence.home])
  
  probability_matrix <- dpois(0:maxgoal, lambda) %*% t(dpois(0:maxgoal, mu))
  
  scaling_matrix <- matrix(tau(c(0,1,0,1), c(0,0,1,1), lambda, mu, res$par['RHO']), nrow=2)
  probability_matrix[1:2, 1:2] <- probability_matrix[1:2, 1:2] * scaling_matrix
  
  HomeWinProbability <- sum(probability_matrix[lower.tri(probability_matrix)])
  DrawProbability <- sum(diag(probability_matrix))
  AwayWinProbability <- sum(probability_matrix[upper.tri(probability_matrix)])
  
  return(c(HomeWinProbability,DrawProbability,AwayWinProbability))
}

