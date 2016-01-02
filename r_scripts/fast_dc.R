do.Fast.fit<-function(df){
    df.indep <- data.frame(Team=as.factor(c(as.character(df$HomeTeam),
                                            as.character(df$AwayTeam))),
                           Opponent=as.factor(c(as.character(df$AwayTeam),
                                                as.character(df$HomeTeam))),
                           Goals=c(df$HG, df$AG),
                           Home=c(rep(1, dim(df)[1]), rep(0, dim(df)[1])))
    m <- glm(Goals ~ Home + Team + Opponent, data=df.indep, family=poisson())
    return(m)
}

do.Fast.DC<-function(m, df){
    expected <- fitted(m)
    home.expected <- expected[1:nrow(df)]
    away.expected <- expected[(nrow(df)+1):(nrow(df)*2)]
    
    DCoptimRhoFn.fast <- function(par){
        rho <- par[1]
        DClogLik(df$HG, df$AG, home.expected, away.expected, rho)
    }
    
    res <- optim(par=c(0.1), fn=DCoptimRhoFn.fast, control=list(fnscale=-1), method='BFGS')
    return(res)
}

fast.DC.predict<-function(m,res,home,away,maxgoal=7){
    # Expected goals home
    lambda <- predict(m, data.frame(Home=1, Team=home, Opponent=away), type='response')
    
    # Expected goals away
    mu <- predict(m, data.frame(Home=0, Team=away, Opponent=home), type='response')
    
    probability_matrix <- dpois(0:maxgoal, lambda) %*% t(dpois(0:maxgoal, mu))
    
    scaling_matrix <- matrix(tau(c(0,1,0,1), c(0,0,1,1), lambda, mu, res$par), nrow=2)
    probability_matrix[1:2, 1:2] <- probability_matrix[1:2, 1:2] * scaling_matrix
    
    HomeWinProbability <- sum(probability_matrix[lower.tri(probability_matrix)])
    DrawProbability <- sum(diag(probability_matrix))
    AwayWinProbability <- sum(probability_matrix[upper.tri(probability_matrix)])
    
    return(c(HomeWinProbability,DrawProbability,AwayWinProbability))
}