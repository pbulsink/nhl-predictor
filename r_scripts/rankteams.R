rankteams<-function (scores = NULL, teams = NULL, family = "poisson", fun = "glm", 
          max.date = "2100-6-1", min.date = "1900-5-1", date.format = "%Y-%m-%d", 
          time.weight.eta = 0, add = NULL, silent = FALSE, ...) 
{
    library(fbRanks)
    if (missing(scores)) 
        stop("A scores data frame must be passed in.\n", call. = FALSE)
    if (!is.data.frame(scores)) 
        stop("scores must be a data frame.\n", call. = FALSE)
    colnames(scores) = tolower(fbRanks::str_strip.white(names(scores)))
    if (any(duplicated(names(scores)))) 
        stop("Duplicated column names are not allowed in the score data frame.  Names are case insensitive and extra space is striped.\n", 
             call. = FALSE)
    req.columns = c("date", "home.team", "home.score", "away.team", 
                    "away.score")
    if (!all(req.columns %in% names(scores))) 
        stop("The scores data frame must include the columns: date, home.team, home.score, away.team, away.score\n", 
             call. = FALSE)
    for (i in names(scores)) {
        if (is.factor(scores[[i]])) 
            scores[[i]] = as.character(scores[[i]])
    }
    if (any(is.na(as.Date(scores$date, date.format)))) 
        stop("The scores dates are not in the same format as date.format.\n", 
             call. = FALSE)
    if (!all(fun %in% c("glm", "speedglm", "glmnet"))) 
        stop("The fun argument must be either \"glm\". \"speedglm\" or \"glmnet\".\n", 
             call. = FALSE)
    if (missing(teams)) {
        if (!silent) 
            cat("Alert: teams data frame was not passed in. Will attempt to construct one from the scores data frame.You should ensure that teams use only one name in scores data frame.\n")
        teams = data.frame(name = sort(unique(c(scores$home.team, 
                                                scores$away.team))), stringsAsFactors = FALSE)
    }
    if (!is.data.frame(teams)) 
        stop("teams must be a data frame.\n", call. = FALSE)
    colnames(teams) = tolower(fbRanks::str_strip.white(names(teams)))
    if (any(duplicated(names(teams)))) 
        stop("Duplicated column names are not allowed in the teams data frame.  Names are case insensitive and extra space is striped.\n", 
             call. = FALSE)
    req.columns = c("name")
    if (!all(req.columns %in% names(teams))) 
        stop("The teams data frame must include the column \"name\" which is the team name.\n", 
             call. = FALSE)
    for (i in names(teams)) {
        if (is.factor(teams[[i]])) 
            teams[[i]] = as.character(teams[[i]])
    }
    if (any(duplicated(c(names(scores), names(teams))))) 
        stop("Column names that appear in both the scores and teams data frames are not allowed.  Names are case insensitive and extra space is striped.\n", 
             call. = FALSE)
    if (is.na(as.Date(max.date, date.format))) 
        stop(paste("max.date must be entered in the following format:", 
                   format(Sys.Date(), date.format), "\n or pass in the argument date.format to specify a different date format.\n"))
    if (is.na(as.Date(min.date, date.format))) 
        stop(paste("min.date must be entered in the following format:", 
                   format(Sys.Date(), date.format), "\n or pass in the argument date.format to specify a different date format.\n"))
    max.date = as.Date(max.date, date.format)
    min.date = as.Date(min.date, date.format)
    display.names = unique(teams$name)
    nteams = length(unique(display.names))
    ok = TRUE
    bad.names = scores$home.team[!(scores$home.team %in% display.names)]
    if (length(bad.names) != 0) {
        cat("The following home team names don't match names in team file:\n")
        cat(as.character(bad.names), sep = ", ")
        cat("\nerrors are on lines ")
        cat((1:dim(scores)[1])[!(scores$home.team %in% display.names)])
        cat("\n")
        ok = FALSE
    }
    bad.names = scores$away.team[!(scores$away.team %in% display.names)]
    if (length(bad.names) != 0) {
        cat("The following away team names don't match names in team file:\n")
        cat(as.character(bad.names), sep = ", ")
        cat("\nerrors are on lines ")
        cat((1:dim(scores)[1])[!(scores$away.team %in% display.names)])
        cat("\n")
        ok = FALSE
    }
    if (!ok) {
        cat("Bad team names in scores file.  Score file being returned.\n")
        return(scores = scores)
    }
    tmp = team.and.score.filters(list(scores = scores, teams = teams), 
                                 ...)
    scores = scores[tmp$include.scores, , drop = FALSE]
    teams = teams[teams$name %in% tmp$include.teams, , drop = FALSE]
    if (!is.null(add) & !is.character(add)) 
        stop("add must be a vector of names to add to the model.", 
             call. = FALSE)
    scores.names = names(scores)[!(names(scores) %in% c("date", 
                                                        "home.team", "home.score", "away.team", "away.score"))]
    add.to.formula = list()
    for (tmp in add) {
        if (paste("home.", tmp, sep = "") %in% scores.names) {
            if (!(paste("away.", tmp, sep = "") %in% scores.names)) 
                stop(paste("home.", tmp, " appears in scores file, therefore, away.", 
                           tmp, " must also appear.\n", sep = ""), call. = FALSE)
            if (tmp %in% scores.names) 
                stop(paste("home.", tmp, ", home.", tmp, " and ", 
                           tmp, "appear in scores file, therefore the predictor to add is ambiguous.\n", 
                           sep = ""), call. = FALSE)
        }
        if (paste("away.", tmp, sep = "") %in% scores.names) {
            if (!(paste("home.", tmp, sep = "") %in% scores.names)) 
                stop(paste("away.", tmp, " appears in scores file, therefore, home.", 
                           tmp, " must also appear.\n", sep = ""), call. = FALSE)
        }
        if (str_sub(tmp, 1, 5) == "home.") 
            stop(paste(tmp, "should not have the home. prefix.  Predictors that are different for home and away are\nentered as home.pred and away.pred (a pair) in scores file, and only pred is passed into add.to.formula.\n", 
                       sep = ""), call. = FALSE)
        if (str_sub(tmp, 1, 5) == "away.") 
            stop(paste(tmp, "should not have the away. prefix.  Predictors that are different for home and away are\nentered as home.pred and away.pred (a pair) in scores file, and only pred is passed into add.to.formula.\n", 
                       sep = ""), call. = FALSE)
        if (paste("home.", tmp, sep = "") %in% scores.names) {
            add.to.formula[[tmp]] = c(paste("home.", tmp, sep = ""), 
                                      paste("away.", tmp, sep = ""))
        }
        else {
            if (!(tmp %in% scores.names)) 
                stop(paste(tmp, " is in add but no column of that name in the scores file.\n", 
                           sep = ""), call. = FALSE)
            add.to.formula[[tmp]] = c(tmp, tmp)
        }
    }
    scores = scores[!(is.na(scores$home.score) & is.na(scores$away.score)), 
                    ]
    scores.glm = scores
    level.names = sort(unique(c(as.character(scores$home.team), 
                                as.character(scores$away.team))))
    scores.glm$home.team = factor(scores$home.team, levels = level.names)
    scores.glm$away.team = factor(scores$away.team, levels = level.names)
    scores.glm = scores.glm[scores.glm$date >= min.date, , drop = FALSE]
    scores.glm = scores.glm[scores.glm$date <= max.date, , drop = FALSE]
    el = cbind(as.character(scores.glm$home.team), as.character(scores.glm$away.team))
    g1 = graph.edgelist(el, directed = FALSE)
    clus = clusters(g1)
    clus.names = get.vertex.attribute(g1, "name")
    glm.fit = list()
    glmer.fit = list()
    scores$home.residuals = NA
    scores$away.residuals = NA
    el.full = cbind(as.character(scores$home.team), as.character(scores$away.team))
    date.filter = scores$date >= min.date & scores$date <= max.date
    tmp.fun = function(x, y) {
        any(x %in% y)
    }
    for (cluster in 1:clus$no) {
        names.in.clus = clus.names[clus$membership == cluster]
        rows.clus = apply(el, 1, tmp.fun, names.in.clus)
        scores.clus = scores.glm[rows.clus, , drop = FALSE]
        time.diff = as.numeric(max.date - scores.clus$date)
        time.diff = c(time.diff, time.diff)
        attack.team = c(as.character(scores.clus$home.team), 
                        as.character(scores.clus$away.team))
        defense.team = c(as.character(scores.clus$away.team), 
                         as.character(scores.clus$home.team))
        scores.team = c(scores.clus$home.score, scores.clus$away.score)
        level.names = sort(unique(c(as.character(scores.clus$home.team), 
                                    as.character(scores.clus$away.team))))
        attack = factor(attack.team, levels = level.names)
        defend = factor(defense.team, levels = level.names)
        if (fun %in% c("speedglm", "glmnet")) {
            xlevels = list()
            xlevels$attack = level.names
            xlevels$defend = level.names
            terms.dataClasses = c(scores.team = "numeric", attack = "factor", 
                                  defend = "factor")
        }
        add.to.f = ""
        for (add.f in names(add.to.formula)) {
            ok = TRUE
            new.col = c(scores.clus[[add.to.formula[[add.f]][1]]], 
                        scores.clus[[add.to.formula[[add.f]][2]]])
            if (is.character(new.col)) {
                new.col = factor(new.col)
                if (fun %in% c("speedglm", "glmnet")) 
                    xlevels[[paste(add.f, ".f", sep = "")]] = levels(new.col)
                if (length(levels(new.col)) == 1) {
                    ok = FALSE
                }
            }
            assign(paste(add.f, ".f", sep = ""), new.col)
            if (ok) {
                add.to.f = paste(add.to.f, "+", paste(add.f, 
                                                      ".f", sep = ""), sep = "")
                if (fun %in% c("speedglm", "glmnet")) 
                    terms.dataClasses[paste(add.f, ".f", sep = "")] = class(new.col)
            }
        }
        if ("glm" %in% fun) {
            my.formula = paste("scores.team~-1+attack+defend", 
                               add.to.f, sep = "")
            glm.fit[[paste("cluster.", cluster, sep = "")]] = glm(formula = as.formula(my.formula), 
                                                                  family = family, na.action = "na.exclude", weights = exp(-1 * 
                                                                                                                               time.weight.eta * time.diff))
            rows.clus.full = apply(el.full, 1, tmp.fun, names.in.clus) & 
                date.filter
            glm.fit[[paste("cluster.", cluster, sep = "")]]
            theresids = residuals(glm.fit[[paste("cluster.", 
                                                 cluster, sep = "")]], type = "response")
            scores$home.residuals[rows.clus.full] = theresids[1:(length(theresids)/2)]
            scores$away.residuals[rows.clus.full] = theresids[(length(theresids)/2 + 
                                                                   1):length(theresids)]
        }
        if ("speedglm" %in% fun) {
            require(speedglm)
            weights = exp(-1 * time.weight.eta * time.diff)
            bad.nas = is.na(scores.team)
            if (sum(bad.nas) == length(scores.team)) {
                glm.fit[[paste("cluster.", cluster, sep = "")]] = "all NAs"
                next
            }
            da = data.frame(scores.team = scores.team, attack = attack, 
                            defend = defend, weights = weights)
            for (add.f in names(add.to.formula)) {
                da[, paste(add.f, ".f", sep = "")] = get(paste(add.f, 
                                                               ".f", sep = ""))
            }
            da = da[!bad.nas, ]
            if (family == "poisson") 
                linkfun = poisson(log)
            my.formula = paste("scores.team~-1+attack+defend", 
                               add.to.f, sep = "")
            glm.fit[[paste("cluster.", cluster, sep = "")]] = speedglm(formula = as.formula(my.formula), 
                                                                       data = da, family = linkfun, weights = da$weights)
            glm.fit[[paste("cluster.", cluster, sep = "")]]$xlevels = xlevels
            attr(glm.fit[[paste("cluster.", cluster, sep = "")]]$terms, 
                 "dataClasses") = terms.dataClasses
            rows.clus.full = apply(el.full, 1, tmp.fun, names.in.clus) & 
                date.filter
            attack.coef = defend.coef = rep(0, length(names.in.clus))
            loc = match(paste("attack", names.in.clus, sep = ""), 
                        names(glm.fit[[paste("cluster.", cluster, sep = "")]]$coef))
            attack.coef = glm.fit[[paste("cluster.", cluster, 
                                         sep = "")]]$coef[loc]
            names(attack.coef) = names.in.clus
            attack.coef[is.na(attack.coef)] = 0
            loc = match(paste("defend", names.in.clus, sep = ""), 
                        names(glm.fit[[paste("cluster.", cluster, sep = "")]]$coef))
            defend.coef = glm.fit[[paste("cluster.", cluster, 
                                         sep = "")]]$coef[loc]
            names(defend.coef) = names.in.clus
            defend.coef[is.na(defend.coef)] = 0
            scores$home.residuals[rows.clus.full] = scores$home.score[rows.clus.full] - 
                exp(attack.coef[match(scores$home.team[rows.clus.full], 
                                      names.in.clus)] + defend.coef[match(scores$away.team[rows.clus.full], 
                                                                          names.in.clus)])
            scores$away.residuals[rows.clus.full] = scores$away.score[rows.clus.full] - 
                exp(attack.coef[match(scores$away.team[rows.clus.full], 
                                      names.in.clus)] + defend.coef[match(scores$home.team[rows.clus.full], 
                                                                          names.in.clus)])
        }
        if ("glmnet" %in% fun) {
            require(glmnet)
            weights = exp(-1 * time.weight.eta * time.diff)
            bad.nas = is.na(scores.team)
            if (sum(bad.nas) == length(scores.team)) {
                glm.fit[[paste("cluster.", cluster, sep = "")]] = "all NAs"
                next
            }
            bad.scores = FALSE
            attack = attack[!bad.nas & !bad.scores]
            defend = defend[!bad.nas & !bad.scores]
            weights = weights[!bad.nas & !bad.scores]
            scores.team = scores.team[!bad.nas & !bad.scores]
            n = length(attack)
            sx.i = 1:n
            sx.j = as.numeric(attack)
            sx.x = rep(1, n)
            sx.dn = paste("attack", level.names, sep = "")
            add.col = length(level.names)
            sx.i = c(sx.i, 1:n)
            sx.j = c(sx.j, as.numeric(defend) + add.col)
            sx.x = c(sx.x, rep(1, n))
            sx.dn = c(sx.dn, paste("defend", level.names, sep = ""))
            first.factor = add.col + 1
            add.col = add.col + length(level.names)
            for (add.f in names(add.to.formula)) {
                add.f = paste(add.f, ".f", sep = "")
                if (is.factor(get(add.f))) {
                    sx.i = c(sx.i, 1:n)
                    sx.j = c(sx.j, as.numeric(get(add.f)[!bad.nas & 
                                                             !bad.scores]) + add.col)
                    sx.x = c(sx.x, rep(1, n))
                    sx.dn = c(sx.dn, paste(levels(get(add.f)), 
                                           add.f, sep = ""))
                    first.factor = c(first.factor, add.col + 1)
                    add.col = add.col + length(levels(get(add.f)))
                }
                else {
                    sx.i = c(sx.i, 1:n)
                    sx.j = c(sx.j, 1 + add.col)
                    sx.x = c(sx.x, get(add.f)[!bad.nas & !bad.scores])
                    sx.dn = c(sx.dn, add.f)
                    add.col = add.col + 1
                }
            }
            sx.i = sx.i[!(sx.j %in% first.factor)]
            sx.x = sx.x[!(sx.j %in% first.factor)]
            sx.j = sx.j[!(sx.j %in% first.factor)]
            sx = sparseMatrix(i = sx.i, j = sx.j, x = sx.x, dims = c(n, 
                                                                     add.col), dimnames = list(1:n, sx.dn))
            call.list = list(x = sx, y = scores.team, intercept = FALSE, 
                             family = family, alpha = 1, lambda = 0, weights = weights)
            glm.fit[[paste("cluster.", cluster, sep = "")]] = do.call(glmnet, 
                                                                      call.list)
            glm.fit[[paste("cluster.", cluster, sep = "")]]$xlevels = xlevels
            my.formula = as.formula(paste("scores.team~-1+attack+defend", 
                                          add.to.f, sep = ""))
            glm.fit[[paste("cluster.", cluster, sep = "")]]$terms = terms(my.formula)
            attr(glm.fit[[paste("cluster.", cluster, sep = "")]]$terms, 
                 "dataClasses") = terms.dataClasses
            rows.clus.full = apply(el.full, 1, tmp.fun, names.in.clus) & 
                date.filter
            attack.coef = defend.coef = rep(NA, length(names.in.clus))
            loc = match(paste("attack", names.in.clus, sep = ""), 
                        rownames(coef(glm.fit[[paste("cluster.", cluster, 
                                                     sep = "")]])))
            attack.coef = coef(glm.fit[[paste("cluster.", cluster, 
                                              sep = "")]], s = 0)[loc]
            names(attack.coef) = names.in.clus
            loc = match(paste("defend", names.in.clus, sep = ""), 
                        rownames(coef(glm.fit[[paste("cluster.", cluster, 
                                                     sep = "")]])))
            defend.coef = coef(glm.fit[[paste("cluster.", cluster, 
                                              sep = "")]], s = 0)[loc]
            names(defend.coef) = names.in.clus
            scores$home.residuals[rows.clus.full] = scores$home.score[rows.clus.full] - 
                exp(attack.coef[match(scores$home.team[rows.clus.full], 
                                      names.in.clus)] + defend.coef[match(scores$away.team[rows.clus.full], 
                                                                          names.in.clus)])
            scores$away.residuals[rows.clus.full] = scores$away.score[rows.clus.full] - 
                exp(attack.coef[match(scores$away.team[rows.clus.full], 
                                      names.in.clus)] + defend.coef[match(scores$home.team[rows.clus.full], 
                                                                          names.in.clus)])
        }
    }
    rtn.list = list(fit = glm.fit, graph = list(graph = g1, membership = clus$membership, 
                                                csize = clus$csize, no = clus$no, names = get.vertex.attribute(g1, 
                                                                                                               "name")), scores = scores, teams = teams, max.date = max.date, 
                    min.date = min.date, time.weight.eta = 0, date.format = date.format)
    class(rtn.list) = "fbRanks"
    if (!silent) 
        print(rtn.list)
    invisible(rtn.list)
}