################### Test Chisq.gof en versión R ##############

chisq.gof <-
function(x, n.classes = ceiling(2 * (length(x)^(2/5))), cut.points = 
    NULL, distribution = "normal", n.param.est = 0, ...)
{
    dist.expanded <- if(!missing(distribution)) char.expand(
            distribution, c("normal", "beta", "cauchy",
            "chisquare", "exponential", "f", "gamma", 
            "lognormal", "logistic", "t", "uniform", 
            "weibull", "binomial", "geometric", 
            "hypergeometric", "negbinomial", "poisson",
            "wilcoxon"),stop("argument 'alternative' must match one of \n \"normal\",\"beta\",\"cauchy\",\"chisquare\",\n                            \"exponential\",\"f\",\"gamma\",\"lognormal\",\n                            \"logistic\",\"t\",\"uniform\",\"weibull\",\n                            \"binomial\",\"geometric\",\"hypergeometric\",\n                           \"negbinomial\",\"poisson\",\"wilcoxon\"\n ." )       )                   else distribution
    if(!is.null(cut.points)) {
        n.cut.points <- length(cut.points)
        n.classes <- n.cut.points - 1
    }
    if((bad.obs <- sum(!(x.ok <- is.finite(x)))) > 0) {
        is.not.finite.warning(x)
        x <- x[x.ok]
        warning(paste(bad.obs, 
            "observations with NA/NaN/Inf in 'x' removed.")
            )
    }
    discrete <- charmatch(dist.expanded, c("binomial", "geometric",
        "hypergeometric", "negbinomial", "poisson", "wilcoxon"),
        nomatch = 0)
    distn <- switch(dist.expanded,
        normal = "norm",
        beta = "beta",
        cauchy = "cauchy",
        chisquare = "chisq",
        exponential = "exp",
        f = "f",
        gamma = "gamma",
        lognormal = "lnorm",
        logistic = "logis",
        t = "t",
        uniform = "unif",
        weibull = "weibull",
        binomial = "binom",
        geometric = "geom",
        hypergeometric = "hyper",
        negbinomial = "nbinom",
        poisson = "pois",
        wilcoxon = "wilcox")
    pdistn <- getFunction(paste("p", distn, sep = ""))
    #Calculate counts for each interval
    if(is.null(cut.points)) {
        if(discrete)
            stop("You must supply the cutpoints for a discrete distribution")
        else {
            #
            # use equiprobable intervals, if the cut points are not specified
            # and the distribution is continuous
            num <- pmin(floor(1 + n.classes * pdistn(x,
                ...)), n.classes)
            prob <- rep(1/n.classes, n.classes)
        }
    }
    else {
        #
        # cutpoints specified
        n.cut.pts <- cut.points
        #create a new cut.point vector
        #with Infs removed, if necessary
        if(cut.points[1] ==  - Inf) n.cut.pts[1] <- min(x,
                cut.points[-1]) - 1
        if(cut.points[n.cut.points] == Inf)
            n.cut.pts[n.cut.points] <- max(x, cut.points[
                 - n.cut.points]) + 1
        if(sum(!(is.finite(n.cut.pts))) > 0)
            stop("Please remove Inf's or NA's from cut.points\n")
        num <- cut(x, n.cut.pts)
        if((bad.obs <- sum(!(num.ok <- is.finite(num)))) > 0) 
            {
            is.not.finite.warning(x)
            num <- num[num.ok]
            warning(paste(bad.obs, 
                "observations do not fall within within the given cutpoints.  \n   There are removed."     ))
            if(length(x) - bad.obs < 2)
                stop("Less than 2 non-missing values. Impossible to continue\n")
        }
        prob <- diff(pdistn(cut.points, ...))
    }
    count <- tabulate(num, n.classes)
    xpec <- length(x) * prob
    X2.list <- .pearson.x2(observed = count, expected = xpec, yates
         = F)
    if(!is.null(X2.list$less.than.5))
        warning("Expected counts < 5. Chi-squared approximation may not\n be appropriate."
            )
    ret.val <- list(statistic = X2.list$X2, parameters = n.classes -
        n.param.est - 1, method = 
        "Chi-square Goodness of Fit Test", alternative = paste(
        "True cdf does not equal the", dist.expanded, 
        "Distn. for at least one sample point."), data.name = 
        deparse(substitute(x)), counts = count, expected = xpec
        )
    ret.val$p.value <- 1 - pchisq(ret.val$statistic, ret.val$
        parameters)
    names(ret.val$statistic) <- "Chi-square"
    names(ret.val$parameters) <- "df"
    ret.val <- ret.val[c("statistic", "parameters", "p.value",
        "alternative", "method", "data.name", "counts", 
        "expected")]
    attr(ret.val, "class") <- "htest"
    return(ret.val)
}
 
.pearson.x2<-
function(observed, expected = NULL, yates = F)
{
    storage.mode(observed) <- "double"
    if(is.null(expected)) {
        expected <- apply(observed, 1, sum)
        if((dlen <- length(dim(observed))) > 1) {
            sum.obs <- sum(observed)
            for(i in 2:dlen)
                expected <- outer(expected, apply(observed, i, sum))/sum.obs
        }
    }
    else storage.mode(expected) <- "double"
    ret.val <- list(X2 = sum((abs(expected - observed) - (if(yates) 0.5 else 0))^2/expected))
    if(any(expected < 5))
        ret.val$less.than.5 <- TRUE
    ret.val
}

is.not.finite.warning<-
function(x, name = deparse(substitute(x)))
{
    # warn about nonfinite values in x (refer to it as 'name')
    if(!is.numeric(x)) {
        warning(paste(name, "is not a numeric dataset"))
    }
    else {
        n.na <- sum(is.na(x))
        # counts all NA's including NaN's
        n.nan <- sum(is.nan(x))
        # is.nan(NA) -> F, no overlap
        n.inf <- sum(is.infinite(x))
        n.bad <- n.na + n.inf
        if(n.bad == 0)
            return()
        n <- c(n.na - n.nan, n.nan, n.inf)
        types <- c("NA's", "NaN's", "+-Inf's")
        msg0 <- paste("There were", n.bad, "nonfinite values in", name, ":")
        msg1 <- paste(n[n > 0], types[n > 0], collapse = ", ")
        warning(paste(msg0, msg1))
    }
}