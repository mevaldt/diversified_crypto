single_asset_points <-
  function (object, return = c("mean", "mu"), risk = c("Cov", 
                                                     "Sigma", "CVaR", "VaR"), auto = TRUE, ...) 
{
  Statistics <- getStatistics(object)
  Type <- getType(object)
  return <- match.arg(return)
  risk <- match.arg(risk)
  if (auto) {
    return = "mu"
    Type = getType(object)
    Estimator = getEstimator(object)
    if (Type == "MV") 
      risk = "Cov"
    if (Type == "MV" & Estimator != "covEstimator") 
      risk = "Sigma"
    if (Type == "QLPM") 
      risk = "Sigma"
    if (Type == "CVaR") 
      risk = "CVaR"
  }
  if (return == "mean") {
    Return = Statistics$mean
  }
  else if (return == "mu") {
    Return = Statistics$mu
  }
  if (risk == "Cov") {
    Risk = sqrt(diag(Statistics$Cov))
  }
  else if (risk == "Sigma") {
    Risk = sqrt(diag(Statistics$Sigma))
  }
  else if (risk == "CVaR") {
    nAssets = getNAssets(object)
    Data = getSeries(object)
    alpha = getAlpha(object)
    Risk = NULL
    for (i in 1:nAssets) Risk = c(Risk, -.cvarRisk(Data[, 
                                                        i], 1, alpha))
  }
  else if (risk == "VaR") {
    nAssets = getNAssets(object)
    Data = getSeries(object)
    alpha = getAlpha(object)
    Risk = NULL
    for (i in 1:nAssets) Risk = c(Risk, -.varRisk(Data[, 
                                                       i], 1, alpha))
  }
  Risk = as.vector(Risk)
  assets = cbind(targetRisk = Risk, targetReturn = Return)
  attr(assets, "control") <- c(targetRisk = risk, targetReturn = return, 
                               auto = as.character(auto))
  # points(assets, ...)
  invisible(assets)
  }

frontier_points <- 
  function (object, frontier = c("both", "lower", "upper"), return = c("mean", 
                                                                     "mu"), risk = c("Cov", "Sigma", "CVaR", "VaR"), auto = TRUE) 
{
  frontier = match.arg(frontier)
  return = match.arg(return)
  risk = match.arg(risk)
  if (auto) {
    Type = getType(object)
    Estimator = getEstimator(object)
    if (Type == "MV") 
      risk = "Cov"
    if (Type == "MV" & Estimator != "covEstimator") 
      risk = "Sigma"
    if (Type == "QLPM") 
      risk = "Sigma"
    if (Type == "CVaR") 
      risk = "CVaR"
  }
  if (is.vector(getTargetRisk(object@portfolio))) {
    targetRisk = getTargetRisk(object@portfolio)[risk]
    targetReturn = getTargetReturn(object@portfolio)[return]
  }
  else {
    targetRisk = getTargetRisk(object@portfolio)[, risk]
    targetReturn = getTargetReturn(object@portfolio)[, return]
  }
  ans = cbind(Risk = targetRisk, Return = targetReturn)
  if (frontier == "upper") {
    index = 1:length(ans[, 1])
    test = c(-1, diff(ans[, 1]))
    index = index[test > 0]
    ans = ans[index, ]
  }
  else if (frontier == "lower") {
    index = 1:length(ans[, 1])
    test = c(-1, diff(ans[, 1]))
    index = index[test < 0]
    if (length(index) == 1) {
      ans = matrix(ans[index, ], ncol = 2)
    }
    else {
      ans = ans[index, ]
    }
  }
  colnames(ans) = c("targetRisk", "targetReturn")
  rownames(ans) = as.character(1:NROW(ans))
  attr(ans, "control") <- c(targetRisk = risk, targetReturn = return, 
                            auto = as.character(auto))
  ans
}

sharpe_ratio_lines <-
  function (object, return = c("mean", "mu"), risk = c("Cov", 
                                                       "Sigma", "CVaR", "VaR"), auto = TRUE, ...) 
  {
    return = match.arg(return)
    risk = match.arg(risk)
    data <- getSeries(object)
    spec <- getSpec(object)
    constraints <- getConstraints(object)
    riskFreeRate <- getRiskFreeRate(object)
    Type <- getType(object)
    frontPoints <- frontierPoints(object, frontier = "upper", 
                                  return = return, risk = risk, auto = auto)
    x <- frontPoints[, 1]
    y <- frontPoints[, 2] - riskFreeRate
    tangencyPortfolio <- tangencyPortfolio(data, spec, constraints)
    x.tg = frontierPoints(tangencyPortfolio, return = return, 
                          risk = risk, auto = auto)[, 2]
    norm <- x.tg/max(y/x)
    index <- 2:length(x)
    x <- x[index]
    y <- y[index]
    y.norm <- (y/x * norm)
    assets <- cbind(x, y.norm)
    # lines(x, y.norm, ...)
    index <- which.max(y.norm)
    # points(x[index], y.norm[index], col = "cyan", cex = 1.5)
    x.tg <- x.tg[index]
    norm2 <- x.tg/max(y)
    Range <- range(y/x * norm)
    nPrecision <- 3
    # Labels <- signif(Range, nPrecision)
    # axis(4, at = Range, labels = c(" ", " "), cex.axis = 0.75)
    # axis(4, at = mean(Range), labels = paste(Labels[1], "   ", 
                                             # Labels[2]), cex.axis = 0.75)
    # mtext("Sharpe Ratio", side = 4, line = 2, cex = 0.75)
    invisible(assets)
  }

tangency_points <-
  function (object, return = c("mean", "mu"), risk = c("Cov", 
                                                       "Sigma", "CVaR", "VaR"), auto = TRUE, ...) 
  {
    return <- match.arg(return)
    risk <- match.arg(risk)
    data <- getSeries(object)
    spec <- getSpec(object)
    constraints <- getConstraints(object)
    tgPortfolio <- tangencyPortfolio(data, spec, constraints)
    assets <- frontierPoints(tgPortfolio, return = return, risk = risk, 
                             auto = auto)
    # points(assets, ...)
    invisible(assets)
  }

tangency_lines <-
  function (object, return = c("mean", "mu"), risk = c("Cov", 
                                                       "Sigma", "CVaR", "VaR"), auto = TRUE, ...) {
    return = match.arg(return)
    risk = match.arg(risk)
    data <- getSeries(object)
    spec <- getSpec(object)
    constraints <- getConstraints(object)
    riskFreeRate <- getRiskFreeRate(object)
    tgPortfolio = tangencyPortfolio(data, spec, constraints)
    assets <- frontierPoints(tgPortfolio, return = return, risk = risk, 
                             auto = auto)
    slope <- (assets[2] - riskFreeRate)/assets[1]
    if (slope > 0) {
      # abline(riskFreeRate, slope, ...)
    }
    else {
      warning("Tangency point does not exist")
    }
    invisible(list(slope = slope, assets = assets))
  }

min_variance_points <-
  function(object,
           return = c("mean", "mu"), risk = c("Cov", "Sigma", "CVaR", "VaR"),
           auto = TRUE, ...)
  {

    return <- match.arg(return)
    risk <- match.arg(risk)
    
    # Get Portfolio Slots:
    data <- getSeries(object)
    spec <- getSpec(object)
    constraints <- getConstraints(object)
    
    # Add Minimum Variance Point:
    mvPortfolio <- minvariancePortfolio(data, spec, constraints)
    assets <- frontierPoints(mvPortfolio, return = return, risk = risk,
                             auto = auto)
    #points(assets, ...)
    
    # Return Value:
    invisible(assets)
  }