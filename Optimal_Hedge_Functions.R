library(tidyverse)
library(rugarch)
library(rmgarch)
library(metafor)
library(MASS)


# Long Only ---------------------------------------------------------------
lo_weights <- function(train_data, y){
  x <- colnames(train_data)[-which(colnames(train_data)==y)]
  return(c(setNames(1, y), setNames(rep(0, length(x)), x)))
}


# Functions for Calculating Optimal Hedge Weights -------------------------
ols_weights <- function(train_data, y, lamda = 1, fwd=1){
  # Calculate the linear regression optimal hedge weights
  # Can use exponentially decaying weights via lamda
  #
  # Args:
  #   train_data: dataframe containing returns
  #   y: a string representing the y variable name in the train_data
  #   lamda: numeric representing the exponential decay rate
  #
  # Returns:
  #   named numeric vector of optimal weights with value 1 for y variable
  fwd <- ifelse(is.na(fwd), 5, fwd)
  x <- colnames(train_data)[-which(colnames(train_data)==y)]
  n <- nrow(train_data)
  #weights = (1 - lamda)^((n - 1):0)
  weights = lamda^(seq(n, 1, by = -1))
  ewma <- colSums(weights * train_data) / sum(weights) * fwd
  diff <- as.matrix(sweep(train_data, 2, ewma))
  biased_cov <- t(diff) %*% (weights * diff) / sum(weights)
  bias_correction <- sum(weights)^2 / (sum(weights)^2 - sum(weights^2))
  ewmc = bias_correction * biased_cov * fwd
  ols <- metafor::matreg(y = y, x = x, R = ewmc, cov = T, means = ewma, n = n)
  coeff <- setNames(-ols$tab$beta, rownames(ols$tab))[-1]

  return(c(setNames(1, y), coeff))
}

robust_weights <- function(train_data, y){
  x <- colnames(train_data)[-which(colnames(train_data)==y)]
  robust_lm <- MASS::rlm(BIZD ~ HYG + XLF + IWM, data = train_data)
  coeff <- robust_lm$coefficients[-1]
  return(c(setNames(1, y), coeff))
}

mgarch_weights <- function(train_data, y, fwd = 5){
  # Calculate optimal hedge weights using multivaiate GARCH DCC
  # Uses multivariate GARCH with dynamic conditional correlation
  # Calculates forward covariance using GARCH(1,1) and forward mean using ARMA(1,1)
  # Used forward covariance and mean to calculate optimal weights
  #
  # Args:
  #   train_data: n x m dataframe containing n period returns for m assets
  #   y: a string representing the y variable name in the train_data
  #   fwd: a numeric representing the number of periods forward to forecast
  #
  # Returns:
  #   named numeric vector of optimal weights with value 1 for y variable
  fwd <- ifelse(is.na(fwd), 5, fwd)
  x <- colnames(train_data)[-which(colnames(train_data)==y)]
  n <- nrow(train_data)

  # Create univariate GARCH model with ARMA(1,1) and GARCH(1,1)
  xspec <- rugarch::ugarchspec(
    mean.model = list(armaOrder = c(1, 1)),
    variance.model = list(garchOrder = c(1,1), model = 'sGARCH'),
    distribution.model = 'norm')
  # Replicate univariate GARCH model for each asset
  uspec <- rugarch::multispec(replicate(ncol(train_data), xspec))
  # Create DCC(1,1) specification
  spec <- rmgarch::dccspec(uspec = uspec, dccOrder = c(1, 1), distribution = 'mvnorm')
  # Create MGARCH model
  multf <- rugarch::multifit(
    uspec,
    data.frame(train_data),
    solver = "hybrid")
  # Fit MGARCH model
  fit <- rmgarch::dccfit(
    spec,
    data = train_data,
    fit.control = list(eval.se = TRUE),
    fit = multf)
  # Use model to forecast ahead
  fwdEst <- rmgarch::dccforecast(fit, n.ahead = fwd)
  # Retrieve forecasted covariance matrix
  #cov <- rmgarch::rcov(fwdEst)[1][[1]][,,fwd]
  cov <- rowSums(rmgarch::rcov(fwdEst)[[1]], dims = 2)

  # Retrieve forecasted means
  #mean <- colMeans(fitted(fwdEst)[,,1])
  mean <- colSums(fitted(fwdEst)[,,1])
  # Use forecasted means and coviarance to calculate optimal weights
  ols <- metafor::matreg(y = y, x = x, R = cov, cov = T, means = mean, n = n)
  coeff <- setNames(-ols$tab$beta, rownames(ols$tab))[-1]
  return(c(setNames(1, y), coeff))
}


# Return Calculation with Optimal Hedge Weights ---------------------------

hedgefit <- function(test_data, weights){
  # Calculate return using supplied weights
  #
  # Args:
  #   test_data: dataframe containing returns for n row periods
  #   weights: names numeric vector representing weights
  # Returns:
  #   n length array of weighted returns
  test_data <- test_data[,names(weights)]
  residual<- rowSums(test_data * weights)
  return(residual)
}

hedgefit_collar <- function(test_data, weights, collar_moneyness = c(.99, 1.01)){
  # Calculate return with hedge weights and protective return collar
  # Revere Collars return of hedge positions
  # Assumes perfect moneyness and no cost of collar (for info purpose only)
  #
  # Args:
  #   test_data: an x length numeric named array of returns
  #   weights: an x length numeric named array of weights
  #   collar_moneyness: length 2 numeric array of target moneyness of collar
  #
  # Returns:
  #   numeric representing the weighted return
  return_moneyness <- collar_moneyness - 1
  for(i in 2:length(test_data)){
    return <- test_data[i]
    if(return > return_moneyness[2]){
      test_data[i] <- return - return_moneyness[2]
    }else if(return < return_moneyness[1]){
      test_data[i] <- return - return_moneyness[1]
    }else{
      test_data[i] <- 0
    }
  }
  return(sum(test_data * weights))
}
