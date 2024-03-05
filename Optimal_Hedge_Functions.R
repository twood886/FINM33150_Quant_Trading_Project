roll_bind <- function(acc, nxt, n){
  # Used with accumulate to create rolling data frame
  if(nrow(acc) < n){
    bind_rows(acc, nxt)
  }else{
    bind_rows(acc[-1,], nxt)
  }
}

hedgefit <- function(test_data, weights){
  # Test the fit of the optimal hedge weights
  # residual is the return of hedged portfolio
  test_data <- test_data[,names(weights)]
  residual<- rowSums(test_data * weights)
  return(residual)
}

ols_coeff <- function(train_data, y, lamda = 1){
  # Function to return the OLS coefficients for Optimal Hedging
  x = colnames(train_data)[-which(colnames(train_data)==y)]
  n <- nrow(train_data)

  #weights = (1 - lamda)^((n - 1):0)
  weights = lamda^(seq(n, 1, by = -1))
  ewma <- colSums(weights * train_data) / sum(weights)
  diff <- as.matrix(sweep(train_data, 2, ewma))
  biased_cov <- t(diff) %*% (weights * diff) / sum(weights)
  bias_correction <- sum(weights)^2 / (sum(weights)^2 - sum(weights^2))
  ewmc = bias_correction * biased_cov
  ols <- metafor::matreg(y = y, x = x, R = ewmc, cov = T, means = ewma, n = n)
  coeff <- setNames(-ols$tab$beta, rownames(ols$tab))[-1]

  return(c(setNames(1, y), coeff))
}


ols_hedgefit <- function(train_data, test_data, y, lamda = NULL){
  # Fit ols_coeff and use output in hedgefit
  if(is.null(train_data)) return(NA)
  weights <- ols_coeff(train_data, y, lamda)
  residual <- hedgefit(test_data, weights)
  return(residual)
}

hedgefit_collar <- function(test_data, weights, collar_moneyness = c(.99, 1.01)){
  # Calculate return of hedge weights with return collar
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



# Multivariate GARCH Code to be put into a function
xspec = rmgarch::ugarchspec(
  mean.model = list(armaOrder = c(1, 1)),
  variance.model = list(garchOrder = c(1,1), model = 'sGARCH'),
  distribution.model = 'norm')
uspec = multispec(replicate(4, xspec))
spec1 = dccspec(uspec = uspec, dccOrder = c(1, 1), distribution = 'mvnorm')
spec1a = dccspec(uspec = uspec, dccOrder = c(1, 1), model='aDCC', distribution = 'mvnorm')

multf = multifit(uspec, data.frame(train_data))
fit1 = dccfit(spec1, data = train_data, fit.control = list(eval.se = TRUE), fit = multf)

fwdForcast <- dccforecast(fit1, n.ahead = 1)
vcvs <- rcov(fwdForcast)

fit_adcc = dccfit(spec1, data = train_data, fit.control = list(eval.se = TRUE), fit = multf)
print(fit1)
print(fit_adcc)
