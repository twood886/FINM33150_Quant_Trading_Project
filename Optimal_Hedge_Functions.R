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
  test_data <- test_data[,names(weights)]
  residual<- rowSums(test_data * weights)
  return(residual)
}

ols_coeff <- function(train_data, y, weighted = F, lamda = .9){
  # Function to return the OLS coefficients for Optimal Hedging
  x = colnames(train_data)[-which(colnames(train_data)==y)]
  n <- nrow(ETF_Returns)

  if(weighted == F){
    formula <- paste(y, "~", paste(x,collapse="+"))
    ols <- lm(formula, train_data)
    coeff <- -ols$coefficients[-1]
  }else{
    n <- nrow(train_data)
    #weights = (1 - lamda)^((n - 1):0)
    weights = lamda^(seq(n, 1, by = -1))
    ewma <- colSums(weights * train_data) / sum(weights)
    diff <- as.matrix(sweep(train_data, 2, ewma))
    biased_cov <- t(diff) %*% (weights * diff) / sum(weights)
    bias_correction <- sum(weights)^2 / (sum(weights)^2 - sum(weights^2))
    ewmc = bias_correction * biased_cov
    ols <- metafor::matreg(y = y, x = x, R = ewmc, cov = T, means = ewma, n = n)
    one <- setNames(1, y)
    coeff <- setNames(-ols$tab$beta, rownames(ols$tab))[-1]
  }
  one <- setNames(1, y)
  coeff_one <- c(one, coeff)
  return(coeff)
}

ols_hedgefit <- function(train_data, test_data, y, weighted = F){
  if(is.null(train_data)) return(NA)
  weights <- ols_coeff(train_data, y, weighted)
  residual <- hedgefit(test_data, weights)
  return(residual)
}
