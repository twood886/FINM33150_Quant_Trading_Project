
idOpt_stk <- function(target_price, side, data, w = 1){
  # Internal function to find option for given target price
  # Will find option with strike price above and below target and weight them
  # Used in idOptCollar only
  #
  # Args:
  #   target_price: numeric representing target stike price
  #   side: character "C" or "P" representing call or put
  #   data: options data for a given security on a given date
  #   w: numeric representing weights

  # Filter data for call or puts
  options_data_side <- dplyr::filter(data, `cp_flag` == side)
  # Get availble strikes
  strikes <- options_data_side$strike_price

  # Find strike immediatly below and above the target strike price
  strike_below <- max(strikes[which(strikes <= target_price * 1000)])
  strike_above <- min(strikes[which(strikes > target_price * 1000)])

  # Weight for the options above and below target strike so weighted average
  # strike is equal to strike price
  strike_range <- strike_above - strike_below
  w_strike_below <- (strike_above - (target_price * 1000)) / strike_range
  w_strike_above <- 1 - w_strike_below

  # Find option below target strike price
  option_below <- options_data_side %>%
    dplyr::filter(`strike_price` == strike_below) %>%
    dplyr::pull(`symbol`)
  # Find option above target strike price
  option_above <- options_data_side %>%
    dplyr::filter(`strike_price` == strike_above) %>%
    dplyr::pull(`symbol`)
  # Return a list with option id and weights
  return(
    list(
      "options" = c(option_below, option_above),
      "weights" = c(w_strike_below, w_strike_above) * w))
}

idOpt_stk2 <- function(target_price, side, data,  w = 1){

  options_data_side <- dplyr::filter(data, `cp_flag` == side)
  strikes <- options_data_side$strike_price

  strike_first <- min(strikes[which(strikes >= target_price * 1000)])
  strike_second <- min(strikes[which(strikes > strike_first)])

  w_strike_first <- 1
  w_strike_second <- ((target_price * 1000) - strike_first) / strike_second

  option_first <- options_data_side %>%
    dplyr::filter(`strike_price` == strike_first) %>%
    dplyr::pull(`symbol`)

  option_second <- options_data_side %>%
    dplyr::filter(`strike_price` == strike_second) %>%
    dplyr::pull(`symbol`)

  return(
    list(
      "options" = c(option_first, option_second),
      "weights" = c(w_strike_first, w_strike_second) * w))
}


idOptCollar <- function(data, ao_date, ex_date, underlying, tgt_mny,
  w = 1, current_price = NULL, option_type = 1, buy_only = F){
  # Function to find option collar for given security with target moneyness
  # Finds calls and puts with strikes immediately above and below target strike
  # and weights them to so that the weighted average strike is equal to target
  #
  # Args:
  #   options_data: dataframe with options data from WRDS
  #   ao_date: date representing as of date
  #   ex_date: date representing the target date for expiry of options
  #   underlying: string representing the underlying security
  #   tgt_mny: numeric vector length 2 representing tgt put and call moneyness
  #   w: numeric representing weight for hedge
  #   current_price: price of underlying. will pull from options_data if null
  #
  # Returns:
  #   2 column data frame with "option" name and "weight"

  # Find current underlying price if not provided
  # If current_price is provides, options will be found with moneyness relative
  if(is.null(current_price)){
    current_price <- data %>%
      filter(`ticker` == underlying) %>%
      filter(`date` == ao_date) %>%
      pull(`underlying_price`) %>%
      pluck(1)}

  # Subset options data for underlying and as of date
  ao_sec_options <-
    data %>%
    dplyr::filter(`date` == ao_date) %>%
    dplyr::filter(`ticker` == underlying) %>%
    dplyr::filter(`ss_flag` == 0)

  # Find expiration dates before and after ex_date
  # Find weights for expiration dates before and after and weight to weighted
  # average is equal to ex_date
  ex_dates <- ao_sec_options$exdate
  ex_before <- max(ex_dates[which(ex_dates <= ex_date)])
  ex_after <- min(ex_dates[which(ex_dates > ex_date)])
  ex_range <- as.numeric(ex_after - ex_before)
  w_ex_before <- as.numeric(ex_after - ex_date) / ex_range
  w_ex_after <- 1 - w_ex_before

  # Find options for the before and after ex_date
  ao_sec_options_before <- dplyr::filter(ao_sec_options, `exdate` == as.Date(ex_before))
  ao_sec_options_after <- dplyr::filter(ao_sec_options, `exdate` == as.Date(ex_after))

  # Find the options for the collar
  tgt_strike_call <- current_price * tgt_mny[[2]]
  tgt_strike_put <- current_price * tgt_mny[[1]]


  calls_before <- idOpt_stk(tgt_strike_call, "C", ao_sec_options_before, w_ex_before * w)
  puts_before <- idOpt_stk(tgt_strike_put, "P", ao_sec_options_before, w_ex_before * -w)
  calls_after <- idOpt_stk(tgt_strike_call, "C", ao_sec_options_after, w_ex_after * w)
  puts_after <- idOpt_stk(tgt_strike_put, "P", ao_sec_options_after, w_ex_after * -w)


  # Combine the options in dataframe
  options <-
    data.frame(
      "option" = c(calls_before$options,
                   calls_after$options,
                   puts_before$options,
                   puts_after$options),
      "weight" = c(calls_before$weights,
                   calls_after$weights,
                   puts_before$weights,
                   puts_after$weights)) %>%
    dplyr::filter(`weight` != 0)

  if(buy_only == T){
    options <- dplyr::filter(options, `weight` < 0)
  }
  return(options)
}

option_collar <- function(w, ao_date, ex_date, data,
  tgt_mny = c(.99, 1.01), option_type = 1, buy_only = F){
  # Wrapper function to find collar options for multiple securities
  #
  # Args:
  #   weights: named numeric vector of hedge weights for underlying
  #   options_data: dataframe with options data from WRDS
  #   ao_date: date representing as of date
  #   ex_date: date representing the target date for expiry of options
  #   underlying: string representing the underlying security
  #   tgt_mny: numeric vector length 2 representing tgt put and call moneyness

  # Remove first weight (BIZD = 1)
  w <- w[-1]
  # Find collars for securities
  options <- purrr::map2(
    w,
    names(w),
    \(x, y) idOptCollar(
      data = data,
      ao_date = ao_date,
      ex_date = ex_date,
      underlying = y,
      tgt_mny = tgt_mny,
      w = x,
      option_type = option_type,
      buy_only=buy_only))
  return(bind_rows(options))
}

minimize_delta <- function(calls, puts, current_price, target_moneyness, hedge_ratio) {
  # Hedge ratio is percentage of hedge per ticker and total_contracts is the total number of contracts to be traded
  total_contracts <- hedge_ratio * 100

  # Initialize the Gurobi model
  model <- gurobi::gurobi_model(env = NULL)

  # Reference vars
  num_calls <- nrow(calls)
  df <- rbind(calls, puts)
  n <- nrow(df)

  # Define the integer variables for weights with max value of total_contracts
  # R's gurobi package uses a different syntax for adding variables
  for (i in 1:n) {
    model$addVar(vtype = "I", name = paste0('weights', i), lb = 0, ub = total_contracts)
  }

  # Define moneyness of calls, puts
  call_moneyness <- (calls$strike_price - current_price) / current_price
  put_moneyness <- (current_price - puts$strike_price) / current_price
  moneyness <- c(call_moneyness, put_moneyness)

  # Create vars for the weighted delta sum and moneyness deviation sum
  # R's gurobi package uses a different syntax for adding constraints and variables
  model$addVar(name = 'deviation_sum', lb = 0)
  model$addVar(name = 'delta_sum', lb = 0)

  # Define delta_sum, deviation_sum
  # Constraints need to be added using the 'addConstr' function in R's gurobi package
  model$addConstr(deviation_sum == sum(mapply(function(weight, money) weight * (money - target_moneyness),
                                              model$getVarByName(paste0('weights', 1:n)), moneyness)), name = "Def_deviation_sum")
  model$addConstr(delta_sum == 100*total_contracts + 100*(-sum(mapply(function(weight, delta) weight * delta,
                                                                      model$getVarByName(paste0('weights', 1:num_calls)), df$delta[1:num_calls])) +
                                                            sum(mapply(function(weight, delta) weight * delta,
                                                                       model$getVarByName(paste0('weights', (num_calls+1):n)), df$delta[(num_calls+1):n]))), name = "Def_delta_sum")

  # Add constraint to ensure the sum of the weights is the number of total contracts defined by the hedge ratio
  model$addConstr(sum(model$getVarByName(paste0('weights', 1:num_calls))) == total_contracts, name = 'call_weight_sum')
  model$addConstr(sum(model$getVarByName(paste0('weights', (num_calls+1):n))) == total_contracts, name = 'put_weight_sum')

  # Set the objective to minimize the delta and deviation_sum
  # The 'setObjective' function has different syntax in R
  model$setObjective(delta_sum^2 + deviation_sum, "minimize")

  # Optimize the model
  result <- gurobi(model, params = list(OutputFlag = 0))

  # Check if the optimization was successful
  if (result$status == "OPTIMAL") {
    # Retrieve the optimized weights
    optimized_weights <- sapply(1:n, function(i) result$x[i])
    df$weight <- optimized_weights
    df$moneyness <- moneyness
    df$total_delta <- 100 * optimized_weights * df$delta
    return(df[, c('date', 'ticker', 'cp_flag', 'strike_price', 'exdate', 'mid_price', 'weight', 'moneyness', 'total_delta')])
  }

  if (result$status %in% c("INF_OR_UNBD", "INFEASIBLE")) {
    print('Model is infeasible')}}

