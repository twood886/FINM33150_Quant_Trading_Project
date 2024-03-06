
.idOpt_stk <- function(target_price, side, data, w = 1){
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
  srike_range <- strike_above - strike_below
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


idOptCollar <- function(options_data, ao_date, ex_date, underlying, tgt_mny,
  w = 1, current_price = NULL){
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
    current_price <- options_data %>%
      filter(`ticker` == underlying) %>%
      filter(`date` == ao_date) %>%
      pull(`underlying_price`) %>%
      pluck(1)}

  # Subset options data for underlying and as of date
  ao_sec_options <-
    options_data %>%
    dplyr::filter(`date` == ao_date) %>%
    dplyr::filter(`ticker` == underlying) %>%
    dplyr::filter(`ss_flag` == 0)

  # Find expiration dates before and after ex_date
  # Find weights for expiration dates before and after and weight to weighted
  # average is equal to ex_date
  ex_dates <- ao_sec_options$exdate
  ex_before <- max(ex_dates[which(ex_dates <= ex_date)])
  ex_after <- max(ex_dates[which(ex_dates > ex_date)])
  ex_range <- as.numeric(ex_after - ex_before)
  w_ex_before <- as.numeric(ex_after - ex_date) / ex_range
  w_ex_after <- 1 - w_ex_before

  # Find options for the before and after ex_date
  ao_sec_options_before <- dplyr::filter(ao_sec_options, `exdate` == as.Date(ex_before))
  ao_sec_options_after <- dplyr::filter(ao_sec_options, `exdate` == as.Date(ex_after))

  # Find the options for the collar
  tgt_strike_call <- current_price * tgt_mny[[2]]
  tgt_strike_put <- current_price * tgt_mny[[1]]
  calls_before <- .idOpt_stk(tgt_strike_call, "C", ao_sec_options_before, -w_ex_before * w)
  puts_before <- .idOpt_stk(tgt_strike_put, "P", ao_sec_options_before, w_ex_before * w)
  calls_after <- .idOpt_stk(tgt_strike_call, "C", ao_sec_options_after, -w_ex_after * w)
  puts_after <- .idOpt_stk(tgt_strike_put, "P", ao_sec_options_after, w_ex_after * w)

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

  return(options)
}

option_collar <- function(weights, ao_date, ex_date, options_data,
  tgt_mny = c(.99, 1.01)){
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
  weights <- weights[-1]
  # Find collars for securities
  options <- map2(
    weights,
    names(weights),
    ~idOptCollar(
      options_data_u, ao_date, ex_date, .y, tgt_mny, .x))
  return(bind_rows(options))
}
