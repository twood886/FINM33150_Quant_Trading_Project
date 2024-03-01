
idOptCollar <- function(
    ao_date, target_date, underlying, current_price, target_Mny_call, target_Mny_put, options_data, w = 1){

  # Subset options data to security and as of date
  ao_sec_options <-
    options_data %>%
    dplyr::filter(`date` == ao_date) %>%
    dplyr::filter(`ticker` == underlying)

  ex_dates <- ao_sec_options$exdate

  ex_before <- max(ex_dates[which(ex_dates <= target_date)])
  ex_after <- max(ex_dates[which(ex_dates > target_date)])

  w_ex_before <- as.numeric(ex_after - target_date) / as.numeric(ex_after - ex_before)
  w_ex_after <- 1 - w_ex_before

  ao_sec_options_before <- dplyr::filter(ao_sec_options, `exdate` == as.Date(ex_before))
  ao_sec_options_after <- dplyr::filter(ao_sec_options, `exdate` == as.Date(ex_after))

  calls_before <- .idOpt_stk(current_price * target_Mny_call,"C", ao_sec_options_before, -w_ex_before)
  puts_before <- .idOpt_stk(current_price * target_Mny_put,"P", ao_sec_options_before, w_ex_before)
  calls_after <- .idOpt_stk(current_price * target_Mny_call,"C", ao_sec_options_after, -w_ex_after)
  puts_after <- .idOpt_stk(current_price * target_Mny_put,"P", ao_sec_options_after, w_ex_after)

  options <-
    data.frame(
      "option" = c(calls_before$options,
                   calls_after$options,
                   puts_before$options,
                   puts_after$options),
      "weight" = w * c(calls_before$weights,
                   calls_after$weights,
                   puts_before$weights,
                   puts_after$weights)) %>%
    dplyr::filter(`weight` != 0)

  return(options)
}





.idOpt_stk <- function(target_price, side, data, w = 1){

  options_data_side <- dplyr::filter(data, `cp_flag` == side)

  strikes <- options_data_side$strike_price

  strike_below <- max(strikes[which(strikes <= target_price * 1000)])
  strike_above <- min(strikes[which(strikes > target_price * 1000)])

  w_strike_below <- (strike_above - (target_price * 1000)) / (strike_above - strike_below)
  w_strike_above <- 1 - w_strike_below

  option_below <- options_data_side %>%
    dplyr::filter(`strike_price` == strike_below) %>%
    dplyr::pull(`symbol`)

  option_above <- options_data_side %>%
    dplyr::filter(`strike_price` == strike_above) %>%
    dplyr::pull(`symbol`)

  return(
    list(
      "options" = c(option_below, option_above),
      "weights" = c(w_strike_below, w_strike_above) * w))
}

