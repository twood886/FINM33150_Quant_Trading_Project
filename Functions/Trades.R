# Trades ------------------------------------------------------------------
# The following classes and methods are for "Trade" S4 Objects
# "Portfolio" objects can be de-accumulated into a collection of "Trade" obejects

# Trade (S4 Class) --------------------------------------------------------
setClass(
  "Trade",
  representation(
    date = "Date",
    secid = "character",
    asset = "Asset",
    dollar_amount = "numeric",
    side = "character"))

setClass(
  "Trade_Equity",
  representation(
    shares = "numeric"),
  contains = "Trade")


# # Trade_Target (S4 Class) -----------------------------------------------
# The following classes and methods are for "Trade_Target" S4 Objects
# Trade_Target Objects are constructed with the underlying assets trades, and
# the target portfolio weights / exposures.
# When used with generatePortfolio functions and a starting capital amount, the
# "Trade_Targets" are accumulated to generate "Portfolio" objects

setClass(
  "Trade_Target",
  representation(
    trade_date = "Date",
    secid = "character",
    side = "character",
    price = "numeric"))

setClass(
  "Trade_Target_Equity",
  representation(
    asset = "Equity",
    target_type = "character",
    target_amount = "numeric"),
  contains = "Trade_Target")

setClass(
  "Trade_Target_Option",
  representation(
    asset = "Option",
    target_type = "character",
    target_amount = "numeric"),
  contains = "Trade_Target")

setClass(
  "Trade_Target_Option_Collar",
  representation(
    asset = "Option_Collar",
    delta_amount = "numeric"),
  contains = "Trade_Target")


# TradeTarget for Equity objects sets the trade amount
setMethod("newTradeTarget", signature(obj = "Equity"),
  function(obj, trade_date, side, pct_capital, price_type = "close", ...){

    if(price_type == "close"){
      price <- obj@close[as.character(trade_date)]
    }

    return(
      new("Trade_Target_Equity",
          trade_date = trade_date,
          secid = secid(obj),
          asset = asset,
          price = price,
          target_type = "pct.capital",
          target_amount = pct_capital,
          side = side))})


arrange_trades <- function(trades){

  side.factor <- factor(c("Buy", "Sell"), ordered = T)

  dates <- as.Date(unlist(lapply(trades, \(x) as.character(x@trade_date))))
  sides <- lapply(trades, \(x) x@side)
  sides_factor <- factor(sides, levels = c("Sell", "Buy"), ordered = T)

  m <-
    dplyr::tibble(
      "index" = seq(1, length(trades)),
      "date" = dates,
      "side" = sides_factor) %>%
    dplyr::arrange(`date`, `side`) %>%
    dplyr::pull(`index`)

  return(trades[m])

}


