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
    number = "numeric",
    side = "character",
    trade.cost = "numeric"))

setClass(
  "Trades",
  representation(trades = "list"),
  prototype(trades = NULL),
  validity = function(object){
    all(sapply(object@trades), \(x) c("Trade") %in% is(x))})

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
    asset = "Asset",
    side = "character",
    price = "numeric"))

setClass(
  "Trade_Targets",
  representation(trade_targets = "list"),
  prototype(trade_targets = NULL),
  validity = function(object){
    all(sapply(object@trade_targets, \(x) c("Trade_Target") %in% is(x)))})

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

setMethod("assets", signature(obj = "Trade_Target"), function(obj) obj@asset)
setMethod("getTradeDate", signature(obj = "Trade_Target"), function(obj) as.Date(obj@trade_date, "1970-01-01"))
setMethod("secid", signature(obj = "Trade_Target"), function(obj) obj@secid)

setGeneric("target2trade", function(obj, ...) standardGeneric("target2trade"))
setMethod("target2trade", signature(obj = "Trade_Target_Equity"),
  function(obj, capital, trade.cost, shares.current = 0){

    trade_date <- obj@trade_date
    trade_price <- obj@price

    if(shares.current !=0){
      trade_share_number <- shares.current %/% (1/obj@target_amount)
    }else{
      tgt_trade_dollar_amount <- (obj@target_amount * capital) / (1 + trade.cost)
      trade_share_number <- tgt_trade_dollar_amount %/% trade_price
    }

    trade_dollar_amount <- trade_share_number * trade_price
    trade_cost <- trade_dollar_amount * trade.cost

    new("Trade",
      date = obj@trade_date,
      secid = secid(obj),
      asset = obj@asset,
      dollar_amount = trade_dollar_amount,
      side = obj@side,
      trade.cost = trade_cost,
      number = trade_shares_number)
    })

setMethod("target2trade", signature(obj = "Trade_Target_Option"),
  function(obj, amount, trade.cost, options.current = 0){

    trade_date <- obj@trade_date
    trade_price <- obj@price
    underlying_price <- obj@asset@underlying_close

    if(obj@target_type == "pct"){
      tgt_trade_dollar_amount = (obj@target_amount * amount)
      trade_option_number = tgt_trade_dollar_amount %/% underlying_price %/% 100 * 100
    }else if(obj@target_type == "pct.current"){
      trade_option_number <- options.current %/% (1/obj@target_amount)
    }

    trade_dollar_amount <- trade_option_number * trade_price
    trade_cost <- trade_dollar_amount * trade.cost

    new("Trade",
      date = obj@trade_date,
      secid = secid(obj),
      asset = obj@asset,
      dollar_amount = trade_dollar_amount,
      side = obj@side,
      trade.cost = trade_cost,
      number = trade_option_number)
  })

setMethod("arrange", signature(obj = "Trade_Targets"),
  function(obj){
    side.factor <- factor(c("Buy", "Sell"), ordered = T)
    dates <- getTradeDate(obj)
    sides <- lapply(obj@trade_targets, \(x) x@side)
    sides_factor <- factor(sides, levels = c("Sell", "Buy"), ordered = T)
    m <-
      dplyr::tibble(
        "index" = seq(1, length(obj@trade_targets)),
        "date" = dates,
        "side" = sides_factor) %>%
      dplyr::arrange(`date`, `side`) %>%
      dplyr::pull(`index`)
    t <- new("Trade_Targets", trade_targets = obj@trade_targets[m])
    return(t)
  })

setMethod("+", signature(e1 = "Trade_Targets", e2 = "Trade_Target"),
  function(e1, e2){
    t <- new("Trade_Targets",trade_targets = c(e1@trade_targets, e2))
    t <- arrange(t)
    return(t)
  })

setMethod("+", signature(e1 = "Trade_Target", e2 = "Trade_Targets"),
  function(e1, e2){
    t <- new("Trade_Targets",trade_targets = c(e1, e2@trade_targets))
    t <- arrange(t)
    return(t)
  })

setMethod("assets", signature(obj = "Trade_Targets"),
  function(obj){
    a <- lapply(obj@trade_targets, assets)
    b <- new("Assets")
    for(x in a){
      b <- b + x
    }
    return(b)
  })

setMethod("getTradeDate", signature(obj = "Trade_Targets"),
  function(obj){
    return(as.Date(sapply(obj@trade_targets, getTradeDate), "1970-01-01"))
  })


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
          asset = obj,
          price = price,
          target_type = "pct.capital",
          target_amount = pct_capital,
          side = side))})

setMethod("newTradeTarget", signature(obj = "Option"),
  function(obj, trade_date, pct = 1, trade_type){

    trade_date <- max(as.Date(names(obj@best_mid)))

    if(trade_type == "Close"){
      pct <- -(pct/abs(pct))
    }

    if(pct > 0){
      side = "Buy"
      price <- obj@best_offer[[as.character(trade_date)]]
    }else{
      side = "Sell"
      price <- obj@best_bid[[as.character(trade_date)]]
    }

    return(
      new("Trade_Target_Option",
        trade_date = trade_date,
        secid = secid(obj),
        asset = obj,
        side = side,
        price = price,
        target_type = trade_type,
        target_amount = as.numeric(pct))
      )
  })


