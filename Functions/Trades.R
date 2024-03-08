# Trades ------------------------------------------------------------------
# The following classes and methods are for "Trade" S4 Objects
# "Portfolio" objects can be de-accumulated into a collection of "Trade" obejects

# Trade (S4 Class) --------------------------------------------------------
setClass(
  "Trade",
  representation(
    date = "Date",
    secid = "character",
    dollar_amount = "numeric",
    number = "numeric",
    side = "character",
    trade.cost = "numeric",
    trade.price = "numeric",
    price = "numeric",
    type = "character"))

setClass(
  "Trades",
  representation(trades = "list"),
  prototype(trades = NULL),
  validity = function(object){
    all(sapply(object@trades, \(x) c("Trade") %in% is(x)))})

setMethod("+", signature(e1 = "Trades", e2 = "Trade"),
  function(e1, e2) return(new("Trades",trades = c(e1@trades, e2))))



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
    target_type = "character",
    target_amount = "numeric",
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
    asset = "Equity"),
  contains = "Trade_Target")

setClass(
  "Trade_Target_Option",
  representation(
    asset = "Option",
    cost = "numeric"),
  contains = "Trade_Target")

# TradeTarget for Equity objects sets the trade amount
setMethod("newTradeTarget", signature(obj = "Equity"),
  function(obj, trade_date, target_amount, target_type, side, price_type = "close", ...){

    if(price_type == "close"){
      price <- obj@close[as.character(trade_date)]
    }

    return(
      new("Trade_Target_Equity",
          trade_date = trade_date,
          secid = secid(obj),
          asset = obj,
          target_type = target_type,
          target_amount = target_amount,
          price = price,
          side = side))})

setMethod("newTradeTarget", signature(obj = "Option"),
  function(obj, trade_date, target_amount, target_type){

    max_date <- max(as.Date(names(obj@best_mid)))
    if(trade_date > max_date){
      trade_date <- max_date
    }

    underlying_price <- obj@underlying_close[[as.character(trade_date)]]


    if(target_type == "Close"){
      pct <- -(target_amount)
      if(target_amount > 0){
        target_amount <- -1
      }else{
        target_amount <- 1
      }
    }else{
      pct <- target_amount
    }

    if(trade_date == as.Date(obj@exdate)){
      if("Option_Put" %in% is(obj)){
        price <- max(0, (obj@strike_price - underlying_price))
      }else if("Option_Call" %in% is(obj)){
        price <- max(0, (underlying_price - obj@strike_price))
      }else{
        price <- obj@best_mid[[as.character(trade_date)]]
      }
    }else{
      price <- obj@best_mid[[as.character(trade_date)]]
    }

    if(pct > 0){
      side = "Buy"
    }else{
      side = "Sell"
    }

    return(
      new("Trade_Target_Option",
          trade_date = trade_date,
          secid = secid(obj),
          asset = obj,
          side = side,
          price = price,
          target_type = target_type,
          target_amount = as.numeric(target_amount),
          cost = as.numeric(pct) * price / underlying_price)
    )
  })




setMethod("assets", signature(obj = "Trade_Target"), function(obj) obj@asset)
setMethod("getTradeDate", signature(obj = "Trade_Target"), function(obj) as.Date(obj@trade_date, "1970-01-01"))
setMethod("secid", signature(obj = "Trade_Target"), function(obj) obj@secid)
setMethod("trade_type", signature(obj = "Trade_Target"), function(obj) obj@target_type)

setMethod("secid", signature(obj = "Trade_Targets"),
  function(obj) sapply(obj@trade_targets, secid))

setMethod("getTradeDate", signature(obj = "Trade_Targets"),
  function(obj){
    as.Date(sapply(obj@trade_targets, getTradeDate), "1970-01-01")})

setMethod("trade_type", signature(obj = "Trade_Targets"),
  function(obj){
    sapply(obj@trade_targets, trade_type)
  })



setMethod("target2trade", signature(obj = "Trade_Target_Equity"),
  function(obj, amount=1, trade.cost){

    trade_date <- obj@trade_date
    trade_price <- as.numeric(obj@price)

    if(obj@target_type == "Open"){
      trade_share_number <- (amount * obj@target_amount) %/% (trade_price * (1+trade.cost))
    }else if(obj@target_type == "Dollar"){
      tgt_trade_dollar_amount <- (obj@target_amount)
      if(tgt_trade_dollar_amount >0){
        trade_share_number <- (tgt_trade_dollar_amount) %/% (trade_price * (1+trade.cost))
      }else{
        trade_share_number <- (tgt_trade_dollar_amount) %/% (trade_price / (1+trade.cost))
      }
    }

    trade_dollar_amount <- trade_share_number * trade_price
    trade_cost <- abs(trade_dollar_amount) * trade.cost

    new("Trade",
      date = obj@trade_date,
      secid = secid(obj),
      dollar_amount = as.numeric(trade_dollar_amount),
      side = obj@side,
      trade.cost = as.numeric(trade_cost),
      number = as.numeric(trade_share_number),
      trade.price = trade_price,
      price = price(obj@asset, trade_date),
      type = is(obj@asset)[[1]])
    })




setMethod("target2trade", signature(obj = "Trade_Target_Option"),
  function(obj, amount, trade.cost, options.current = 0){

    trade_date <- obj@trade_date
    trade_price <- as.numeric(obj@price)
    underlying_price <- obj@asset@underlying_close[[as.character(trade_date)]]

    if(obj@target_type == "Open"){
      tgt_trade_dollar_amount = (obj@target_amount * amount)
      trade_option_number = tgt_trade_dollar_amount %/% underlying_price %/% 100 * 100
    }else if(obj@target_type == "Close"){
      trade_option_number <- -options.current
    }

    trade_dollar_amount <- trade_option_number * trade_price
    trade_cost <- abs(trade_dollar_amount) * trade.cost

    new("Trade",
      date = obj@trade_date,
      secid = secid(obj),
      dollar_amount = trade_dollar_amount,
      side = obj@side,
      trade.cost = trade_cost,
      number = trade_option_number,
      trade.price = trade_price,
      price = price(obj@asset, trade_date),
      type = is(obj@asset)[[1]])
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






