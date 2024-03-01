setClass(
  "Backtest",
  representation(
    dates = "Date",
    assets = "Assets",
    portfolio = "list",
    trades = "list"),
  prototype(
    dates = NULL,
    assets = new("Assets")))


setMethod("assets", "Backtest", function(obj) obj@assets)

setGeneric("assetids", function(backtest) standardGeneric("assetids"))
setMethod("assetids", signature(backtest = "Backtest"),
  function(backtest){
    assets <- assets(backtest)
    if(length(assets) != 0){
      secids <- sapply(assets, secid)
    }else{
      return(NULL)
    }
  })

setGeneric("assets<-", function(x, assets) standardGeneric("assets<-"))
setMethod("assets<-", signature(x = "Backtest"), function(x, assets){
    x@assets <- assets
    x
  })


backtestTradeTargets <- function(dates, trade_targets, cash_rates, initial.capital = 1000000, transaction.cost = 0){

  # Create backtest object
  BT <- new("Backtest", dates = dates)

  # Sort Trades
  trade_targets <- arrange_trades(trade_targets)

  # Create cash asset and assign it to the backtest
  if(length(cash_rates) == 1){
    rate <- setNames(rep(cash_rates, length(dates)), dates)
  }
  cash <- CashConstantRate(rate)
  BT@assets<- assets(BT) + cash

  # Create the initial cash position based on the initial capital supplied
  cash_position <- new(
    "Position",
    date = dates[1],
    secid = "cash",
    type = class(cash)[1],
    position = initial.capital,
    price = 1,
    value = initial.capital)

  # Create the initial portfolio for the backtest
  # Consists of just the cash position
  portfolio <- new(
    "Portfolio",
    date = dates[1],
    positions = c(cash_position),
    value = initial.capital,
    cash = initial.capital)

  # Make any first day trades
  firstday_trades <-


  for(date in dates[-1]){



  }



}
