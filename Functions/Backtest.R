setClass(
  "Backtest",
  representation(
    dates = "Date",
    assets = "list",
    portfolio = "list",
    trades = "list"))

setGeneric("assets", function(backtest) standardGeneric("assets"))
setMethod("assets", "Backtest", function(backtest) backtest@assets)

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

setGeneric("assets<-", function(backtest, asset) standardGeneric("assets<-"))
setMethod("assets<-", signature(backtest = "Backtest", asset = "Asset"),
  function(backtest, asset){
    assets <- assets(backtest)
    backtest@assets <- c(assets, asset)
    return(backtest)
  })

backtest <- function(dates, trade_targets, cash_rates, initial.capital = 1000000, transaction.cost = 0){

  # Create backtest object
  BT <- new("Backtest", dates = dates)

  # Sort Trades
  trade_targets <- arrange_trades(trade_targets)

  # Create cash asset and assign it to the backtest
  if(length(cash_rates) == 1){
    rate <- setNames(rep(cash_rates, length(dates)), dates)
  }
  cash <-new("Cash", secid = "cash", dailyrate = rate)
  assets(BT) <- cash

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
