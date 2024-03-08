setClass(
  "Backtest",
  representation(
    dates = "Date",
    assets = "Assets",
    portfolio = "list",
    trades = "Trades"),
  prototype(
    dates = NULL,
    assets = new("Assets"),
    trades = new("Trades")))


setMethod("assets", "Backtest", function(obj) obj@assets)

setMethod("secid", signature(obj = "Backtest"),
  function(obj){
    a <- assets(obj)
    if(length(obj) != 0){
      return(sapply(obj, secid))
    }else{
      return(NULL)
    }
  })

setMethod("+", signature(e1 = "Backtest", e2 = "Trade"),
  function(e1, e2){
    e1@trades <- e1@trades + e2
    return(e1)})




backtestTradeTargets <- function(dates, trade_targets, cash_rates, initial.capital = 1000000, transaction.cost = 0){
  # Create backtest object
  BT <- new("Backtest", dates = dates, assets = assets(trade_targets))
  for(dateloc in 1:length(dates)){
    date <- dates[[dateloc]]
    print(date)
    if(dateloc == 1){
      portfolio <- new(
        "Portfolio",
        date = date,
        capital = initial.capital,
        value = initial.capital,
        cash = initial.capital)

      BIZD_buy_loc <- which(
        secid(trade_targets)== "BIZD" &
        getTradeDate(trade_targets) == date)

      BIZD_buy_tgt <- trade_targets@trade_targets[[BIZD_buy_loc]]
      BIZD_trade <- target2trade(BIZD_buy_tgt, portfolio@cash, transaction.cost)
      portfolio <- addTrades(portfolio, BIZD_trade)
      BT@trades <- BT@trades + BIZD_trade

    }else{
      portfolio <- updatePortfolio(BT@portfolio[[dateloc - 1]], BT@assets, date)
    }

    option_close_loc <- which(
      secid(trade_targets) != "BIZD" &
      getTradeDate(trade_targets) == date &
      trade_type(trade_targets) == "Close")
    if(length(option_close_loc)!=0){
      option_close_target <- trade_targets@trade_targets[option_close_loc]
      for(option in option_close_target){
        current_pos <- portfolio@positions@positions[[which(secid(option) == secid(portfolio))]]
        trade <- target2trade(
          option,
          amount =1,
          trade.cost = transaction.cost,
          options.current = current_pos@position)
        portfolio <- addTrades(portfolio, trade)
        BT@trades <- BT@trades + trade
      }
      BIZD_trade_tgt <- newTradeTarget(
        BIZD,
        trade_date = date,
        target_amount = portfolio@cash,
        target_type = "Dollar",
        side = ifelse(portfolio@cash > 0, "Buy", "Sell"))
      BIZD_trade <- target2trade(BIZD_trade_tgt, trade.cost = transaction.cost)
      portfolio <- addTrades(portfolio, BIZD_trade)
      BT@trades <- BT@trades + BIZD_trade
    }

    option_open_loc <- which(
      secid(trade_targets) != "BIZD" &
      getTradeDate(trade_targets) == date &
      trade_type(trade_targets) == "Open")

    if(length(option_open_loc)!=0){
      option_open_target <- trade_targets@trade_targets[option_open_loc]
      option_open_costs <- sapply(option_open_target, \(x) x@cost)
      option_open_cost_tot <- sum(option_open_costs + abs(option_open_costs * transaction.cost))

      # Sell BIZD to buy
      existingBIZD <- value(portfolio@positions@positions[[which(secid(portfolio@positions)=="BIZD")]])
      BIZD <- BT@assets@assets[[which(BT@assets@secids == "BIZD")]]
      BIZD_trade_tgt <- newTradeTarget(
        BIZD,
        trade_date = date,
        target_amount = existingBIZD * -option_open_cost_tot * (1 + transaction.cost),
        target_type = "Dollar",
        side = ifelse(option_open_cost_tot > 0, "Sell", "Buy"))
      BIZD_trade <- target2trade(BIZD_trade_tgt, trade.cost = transaction.cost)
      portfolio <- addTrades(portfolio, BIZD_trade)
      BT@trades <- BT@trades + BIZD_trade
      existingBIZD <- value(portfolio@positions@positions[[which(secid(portfolio@positions)=="BIZD")]])

      for(option in option_open_target){
        trade <- target2trade(option, existingBIZD, transaction.cost)
        portfolio <- addTrades(portfolio, trade)
        BT@trades <- BT@trades + trade
      }
    }
    BT@portfolio <- c(BT@portfolio, portfolio)
  }
  return(BT)
}
