setClass(
  "Backtest",
  representation(
    dates = "Date",
    assets = "Assets",
    portfolio = "list",
    trades = "Trade_Targets"),
  prototype(
    dates = NULL,
    assets = new("Assets"),
    trades = new("Trade_Targets")))


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







backtestTradeTargets <- function(dates, trade_targets, cash_rates, initial.capital = 1000000, transaction.cost = 0){
  dates <- sort(dates)
  # Create backtest object
  BT <- new("Backtest", dates = dates, assets = assets(trade_targets), trades = trade_targets)

  # # Create cash asset and assign it to the backtest
  # if(length(cash_rates) == 1){
  #   rate <- setNames(rep(cash_rates, length(dates)), dates)
  # }
  # cash <- CashConstantRate(rate)
  # BT@assets<- assets(BT) + cash

  # Create the initial cash position based on the initial capital supplied
  # cash_position <- new(
  #   "Position",
  #   date = date,
  #   secid = "cash",
  #   type = class(cash)[1],
  #   position = initial.capital,
  #   price = 1,
  #   value = initial.capital)
  #
  # portfolio <- new(
  #   "Portfolio",
  #   date = dates[1],
  #   capital = initial.capital,
  #   value = initial.capital,
  #   cash = initial.capital)



  # option_trade_tgt_first <-
  #   trade_targets@trade_targets[which(secid(trade_targets)!= "BIZD" & getTradeDate(trade_targets) == dates[1])]
  #
  # cost_option_trade_tgt_first <- -sum(sapply(option_trade_tgt_first, \(x) x@cost))
  #
  # # Make first BIZD Trade
  # firstday_trades_BIZD <- which(secid(secid(trade_targets) == "BIZD") & )



  for(dateloc in 1:length(dates)){
    date <- dates[[dateloc]]

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
      }

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
      existingBIZD <- value(portfolio@positions@positions[[which(secid(portfolio@positions)=="BIZD")]])

      for(option in option_open_target){
        trade <- target2trade(option, existingBIZD, transaction.cost)
        portfolio <- addTrades(portfolio, trade)
      }
    }


    BT@portfolio <- c(BT@portfolio, portfolio)

  }
}
