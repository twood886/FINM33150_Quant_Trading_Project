
# Portfolio (S4 Class) ----------------------------------------------------
setClass(
  "Portfolio",
  representation(
    date = "Date",
    positions = "list",
    value = "numeric",
    capital = "numeric",
    cash = "numeric"))



# Update Portfolio Method -------------------------------------------------
setGeneric("updatePortfolio", function(portfolio, ...) standardGeneric("updatePortfolio"))
setMethod("updatePortfolio", signature(portfolio = "Portfolio"),
  function(portfolio, assets, ))




setGeneric("addtrade", function(portfolio, trade, ...) standardGeneric("addtrade"))

setMethod("addtrade", signature(portfolio = "Portfolio_Periods", trade = "Trade_Target"),
  function(portfolio, trade, trade.cost, ...){

    if()

    trade_price <- trade@price
    tgt_trade_dollar_amount <- (trade@target_amount * portfolio@value) / (1 + trade.cost)
    trade_share_number <- tgt_trade_dollar_amount %/% trade_price
    trade_dollar_amount <- trade_share_number * trade_price
    trade_commission <- trade_dollar_amount * trade.cost



    return(
      new(
        "Portfolio_Period",
        date = portfolio@date,


    )

