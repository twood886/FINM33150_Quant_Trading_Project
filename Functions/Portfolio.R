
# Portfolio (S4 Class) ----------------------------------------------------
setClass(
  "Portfolio",
  representation(
    date = "Date",
    positions = "list",
    value = "numeric",
    capital = "numeric",
    cash = "numeric"))

setMethod("secid", signature("Portfolio"), function(obj) secid(obj@positions))

# Update Portfolio Method -------------------------------------------------
setGeneric("updatePortfolio", function(portfolio, assets, ...) standardGeneric("updatePortfolio"))
setMethod("updatePortfolio", signature(obj1 = "Portfolio", obj2 = "Assets"),
  function(portfolio, assets, date){




})



setMethod("addTrades", signature(obj1 = "Portfolio", obj2 = "Trade_Target"),
  function(obj1, obj2, trade.cost){

    capital <- obj1@capital
    cash <- obj1@cash



    if(secid(obj2) %in% secid(obj1))



  }
)






setMethod("addTrades", signature(obj1 = "Portfolio", obj2 = "Trade_Targets"),
  function(obj1, obj2, trade.cost, ...){

    for(trade in obj2){



      trade <- target2trade()


    }

    capital <- obj1@capital
    cash <- obj1@cash

    for(t in obj2@trade_targets){

      if(secid(t) %in% )



    }



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

)})



setMethod("addTrades", signature(obj1 = "Portfolio", obj2 = "Trade_Target"),
  function(obj1, obj2, trade.cost){

    if(secid(obj2) %in% secid(obj1)){



    }else{




    }







  })
