
# Portfolio (S4 Class) ----------------------------------------------------
setClass(
  "Portfolio",
  representation(
    date = "Date",
    positions = "Positions",
    value = "numeric",
    capital = "numeric",
    cash = "numeric"))

setMethod("secid", signature("Portfolio"), function(obj) secid(obj@positions))

# Update Portfolio Method -------------------------------------------------
setGeneric("updatePortfolio", function(obj1, obj2, ...) standardGeneric("updatePortfolio"))
setMethod("updatePortfolio", signature(obj1 = "Portfolio", obj2 = "Assets"),
  function(obj1, obj2, date){

    portfolio <- new("Portfolio",
      date = date,
      positions = updatePosition(obj1@positions, obj2, date),
      cash = obj1@cash)

    portfolio@value <- sum(sapply(portfolio@positions@positions, \(x) x@value)) + portfolio@cash
    portfolio@capital <- portfolio@value
    return(portfolio)
})


setMethod("addTrades", signature(obj1 = "Portfolio", obj2 = "Trade"),
  function(obj1, obj2){
    date <- as.Date(obj1@date)
    capital <- obj1@capital
    cash <- obj1@cash

    newposition <- new("Position",
      date = date,
      secid = obj2@secid,
      type = obj2@type,
      position = obj2@number,
      price = obj2@price,
      value = obj2@dollar_amount)

    positions <- obj1@positions + newposition

    new_portfolio <- new("Portfolio",
        date = date,
        positions = positions,
        cash = cash - obj2@dollar_amount - obj2@trade.cost,
        capital = capital - obj2@trade.cost)

    new_portfolio@value <-  sum(sapply(new_portfolio@positions@positions, \(x) x@value)) + new_portfolio@cash

    return(new_portfolio)})
