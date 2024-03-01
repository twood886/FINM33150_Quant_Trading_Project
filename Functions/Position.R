setClass(
  "Position",
  representation(
    date = "Date",
    secid = "character",
    type = "character",
    position = "numeric",
    price = "numeric",
    value = "numeric"))


setGeneric("value", function(object) standardGeneric("value"))
setMethod("value", signature(object = "Position", function(object) object@value))


# Update Position Method --------------------------------------------------
# The following methods are used to update the position values
setGeneric("updatePosition", function(date, position, asset, ...) standardGeneric("updatePosition"))

setMethod("updatePosition", signature(date = "Date", position = "Position", asset = "Cash"),
  function(date, position, asset, ...){

    updated_position_value <- position@position * (1 + asset@dailyrate[as.character(date)])
    new_position <- new("Position",
      date = date,
      secid = position@secid,
      type = position@type,
      position = updated_position_value,
      price = 1,
      value = updated_position_value)

    return(new_position)
  })

setMethod("updatePosition", signature(date = "Date", position = "Position", asset = "Equity"),
  function(date, position, equity, invest_dividend = T...){

    price_close <- equity@close[as.character(date)]

    # Need to add split logic
    # Invest dividends
    dividend <- replace_na(equity@dividend[as.character(date)], 0)
    position_chg <- (position@position * dividend) / price_close

    new_position <- new(
      "Position",
      date = date,
      secid = equity@secid,
      type = "Equity",
      position = position@position + position_chg,
      price = price_close)

    new_position@value <- new_position@position * new_position@price
    return(new_position)
  })
