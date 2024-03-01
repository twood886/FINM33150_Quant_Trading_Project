# Position (S4 Class) -----------------------------------------------------
setClass(
  "Position",
  representation(
    date = "Date",
    secid = "character",
    type = "character",
    position = "numeric",
    price = "numeric",
    value = "numeric"))

setMethod("value", signature(object = "Position", function(object) object@value))
setMethod("secid", signature(obj = "Position"), function(obj) obj@secid)


# Positions (S4 Class) ----------------------------------------------------
setClass(
  "Positions",
  representation(positions = "list"),
  prototype(positions = NULL),
  validity = function(object){
    all(sapply(object@positions, \(x) c("Position") %in% is(x)))})
















# Update Position Method --------------------------------------------------
# The following methods are used to update the position values
setMethod("updatePosition", signature(date = "Date", obj1 = "Position", obj2 = "Assets"),
  function(date, obj1, obj2, ...){
    position_asset <- assets(obj2)[[which(secid(obj2) == secid(obj1))]]
    updatePosition(date, obj1, position_asset)})


setMethod("updatePosition", signature(date = "Date", obj1 = "Position", obj2 = "Cash"),
  function(date, obj1, obj2, ...){
    prev_date <- obj1@date
    d <- as.character(seq.Date(prev_date, date, "day"))
    cash_return <- prod(1+expandDate(obj2@dailyrate)[d]) - 1
    updated_position_value <- position@position * (1 + cash_return)

    new_position <- new("Position",
      date = date,
      secid = secid(obj1),
      type = obj1@type,
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
