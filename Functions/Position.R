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
setMethod("+", signature(e1 = "Position", e2 = "Position"),
  function(e1, e2) new("Positions", positions = c(e1, e2)))

# Positions (S4 Class) ----------------------------------------------------
setClass(
  "Positions",
  representation(positions = "list"),
  prototype(positions = NULL),
  validity = function(object){
    all(sapply(object@positions, \(x) c("Position") %in% is(x)))})

setMethod("+", signature(e1 = "Positions", e2 = "Position"),
  function(e1, e2) new("Positions", positions <- c(e1@positions, e2)))

setMethod("secid", signature(obj = "Positions"), function(obj) sapply(obj@positions, secid))











# Update Position Method --------------------------------------------------
# The following methods are used to update the position values
setMethod("updatePosition", signature(obj1 = "Positions", obj2 = "Assets"),
  function(obj1, obj2, date){
    new("Positions",
      positions = lapply(obj1@positions, updatePosition, obj2, date))
  })

setMethod("updatePosition", signature(obj1 = "Position", obj2 = "Assets"),
  function(obj1, obj2, date, ...){
    position_asset <- assets(obj2)[[which(secid(obj2) == secid(obj1))]]
    updatePosition(obj1, position_asset, date)})

setMethod("updatePosition", signature(obj1 = "Position", obj2 = "Cash"),
  function(obj1, obj2, date, ...){
    prev_date <- obj1@date
    d <- as.character(seq.Date(prev_date, date, "day"))
    cash_return <- prod(1+expandDate(obj2@dailyrate)[d]) - 1
    updated_position_value <- obj1@position * (1 + cash_return)

    new_position <- new("Position",
      date = date,
      secid = secid(obj1),
      type = obj1@type,
      position = updated_position_value,
      price = 1,
      value = updated_position_value)

    return(new_position)
  })

setMethod("updatePosition", signature(obj1 = "Position", obj2 = "Equity"),
  function(obj1, obj2, date, invest_dividend = T, ...){

    price_close <- price(obj2, date)

    # Need to add split logic
    # Invest dividends
    dividend <- replace_na(obj2@dividend[[as.character(date)]], 0)
    position_chg <- (obj1@position * dividend) / price_close

    new("Position",
      date = date,
      secid = secid(obj2),
      type = "Equity",
      position = obj1@position + position_chg,
      value = (obj1@position + position_chg) * price_close,
      price = price_close)
  })

setMethod("updatePosition", signature(obj1 = "Position", obj2 = "Option"),
  function(obj1, obj2, date, ...){

    price_mid <- price(obj2, date)
    new("Position",
      date = date,
      secid = secid(obj2),
      type = obj1@type,
      position = obj1@position,
      price = price_mid,
      value = obj1@position * price_mid)
  })
