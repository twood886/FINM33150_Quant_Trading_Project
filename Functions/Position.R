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

setMethod("value", signature(obj = "Position"), function(obj) obj@value)
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
  function(e1, e2){

    existing <- which(secid(e1) == secid(e2))
    if(length(existing) == 0){
      return(new("Positions", positions = c(e1@positions, e2)))
    }else{
      existingpos <- e1@positions[[existing]]
      updatedpos <- new("Position",
        date = e2@date,
        secid = e2@secid,
        type = e2@type,
        position = existingpos@position + e2@position,
        price = e2@price,
        value = existingpos@value + e2@value)

      if(updatedpos@position == 0){
        return(
          new("Positions",
              positions = c(e1@positions[-existing])))
      }else{
        return(
          new("Positions",
              positions = c(e1@positions[-existing], updatedpos)))
      }
    }
  })

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

