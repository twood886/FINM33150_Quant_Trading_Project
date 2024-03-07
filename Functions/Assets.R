# Asset (S4 Class) --------------------------------------------------------
# Parent class for all assets
setClass("Asset", representation(secid = "character"))


# Equity (S4 Class) -------------------------------------------------------
# Object representing an equity asset
setClass(
  "Equity",
  contains = "Asset",
  representation(
    open = "numeric",
    high = "numeric",
    low = "numeric",
    close = "numeric",
    dividend = "numeric",
    volume = "numeric",
    split = "numeric",
    adj_open = "numeric",
    adj_high = "numeric",
    adj_low = "numeric",
    adj_close = "numeric",
    adj_volume = "numeric"))

setClass(
  "Option",
  representation(
    underlying = "character",
    underlying_close = "numeric",
    exdate = "Date",
    contract_size = "numeric",
    strike_price = "numeric",
    last_date = "Date",
    best_bid = "numeric",
    best_offer = "numeric",
    best_mid = "numeric",
    volume = "numeric",
    open_interest = "numeric",
    impl_volatility = "numeric",
    delta = "numeric",
    gamma = "numeric",
    vega = "numeric",
    theta = "numeric"),
  contains = "Asset")

setClass("Option_Call", contains = "Option")
setClass("Option_Put", contains = "Option")

setClass(
  "Option_Collar",
  representation(
    underlying = "character",
    equity = "Equity",
    call = "Option_Call",
    put = "Option_Put"))



setMethod("secid", signature(obj = "Asset"), function(obj) obj@secid)

setMethod("price", signature(obj = "Equity"),
  function(obj, date){
    price_dates <- as.Date(names(obj@close))
    date_last <- max(price_dates[which(price_dates <= date)])
    obj@close[[as.character(date_last)]]
    })


setMethod("price", signature(obj = "Option"),
  function(obj, date){
    price_dates <- as.Date(names(obj@best_mid))
    date_last <- max(price_dates[which(price_dates <= date)])
    obj@best_mid[[as.character(date_last)]]
    })


setMethod("+", signature(e1 = "Asset", e2 = "Asset"),
  function(e1,e2){
    a <- new("Assets")
    a <- a + e1
    a <- a + e2
    return(a)
})


# Assets (S4 Class) -------------------------------------------------------
# Object representing a list of assets
setClass(
  "Assets",
  representation(secids = "character", assets = "list"),
  prototype(secids = NULL, assets = NULL),
  validity = function(object){
    all(sapply(object@assets, \(x) c("Asset") %in% is(x)))})

setMethod("secid", signature(obj = "Assets"), function(obj) obj@secids)
setMethod("assets", signature(obj = "Assets"), function(obj) obj@assets)
setMethod("+", signature(e1 = "Assets", e2 = "Asset"),
  function(e1, e2){

    if(secid(e2) %in% e1@secids) return(e1)

    new("Assets",
      secids = c(secid(e1), secid(e2)),
      assets = c(e1@assets, e2))
})


# Create Equity Object from Quandl Data -----------------------------------
QuandlEquity <- function(secid, data){
  new("Equity",
    secid = secid,
    open = assignColsNamed(data, "open", "date"),
    high = assignColsNamed(data, "high", "date"),
    low = assignColsNamed(data, "low", "date"),
    close = assignColsNamed(data, "close", "date"),
    dividend = assignColsNamed(data, "dividend", "date"),
    volume = assignColsNamed(data, "volume", "date"),
    split = assignColsNamed(data, "split", "date"),
    adj_open =  assignColsNamed(data, "adj_open", "date"),
    adj_high = assignColsNamed(data, "adj_high", "date"),
    adj_low = assignColsNamed(data, "adj_low", "date"),
    adj_close = assignColsNamed(data, "adj_close", "date"),
    adj_volume = assignColsNamed(data, "adj_volume", "date"))}


# Create Option Object from WRDS Data -------------------------------------
WRDSOption <- function(data){

  data <- dplyr::arrange(data, `date`)

  cp <- data[1,"cp_flag"]

  if(cp == "C"){
    option <- new("Option_Call")
  }else{
    option <- new("Option_Put")
  }
  option@secid = as.character(data[1, "symbol"])
  option@underlying = as.character(data[1,"issuer"])
  option@exdate = data[1, "exdate"][[1]]
  option@contract_size = as.numeric(data[1, "contract_size"])
  option@strike_price = as.numeric(data[1, "strike_price"] / 1000)
  option@best_bid = assignColsNamed(data, "best_bid", "date")
  option@best_offer = assignColsNamed(data, "best_offer", "date")
  option@best_mid <- rowMeans(cbind(option@best_bid, option@best_offer), na.rm = F)
  option@volume = assignColsNamed(data, "volume", "date")
  option@open_interest = assignColsNamed(data, "open_interest", "date")
  option@impl_volatility = assignColsNamed(data, "impl_volatility", "date")
  option@delta = assignColsNamed(data, "delta", "date")
  option@gamma = assignColsNamed(data, "gamma", "date")
  option@vega = assignColsNamed(data, "vega", "date")
  option@theta = assignColsNamed(data, "theta", "date")
  option@underlying_close = assignColsNamed(data, "underlying_price", "date")

  return(option)
}
