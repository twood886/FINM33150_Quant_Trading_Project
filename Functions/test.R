library(tidyverse)
library(readxl)
library(readr)
library(Quandl)
Quandl.api_key("EsFRJz6Xd3CxmytxKgCB")
source("functions_data_retrieval.R")


dates <- seq.Date(as.Date("2023-01-03"), as.Date("2023-01-31"),"days")

BIZD <- QuandlEquity("BIZD", Quandl.datatable('QUOTEMEDIA/PRICES', ticker = "BIZD"))


BIZD_trade_enter <- newTradeTarget(BIZD, as.Date("2023-01-03"), "Buy", 1)
BIZD_trade_exit <- newTradeTarget(BIZD, as.Date("2023-01-31"), "Sell", 1)

HYG_option <-
  options_data %>%
  filter(`ticker` == "HYG") %>%
  filter(`strike_price` == 75000) %>%
  filter(`exdate` == as.Date("2023-01-06")) %>%
  filter(`cp_flag` == "C") %>%
  WRDSOption()

HYG_option@underlying_close <- 74.62


HYG_option_trade <- newTradeTarget(HYG_option, as.Date("2023-01-04"), "Buy", "pct", 0.7)

trades <- new("Trade_Targets") +
  BIZD_trade_enter +
  BIZD_trade_exit +
  HYG_option_trade

# Test updating new
a <- assets(trades)
obj2 <- assets(a)[[1]]
equity_position <- new("Position",
  date = as.Date("2023-01-03"),
  secid = "BIZD",
  type = "Equity",
  position = 100,
  price = 14.29,
  value = 1429)
updatePosition(obj1, obj2, as.Date("2023-01-04"))

obj2 <- assets(a)[[2]]
option_position <- new("Position",
  date = as.Date("2023-01-03"),
  secid = secid(obj2),
  type = "Option",
  position = 100,
  price = price(obj2, as.Date("2023-01-03")),
  value = 0.05 * 100)

obj1 <- equity_position + option_position
obj2 <- assets(trades)

test <- updatePosition(obj1, obj2, as.Date("2023-01-04"))
