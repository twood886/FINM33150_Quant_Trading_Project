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

trades <- new("Trade_Targets") +
  BIZD_trade_enter +
  BIZD_trade_exit
