library(tidyverse)
library(readxl)
library(readr)
library(Quandl)
Quandl.api_key("EsFRJz6Xd3CxmytxKgCB")
source("functions_data_retrieval.R")


dates <- seq.Date(as.Date("2023-01-03"), as.Date("2023-01-31"),"days")

BIZD <- QuandlEquity("BIZD", Quandl.datatable('QUOTEMEDIA/PRICES', ticker = "BIZD"))

BIZD_trade <- newTradeTarget(BIZD, as.Date("2023-01-03"), "Buy", 1)
trades <- c(BIZD_trade)
