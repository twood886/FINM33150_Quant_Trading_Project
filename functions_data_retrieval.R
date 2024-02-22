getCWBDCData <- function(
    url = "https://www.bdcs.com/docs/bdcs/ChartData.json?_=1708270265123"){

  library(jsonlite)
  library(dplyr)
  data <- url %>%
    jsonlite::fromJSON() %>%
    as.data.frame() %>%
    dplyr::rename(`date` = `x`) %>%
    dplyr::mutate(`date` = as.Date(`date`))

  return(data)
}
