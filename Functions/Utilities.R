# Helper Functions --------------------------------------------------------
assignColsNamed <- function(data, col, namecol){

  if(namecol %in% colnames(data)){
    if(length(data[,{{namecol}}][[1]]) ==1){
      dataname <- data[,{{namecol}}]
    }else{
      dataname <- data[,{{namecol}}][[1]]
    }
  }else{
    dataname <- NA
  }

  if(col %in% colnames(data)){
    if(length(data[,{{col}}][[1]]) == 1){
      x <- data[,{{col}}]
    }else{
      x <- data[,{{col}}][[1]]
    }
  }else{
    x <- rep(NA, length(dataname))
  }
  names(x) <- dataname
  return(x)
}


named_group_split <- function (...) {
  data <- group_by(...)
  names <- group_keys(data) %>% map(as.character) %>% reduce(paste,
                                                             sep = "~~")
  group_split(data) %>% set_names(names)
}


expandDate <- function(x){
  # Function to expand data for all dates with max and min dates of data
  # and fill down missing values
  # Useful for price data to fill in weekends
  #
  # Args:
  #   x: named vector with dates as names or dataframe with "date" column
  #
  # Returns:
  #   named vector with dates as names or dataframe with "date" column
  #   fills missing dates with previous values
  y <- x

  if(typeof(x) == "double"){
    x <-
    tibble::tibble(
      "date" = as.Date(names(x)),
      "x" = x) %>%
    dplyr::arrange(`date`)
  }

  alldates <- seq.Date(min(x$date), max(x$date), "days")

  expx <-
    dplyr::full_join(
      x,
      tibble::tibble("date" = alldates),
      by = "date") %>%
    dplyr::arrange(`date`) %>%
    tidyr::fill(c(everything(),-`date`), .direction = "down")

  if(typeof(y) == "double"){
    out <- setNames(expx$x, expx$date)
  }else{
    out <- expx
  }

  return(out)
}

roll_bind <- function(acc, nxt, n){
  # Function to bind rows from list of dataframes with max n rows
  # If number of rows is equal to n before bind, the first row is removed
  # Used with purrr::accumulate function
  #
  # Args:
  #   acc: existing dataframe
  #   nxt: dataframe to bound to acc
  #   n: max number of rows
  #
  # Returns:
  #   dataframe containing n number of rows
  if(nrow(acc) + nrow(nxt) <= n){
    bind_rows(acc, nxt)
  }else{
    bind_rows(acc[-1,], nxt)
  }
}

