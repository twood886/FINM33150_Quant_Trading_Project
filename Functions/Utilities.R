# Helper Functions --------------------------------------------------------
assignColsNamed <- function(data, col, namecol){

  if(namecol %in% colnames(data)){
    dataname <- data[,{{namecol}}][[1]]
  }else{
    dataname <- NA
  }

  if(col %in% colnames(data)){
    x <- data[,{{col}}][[1]]
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
