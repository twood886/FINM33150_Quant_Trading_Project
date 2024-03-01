# Helper Functions --------------------------------------------------------
assignColsNamed <- function(data, col, namecol){

  if(namecol %in% colnames(data)){
    dataname <- data[,{{namecol}}]
  }else{
    dataname <- NA
  }

  if(col %in% colnames(data)){
    x <- data[,{{col}}]
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
  xdf <-
    tibble::tibble(
      "date" = as.Date(names(x)),
      "x" = x) %>%
    dplyr::arrange(`date`)

  alldates <- seq.Date(min(xdf$date), max(xdf$date), "days")

  expx <-
    dplyr::full_join(
      xdf,
      tibble::tibble("date" = alldates),
      by = "date") %>%
    dplyr::arrange(`date`) %>%
    tidyr::fill(`x`, .direction = "down")

  setNames(expx$x, expx$date)
}
