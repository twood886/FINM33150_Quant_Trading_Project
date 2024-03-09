library(tidyverse)
library(lubridate)
library(scales)
library(forecast)
# Plot Functions ----------------------------------------------------------



# Single Security ---------------------------------------------------------
makeReturnPlots <- function(name, data, datecol, pricecol=NULL, returncol = NULL){

  if(is.null(pricecol) & is.null(returncol)){
    stop("Must specify price or return col")
  }

  if(!is.null(pricecol)){

    return_data_daily <- data %>%
      dplyr::select({{datecol}}, {{pricecol}}) %>%
      dplyr::rename(
        `date` = {{datecol}},
        `price` = {{pricecol}}) %>%
      dplyr::arrange(`date`) %>%
      dplyr::mutate(
        `start` = `date`,
        `return` = `price` / lag(`price`, 1) -1,
        `log_return` = log(`price`) - log(lag(`price`,1)),
        `index` = cumprod(1 + tidyr::replace_na(`return`, 0)))
  }

  return_data_weekly <- return_data_daily %>%
    dplyr::mutate(
      `year` = lubridate::year(`date`),
      `week` = lubridate::week(`date`)) %>%
    dplyr::summarise(
      `date` = max(`date`),
      `start` = min(`start`),
      `return` = prod(1+`return`)-1,
      `log_return` = sum(`log_return`),
      `index` = dplyr::last(`index`),
      .by = c(`year`,`week`))

  return_data_monthly <- return_data_daily %>%
    dplyr::mutate(
      `year` = lubridate::year(`date`),
      `month` = lubridate::month(`date`)) %>%
    dplyr::summarise(
      `date` = max(`date`),
      `start` = min(`start`),
      `return` = prod(1+`return`)-1,
      `log_return` = sum(`log_return`),
      `index` = dplyr::last(`index`),
      .by = c(`year`,`month`))

  returns_data <- list(
    "Daily" = return_data_daily,
    "Weekly" = return_data_weekly,
    "Monthly" = return_data_monthly)

  summary <- purrr::map2(returns_data, names(returns_data), ~return_summary(.y, .x))
  index_plots <- purrr::map2(returns_data, names(returns_data), ~plot_index(name, .y, .x))
  hist_plots <- map2(returns_data, names(returns_data), ~plot_return_hist(name, .y, .x))
  hist_plot_all <- plot_return_hist(name, names(returns_data), returns_data)

  qq_plots <- map2(returns_data, names(returns_data), ~plot_return_qq(name, .y, .x))
  qq_plot_all <- plot_return_qq(name, names(returns_data), returns_data)
  acfpcf_plots <- map2(returns_data, names(returns_data), ~plot_return_acfpcf(name, .y, .x))
  return_plots <- map2(returns_data, names(returns_data), ~plot_return_ts(name, .y, .x))
  return_plot_all <- plot_return_ts(name, names(returns_data), returns_data)

  return(list(
    "data" = returns_data,
    "summary" = summary,
   "index_plots" = index_plots,
   "hist_plots" = hist_plots,
   "hist_plot_all" = hist_plot_all,
   "qq_plots" = qq_plots,
   "qq_plot_all" = qq_plot_all,
   "acfpcf_plots" = acfpcf_plots,
   "return_plots" = return_plots,
   "return_plot_all" = return_plot_all))

  # plot_grid(
  #   hist_plots[["Daily"]]  + labs(title = NULL, subtitle = "Distribution of Returns"),
  #   qq_plots[["Daily"]] + labs(title = NULL, subtitle = "QQPlot"),
  #   acfpcf_plots[["Daily"]] + labs(title = NULL, subtitle = "ACF & PCF"),
  #   return_plots[["Daily"]] + labs(title = NULL, subtitle = "Returns")) +
  #   plot_annotation(title = paste(name, "Return Characteristics"))


}
plot_index <- function(name, frq, data){
  title = paste(name, " - Growth of $1000")
  start.date <- min(data$start, na.rm = T)
  end.date <- max(data$date)

  subtitle <- paste(
    "Data from",
    format(start.date, format = "%b %d, %Y"),
    "to",
    format(end.date, format = "%b %d, %Y"),
    paste0("(", frq, ")"))

  p <- data %>%
    tidyr::drop_na(`index`) %>%
    mutate(`index` = `index` * 1000) %>%
    ggplot() +
    aes(x = date, y = index) +
    geom_line(colour = "#112446") +
    scale_y_continuous(labels = scales::dollar_format()) +
    labs(
      x = "Date",
      y = "Return Index (Indexed to $1000)",
      title =  title,
      subtitle = subtitle) +
    theme_classic()
  return(p)
}
plot_return_hist <- function(name, frq, data){

  if(!is.data.frame(data)){

    data <- data  %>%
      dplyr::bind_rows(.id = "id") %>%
      dplyr::mutate(`id` = factor(`id`, levels = frq, ordered = T))

    title = paste(name, " - Distribution of Returns by Horizon")
    start.date <- min(data$start, na.rm = T)
    end.date <- max(data$date)
    subtitle <- paste(
      "Data from",
      format(start.date, format = "%b %d, %Y"),
      "to",
      format(end.date, format = "%b %d, %Y"))

    p <- data  %>%
      tidyr::drop_na(`return`) %>%
      ggplot() +
      aes(x = return, fill = `id`) +
      geom_histogram(bins = 30L) +
      scale_fill_viridis_d(option = "viridis", direction = 1) +
      scale_x_continuous(labels = scales::percent_format()) +
      labs(
        x = "Return Bin",
        y = "Frequency",
        fill = "Return Horizon",
        title =  title,
        subtitle = subtitle) +
      theme_classic() +
      facet_wrap(vars(`id`), scales = "free")

  }else{
    title = paste(name, " - Distribution of", frq, "Returns")
    start.date <- min(data$start, na.rm = T)
    end.date <- max(data$date)
    subtitle <- paste(
      "Data from",
      format(start.date, format = "%b %d, %Y"),
      "to",
      format(end.date, format = "%b %d, %Y"),
      paste0("(", frq, ")"))

    p <- data %>%
      tidyr::drop_na(`return`) %>%
      ggplot() +
      aes(x = `return`) +
      geom_histogram(bins = 30L, fill = "#112446") +
      scale_x_continuous(labels = scales::percent_format()) +
      labs(
        x = "Return Bin",
        y = "Frequency",
        title =  title,
        subtitle = subtitle) +
      theme_classic()
  }
  return(p)
}
plot_return_qq <- function(name, frq, data){

  if(!is.data.frame(data)){

    data <- data  %>%
      dplyr::bind_rows(.id = "id") %>%
      dplyr::mutate(`id` = factor(`id`, levels = frq, ordered = T))

    title = paste(name, " - QQ Plot of Returns by Horizon")
    start.date <- min(data$start, na.rm = T)
    end.date <- max(data$date)
    subtitle <- paste(
      "Data from",
      format(start.date, format = "%b %d, %Y"),
      "to",
      format(end.date, format = "%b %d, %Y"))

    p <- data  %>%
      tidyr::drop_na(`return`) %>%
      ggplot() +
      aes(sample = return, colour = `id`) +
      stat_qq() +
      stat_qq_line() +
      scale_fill_viridis_d(option = "viridis", direction = 1) +
      #scale_x_continuous(labels = scales::percent_format()) +
      labs(
        x = "Theoretical Quantiles",
        y = "Data Sample Quantiles",
        colour = "Return Horizon",
        title =  title,
        subtitle = subtitle) +
      theme_classic() +
      facet_wrap(vars(`id`), scales = "free")

  }else{
    title = paste(name, " - QQ Plot of", frq, "Returns")
    start.date <- min(data$start, na.rm = T)
    end.date <- max(data$date)
    subtitle <- paste(
      "Data from",
      format(start.date, format = "%b %d, %Y"),
      "to",
      format(end.date, format = "%b %d, %Y"),
      paste0("(", frq, ")"))

    p <- data %>%
      tidyr::drop_na(`return`) %>%
      ggplot() +
      aes(sample = `return`) +
      stat_qq() +
      stat_qq_line() +
      labs(
        x = "Theoreticl Quantiles",
        y = "Data Sample Quantiles",
        title =  title,
        subtitle = subtitle) +
      theme_classic()


  }
  return(p)
}
plot_return_acfpcf <- function(name, frq, data){

  lag.max <- case_when(
    frq == "Daily" ~ 63,
    frq == "Weekly" ~ 52,
    frq == "Monthly" ~ 12)

  title = paste(name, " - ACF & PCF of", frq, "Returns")
  start.date <- min(data$start, na.rm = T)
  end.date <- max(data$date)
  subtitle <- paste(
    lag.max, "Lags",
    "Data from",
    format(start.date, format = "%b %d, %Y"),
    "to",
    format(end.date, format = "%b %d, %Y"),
    paste0("(", frq, ")"))

  x <- as.data.frame(drop_na(data,return)$return)
  x.acf <- acf(x, plot=F, lag.max=lag.max, type="correlation")
  x.pcf <- acf(x, plot=F, lag.max=lag.max, type="partial")
  ci.line <- qnorm((1 - 0.95) / 2) / sqrt(x.acf$n.used)

  d.acf <- data.frame(lag=x.acf$lag, acf=x.acf$acf)
  d.pcf <- data.frame(lag=x.pcf$lag, acf=x.pcf$acf)
  d <- bind_rows(
    "acf" = d.acf,
    "pcf" = d.pcf,
    .id = "id")

  p <- d %>%
    ggplot(aes(x=lag, y=acf)) +
    geom_hline(yintercept=0) +
    facet_wrap(vars(`id`), scales = "free") +
    geom_segment(aes(xend=lag,yend=0), color = "#112446") +
    theme_classic() +
    geom_hline(yintercept=ci.line, color="red", linetype="dashed") +
    geom_hline(yintercept=-ci.line, color="red", linetype="dashed") +
    labs(
      x = "Auto and Partial Auto Correlation",
      y = "Lags",
      title =  title,
      subtitle = subtitle)

  return(p)
}
plot_return_ts <- function(name, frq, data){
  if(!is.data.frame(data)){

    data <- data  %>%
      dplyr::bind_rows(.id = "id") %>%
      dplyr::mutate(`id` = factor(`id`, levels = frq, ordered = T))

    title = paste(name, "- Returns by Horizon")
    start.date <- min(data$start, na.rm = T)
    end.date <- max(data$date)
    subtitle <- paste(
      "Data from",
      format(start.date, format = "%b %d, %Y"),
      "to",
      format(end.date, format = "%b %d, %Y"))

    p <- data  %>%
      tidyr::drop_na(`return`) %>%
      dplyr::mutate(`abs_return` = abs(`return`)) %>%
      ggplot() +
      aes(x = date, y = return) +
      geom_line(colour = "#112446") +
      theme_classic() +
      facet_wrap(vars(id), scales = "free") +
      scale_y_continuous(labels = scales::percent_format()) +
      labs(
        x = "Date",
        y = "Return",
        title =  title,
        subtitle = subtitle)


  }else{
    title = paste(name, "-", frq, "Returns")
    start.date <- min(data$start, na.rm = T)
    end.date <- max(data$date)
    subtitle <- paste(
      "Data from",
      format(start.date, format = "%b %d, %Y"),
      "to",
      format(end.date, format = "%b %d, %Y"),
      paste0("(", frq, ")"))

    p <- data %>%
      tidyr::drop_na(`return`) %>%
      dplyr::mutate(`abs_return` = abs(`return`)) %>%
      ggplot() +
      aes(x = date, y = return) +
      geom_line(colour = "#112446") +
      theme_classic() +
      scale_y_continuous(labels = scales::percent_format()) +
      labs(
        x = "Date",
        y = "Return",
        title =  title,
        subtitle = subtitle)
  }
  return(p)
}
return_summary <- function(frq, data){
  mult <- case_when(
    frq == "Daily" ~ 252,
    frq == "Weekly" ~ 52,
    frq == "Monthly" ~ 12)
  data <- data %>%
    filter(!is.na(`return`))

  total_return <- prod(1+data$return) -1
  annmean <- mean(data$return) * mult
  annvol <- sd(data$return) * sqrt(mult)
  downside_vol <- sd(ifelse(data$return <0 , data$return, 0)) * sqrt(mult)
  skew <- DescTools::Skew(data$return)
  kurtosis <- DescTools::Kurt(data$return)

  Sharpe <- annmean / annvol
  Sortino <- annmean / downside_vol

  previous_peaks <- cummax(data$index)
  drawdowns <- (data$index - previous_peaks) / previous_peaks
  max_drawdown <- min(drawdowns)

  sum <-c(
    paste("Total Return:", paste0(formatC(total_return * 100, digits = 3),"%"), sep = "\t\t"),
    paste("Ann. Mean Returns:", paste0(formatC(annmean * 100, digits = 3),"%"), sep = "\t"),
    paste("Ann. Return StdDev:", paste0(formatC(annvol * 100, digits = 3),"%"), sep = "\t"),
    paste("Sharpe Ratio (no rf):", formatC(Sharpe, digits = 3), sep = "\t"),
    paste("Sortino Ratio (no rf):", formatC(Sortino, digits = 3), sep = "\t"),
    paste("Skewness:", formatC(skew, digits = 2), sep = "\t\t"),
    paste("Excess Kurtosis", formatC(kurtosis, digits = 3), sep = "\t\t"),
    paste("Max Drawdown:", paste0(formatC(max_drawdown * 100, digits = 3),"%"), sep = "\t\t"))

  return(sum)
}





plot_2_qq <- function(names, data, frq){
  name1 <- names[[1]]
  name2 <- names[[2]]
  data1 <- data[[1]] %>% drop_na(`return`)
  data2 <- data[[2]] %>% drop_na(`return`)

  gg_qq_empirical <- function(a, b, quantiles = seq(0, 1, 0.01))
  {
    a_lab <- deparse(substitute(a))
    if(missing(b)) {
      b <- rnorm(length(a), mean(a), sd(a))
      b_lab <- "normal distribution"
    }
    else b_lab <- deparse(substitute(b))

    ggplot(mapping = aes(x = quantile(a, quantiles),
                         y = quantile(b, quantiles))) +
      geom_point() +
      geom_abline(aes(slope = 1, intercept = 0), linetype = 2) +
      labs(x = paste(deparse(substitute(a)), "quantiles"),
           y = paste(deparse(substitute(b)), "quantiles"),
           title = paste("Empirical qq plot of", a_lab, "against", b_lab))
  }

  title = paste("QQ Plot of", name1, "vs", name2)
  subtitle = paste(frq, "Data")

  p <- gg_qq_empirical(data2$return, data1$return) +
    theme_classic() +
    labs(
      x = paste(name2, "Quantiles"),
      y = paste(name1, "Quantiles"),
      title =  title,
      subtitle = subtitle)
  return(p)
}

plot_roll_corr <- function(names, data, frq, n){
  name1 <- names[[1]]
  name2 <- names[[2]]
  data1 <- data[[1]] %>% drop_na(`return`) %>% dplyr::select(`date`, `return`)
  data2 <- data[[2]] %>% drop_na(`return`) %>% dplyr::select(`date`, `return`)

  data3 <- inner_join(data1, data2, by = "date") %>%
    tidyr::nest(data = c(everything(),-`date`), .by = `date`) %>%
    dplyr::mutate(`rolldata` = purrr::accumulate(`data`, roll_bind, n)) %>%
    dplyr::slice(-(1:13)) %>%
    dplyr::mutate(`cor` = map(`rolldata`, \(x) cor(x$return.x, x$return.y))) %>%
    dplyr::select(`date`, `cor`) %>%
    tidyr::unnest(everything())

  title <- paste(name1,"-",name2,"Rolling",n,frq,"Correlation")
  p <- data3 %>%
    ggplot() +
    aes(x = date, y = cor) +
    geom_line(colour = "#112446") +
    theme_classic()+
    labs(
      x = "Date",
      y = "Correlation",
      title = title)
  return(p)
}
