

# Add Base Trend ----------------------------------------------------------


#' oh_base
#'
#' days_back used in future Close value and High, Lows
#'
#' @param pdata
#' @param days_back
#'
#' @return
#' @export
#'
#'
oh_base <- function(pdata, close_ahead = 3){
  p_res <- pdata %>%
    mutate(nOpen = lead(Open)) %>%
    mutate(CloseAhead = lead(Close,n=close_ahead),
           Close1 = lead(Close),
           Open1 = lead(Open)) %>%
    ast::add_ATR() %>%
    mutate(OH = High-Open,
           OL = Open-Low) %>%

    mutate(maOH = TTR::runMean(OH,n=15),
           maOH3 = TTR::runMean(OH,n=3),
           maOL = TTR::runMean(OL,n=15),
           maOL3 = TTR::runMean(OL,n=3),
           Diff = (maOH-maOL)) %>%

    # this is the trend
    mutate(maDiff = TTR::runMean(Diff, n = 7),
           maD_atr = maDiff/atr*100)

  p_res$OH_LS <- ""
  p_res$OH_LS <- ifelse(p_res$maD_atr >= 20, "L", p_res$OH_LS)
  p_res$OH_LS <- ifelse(p_res$maD_atr <= -20, "S", p_res$OH_LS)

  p_res <- p_res %>%
    mutate(LS = if_else(maOH > maOL, "L", "S")) %>%
    #mutate(LS = if_else(maDiff > 0, "L", "S")) %>%

    ast::add_ls_num() %>%
    #add_oh_ls_num() %>%
    mutate(mxH = future_value(High, close_ahead, NA, max)) %>%
    mutate(mnL = future_value(Low, close_ahead, NA, min))
    #ast::rnd_all()

  p_res <- as.data.frame(p_res)
  return(p_res)
}



#' add_oh_ls_num
#' Adds trade # (trn) and indv row # (rn)
#' based on OH_LS - added to data set via oh_base fnc above
#'
#' @param f_data
#'
#' @return
#' @export
#'
#'
add_oh_ls_num <- function(f_data){
  rn_num <- 0
  tr_num <- 0
  f_data$rn  <- 0
  f_data$trn <- 0
  for(i in 2:nrow(f_data)){
    f_data$prevOH_LS <- lag(f_data$OH_LS)
    if(!is.na(f_data$prevOH_LS[i])){
      if(f_data$prevOH_LS[i] != f_data$OH_LS[i]){
        #browser()
        rn_num <- 1
        tr_num <- tr_num + 1
      } else {
        rn_num <- rn_num + 1
      }
    }
    f_data$rn[i] <- rn_num
    f_data$trn[i] <- tr_num
  }
  return(f_data)
}


#' add_ls_num
#' Adds trade # (trn) and indv row # (rn)
#' based on OH_LS - added to data set via oh_base fnc above
#'
#' GENERALISE this
#'
#' @param f_data
#'
#' @return
#' @export
#'
#'
add_ls_num <- function(f_data){
  rn_num <- 0
  tr_num <- 0
  f_data$rn  <- 0
  f_data$trn <- 0
  for(i in 2:nrow(f_data)){
    f_data$prevLS <- lag(f_data$LS)
    if(!is.na(f_data$prevLS[i])){
      if(f_data$prevLS[i] != f_data$LS[i]){
        #browser()
        rn_num <- 1
        tr_num <- tr_num + 1
      } else {
        rn_num <- rn_num + 1
      }
    }
    f_data$rn[i] <- rn_num
    f_data$trn[i] <- tr_num
  }
  return(f_data)
}


#' add_ls_num_3
#' Adds trade # (trn) and indv row # (rn)
#' if there are 3 values for LS - L, S, N for not trending ...
#'
#' GENERALISE this
#'
#' @param f_data
#'
#' @return
#' @export
#'
#'
add_ls_num_3 <- function(f_data){
  rn_num <- 0
  tr_num <- 0
  f_data$rn  <- 0
  f_data$trn <- 0
  for(i in 2:nrow(f_data)){
    f_data$prevLS <- lag(f_data$LS)
    if(!is.na(f_data$prevLS[i])){
      if(f_data$LS[i] != "N"){
        if(f_data$prevLS[i] != f_data$LS[i]){
          #browser()
          rn_num <- 1
          tr_num <- tr_num + 1
        } else {
          rn_num <- rn_num + 1
        }
        f_data$rn[i] <- rn_num
        f_data$trn[i] <- tr_num
      }
    }

  }
  return(f_data %>% select(-prevLS))
}



add_prev_ls_num_3 <- function(f_data){
  rn_num <- 0
  tr_num <- 0
  f_data$rn  <- 0
  f_data$trn <- 0
  for(i in 2:nrow(f_data)){
    f_data$prevLS <- lag(f_data$LS)
    f_data$prev2LS <- lag(f_data$LS,n=2)
    if(!is.na(f_data$prevLS[i])){
      if(f_data$prevLS[i] != "N"){
        if(f_data$prevLS[i] != f_data$prev2LS[i]){
          #browser()
          rn_num <- 1
          tr_num <- tr_num + 1
        } else {
          rn_num <- rn_num + 1
        }
        f_data$rn[i] <- rn_num
        f_data$trn[i] <- tr_num
      }
    }

  }
  return(f_data)
}



#' update_ftse_tick_saved
#'
#' Loops through a list of file names, for each one downloads latest data
#' Combines with exisitng data and saves
#'
#' @return
#' @export
#'
update_ftse_tick_saved <- function(){
  ftse_file_names <- list.files(path="/home/allanx38/ast/Data/FTSE_Data")
  for(i in 1 : length(ftse_file_names)){
    print(paste0(i, " of ", length(ftse_file_names)))
    x <- NULL
    tdata <- NULL
    dt <- NULL
    x <- read.csv( paste0("/home/allanx38/ast/Data/FTSE_Data/", ftse_file_names[i]) )
    if(!is.null(x)){
      # read data from Yahoo
      ticker <- stringr::str_sub(ftse_file_names[i],0,-5)
      #dt <- ast::get_yahoo_fin_tick_tc(sym = ticker,nm = ticker,daysback = 50)
      try(dt <- get_yahoo_fin_tick(sym = ticker,nm = ticker, daysback = 50))
      if(!is.null(dt)){
        dt <- dt %>% select(Date,Open,High,Low,Close,Tick)
        dt$Date <- as.character(dt$Date)
        # read data saved on disc
        cs <- read.csv( paste0("/home/allanx38/ast/Data/FTSE_Data/", ftse_file_names[i]) )
        cs <- cs %>% filter(Open != 0)
        # remove 0 values ...
        cs <- cs %>% filter(Open != 0)
        # combine rows and save
        tdata <- bind_rows(cs,dt %>% filter(!Date %in% cs$Date)) %>% distinct() %>% arrange(Date)
        readr::write_csv(tdata, paste0("/home/allanx38/ast/Data/FTSE_Data/", ftse_file_names[i]))
      }
    }
  }
}


#' update_ftse_tick_saved
#'
#' Loops through a list of file names, for each one downloads latest data
#' Combines with exisitng data and saves
#'
#' @return
#' @export
#'
update_dax_tick_saved <- function(d_back = 50){
  ftse_file_names <- list.files(path="/home/allanx38/ast/Data/Dax_Data")
  for(i in 1 : length(ftse_file_names)){
    x <- NULL
    tdata <- NULL
    dt <- NULL
    x <- read.csv( paste0("/home/allanx38/ast/Data/Dax_Data/", ftse_file_names[i]) )
    if(!is.null(x)){
      # read data from Yahoo
      ticker <- stringr::str_sub(ftse_file_names[i],0,-5)
      try(dt <- get_yahoo_fin_tick(sym = ticker,nm = ticker, daysback = d_back))
      if(!is.null(dt)){
        dt <- dt %>% select(Date,Open,High,Low,Close,Tick)
        dt$Date <- as.character(dt$Date)
        # read data saved on disc
        cs <- read.csv( paste0("/home/allanx38/ast/Data/Dax_Data/", ftse_file_names[i]) )
        cs <- cs %>% filter(Open != 0)
        # remove 0 values ...
        cs <- cs %>% filter(Open != 0)
        # combine rows and save
        tdata <- bind_rows(cs,dt %>% filter(!Date %in% cs$Date)) %>% distinct() %>% arrange(Date)
        readr::write_csv(tdata, paste0("/home/allanx38/ast/Data/Dax_Data/", ftse_file_names[i]))
      }
    }
  }
}


# general version of funtion
update_tick_on_disc <- function(file_path, days_back = 50){
  index_file_names <- list.files(path = file_path)
  for(i in 1 : length(index_file_names)){
    x <- NULL
    tdata <- NULL
    dt <- NULL
    x <- read.csv( paste0(file_path, index_file_names[i]) )
    if(!is.null(x)){
      # read data from Yahoo
      ticker <- stringr::str_sub(index_file_names[i],0,-5)
      #dt <- ast::get_yahoo_fin_tick_tc(sym = ticker,nm = ticker,daysback = 50)
      try(dt <- get_yahoo_fin_tick(sym = ticker,nm = ticker, daysback = days_back))
      if(!is.null(dt)){
        dt <- dt %>% select(Date,Open,High,Low,Close,Tick)
        dt$Date <- as.character(dt$Date)
        # read data saved on disc
        cs <- read.csv( paste0(file_path, index_file_names[i]) )
        cs <- cs %>% filter(Open != 0)
        # remove 0 values ...
        cs <- cs %>% filter(Open != 0)
        # combine rows and save
        tdata <- bind_rows(cs,dt %>% filter(!Date %in% cs$Date)) %>% distinct() %>% arrange(Date)
        readr::write_csv(tdata, paste0(file_path, index_file_names[i]))
      }
    }
  }
}



#' todays_lng_oh_trades
#' create base_oh data
#' Filter is : "L", maD_num above certain level, OL/atr above certain level
#'
#' @param lng_tick_vec
#'
#' @return
#' @export
#'
#' @examples
todays_lng_oh_trades <- function(lng_tick_vec, maD_num = 20, maOL3_num = 0.5){

  for(i in 1 : length(lng_tick_vec)){
    x <- NULL
    x <- read.csv( paste0("/home/allanx38/ast/Data/FTSE_Data/", lng_tick_vec[i], ".csv") )
    print(x$Tick[1])
    if(!is.null(x)){
      x <- x %>% filter(Date > '2021-01-01') %>% oh_base()
      last_row <- nrow(x)
      if(x$LS[last_row]=="L" & x$maD_atr[last_row] >= maD_num & (x$maOL3[last_row]/x$atr[last_row]) >= maOL3_num) {
        print("---------------")
        print(x$Tick[1])
        print( x %>% select(1:6,atr,LS,maOH,OL,maOL,maOL3,maD_atr,maDiff) %>% tail(n=15) )
      }
    }
  }
}



#' todays_sht_oh_trades
#'
#' @param sht_tick_vec
#'
#' @return
#' @export
#'
#' @examples
todays_sht_oh_trades <- function(sht_tick_vec,maD_num = -20, maOH3_num = 0.5){

  for(i in 1 : length(sht_tick_vec)){
    x <- NULL
    x <- read.csv( paste0("/home/allanx38/ast/Data/FTSE_Data/", sht_tick_vec[i], ".csv") )
    print(x$Tick[1])
    if(!is.null(x)){
      x <- x %>% filter(Date > '2021-01-01') %>% oh_base()
      last_row <- nrow(x)
      if(x$LS[last_row]=="S" & x$maD_atr[last_row] <= maD_num & (x$maOH3[last_row]/x$atr[last_row]) >= maOH3_num){
        print("---------------")
        print(x$Tick[1])
        print( x %>% select(1:6,atr,LS,maOL,OH,maOH,maOH3,maD_atr,maDiff) %>% tail(n=10) )
      }
    }
  }
}





# Trade Filters -----------------------------------------------------------------

# Trade 2 filter
# OH is L AND index is L
oh_filter_2_lng <- function(pdata){
  pdata %>% filter(LS == "L", FLS == "L", FmaD_atr >= 0)
}

# OH is S AND index is S
oh_filter_2_sht <- function(pdata){
  pdata %>% filter(LS == "S", FLS == "S", FmaD_atr >= 0)
}


# Trade 1 filter
oh_filter_2a_lng <- function(pdata){
  pdata %>% filter(LS == "L", FLS == "L", FmaD_atr >= 0, FmaClose25 > 0)
}



# Trade -------------------------------------------------------------------

#' trade_oh
#'
#' @param tickFUN function that returns list of ticks from folder
#' @param index_csv name of index csv file e.g. "^FTSE.csv"
#' @param filterFUN_Lng filters long trades
#' @param filterFUN_Sht filters short trades
#' @param tick_csv which tick we are trading
#' @param c_ahead days ahead to try and get an atr trade
#'
#' @return
#' @export
#'
trade_oh <- function(tickFUN, index_csv, filterFUN_Lng, filterFUN_Sht, tick_csv, c_ahead = 5){

  # get index data e.g. FTSE, Dow, Dax
  index_data <- tickFUN(index_csv) %>%
    oh_base() %>%
    mutate(FmaClose25 = TTR::runMean(Close,n=25)) %>%
    select(Date, LS, maD_atr,FmaClose25) %>%
    rename(FLS = LS, FmaD_atr = maD_atr)

  # get stock data from an index
  trade_data <- tickFUN(tick_csv) %>% oh_base(close_ahead = c_ahead)

  # join index and stock
  trade_data <- left_join(trade_data, index_data, by = join_by(Date))

  # Long data
    lng_data <- NULL
    # filter long first
    lng_data <- filterFUN_Lng(trade_data)
    if(nrow(lng_data)>0){
      # create lng trades
      lng_data <- oh_trade1_lng_summary(lng_data)
      # generate res set
      lng_res <- trade_oh_res_set(lng_data, tick_csv, "L")
    }

  # Short data
    sht_data <- NULL
    # filter long first
    sht_data <- filterFUN_Sht(trade_data)
    if(nrow(sht_data)>0){
      # create sht trades
      sht_data <- oh_trade1_sht_summary(sht_data)
      # generate res set
      sht_res <- trade_oh_res_set(sht_data, tick_csv, "S")
    }

  # Bind Lng and Sht
    oh_data <- bind_rows(lng_data,lng_data)
    oh_res  <- bind_rows(lng_res, sht_res)

  # generate res list, data and summary
  res_list <- list(oh_data = oh_data, oh_res = oh_res)

  return(res_list)
}


#' trade_oh_res_set
#'
#' @param p_data
#' @param tick_csv
#' @param dir_nm
#'
#' @return
#' @export
#'
trade_oh_res_set <- function(p_data, tick_csv, dir_nm){
  # res set
  num     <- nrow(p_data)
  avg_run <- mean(p_data$n)
  wins    <- sum(p_data$cpl > 0)
  avg_cpl <- mean(p_data$cpl)
  avg_atr_pl <- mean(p_data$matr)
  avg_atr <- mean(p_data$atr)

  res <- data.frame(Dir = dir_nm,
                    Tick = tick_csv,
                    NumTrades =  num,
                    AvgRun = round(avg_run,1),
                    Wins = wins,
                    AvgCpl = round(avg_cpl,1),
                    AvgAtrPl = round(avg_atr_pl,1),
                    AvgAtr = round(avg_atr,1),
                    AvgCplPer = round(avg_cpl / avg_atr,1),
                    AvgAtrPer = round(avg_atr_pl / avg_atr,1))
}



#' oh_trade1_lng_summary
#' oh_base data, then summarizes data grouped by Tick,trn
#' CloseP is end of trade period ...
#'
#' @param pdata - tick data plus oh_base
#'
#' @return
#' @export
#'
#' @examples
oh_trade1_lng_summary <- function(pdata){
  pdata %>%
  group_by(Tick,trn) %>%
    summarise(Dir = "L",
              Tick = first(Tick),
              OpenD = first(Date),
              CloseD = last(Date),
              OpenP = first(Close),
              CloseP = last(Close),
              maxP = max(High),
              cpl = CloseP - OpenP,
              mpl = maxP - OpenP,
              atr = mean(atr, na.rm = T),
              n = n(),
              matr = if_else(mpl>atr,atr,cpl), .groups = "drop") %>%
    group_by() %>%
    mutate(sm_cpl = sum(cpl), sm_mpl = sum(matr))
}



#' oh_trade1_sht_summary
#'
#' @param pdata - tick data plus oh_base
#'
#' @return
#' @export
#'
#' @examples
oh_trade1_sht_summary <- function(pdata){
  pdata %>%
    group_by(Tick,trn) %>%
    summarise(Dir = "S",
              Tick = first(Tick),
              OpenD = first(Date),
              CloseD = last(Date),
              OpenP = first(Close),
              CloseP = last(Close),
              maxP = min(Low),
              cpl = OpenP - CloseP,
              mpl = OpenP - maxP,
              atr = mean(atr, na.rm = T),
              n = n(),
              matr = if_else(mpl>atr,atr,cpl),.groups = "drop") %>%
    group_by() %>%
    mutate(sm_cpl = sum(cpl), sm_mpl = sum(matr))
}




#' loop_thru_ftse_oh
#'
#' @param start_loop
#' @param days_back
#'
#' @return
#' @export
#'
loop_thru_ftse_oh <- function(start_loop = 1, days_back = 3){
  ftse_file_names <- list.files(path="/home/allanx38/ast/Data/FTSE_Data")
  res_all <- NULL
  for(i in start_loop : length(ftse_file_names)){
    x <- NULL
    x <- read.csv( paste0("/home/allanx38/ast/Data/FTSE_Data/", ftse_file_names[i]) )
    if(!is.null(x)){
      # Add base details
      res <- oh_base(x, days_back)
      res_all <- bind_rows(res_all,res)
    }
  }
  return(res_all)
}


#' general version of loop_thru_ftse_oh any index ...
loop_thru_index_oh <- function(file_path, start_loop = 1, days_back = 3){
  ftse_file_names <- list.files(path = file_path)
  res_all <- NULL
  for(i in start_loop : length(ftse_file_names)){
    x <- NULL
    x <- read.csv( paste0(file_path, ftse_file_names[i]) )
    if(!is.null(x)){
      # Add base details
      res <- oh_base(x, days_back)
      res_all <- bind_rows(res_all,res)
    }
  }
  return(res_all)
}



#' oh_long_pb
#' filters the base data - maD_atr and maOL3/atr
#'
#' @param pdata
#' @param maD_filter
#'
#' @return
#' @export
#'
oh_long_pb <- function(pdata,maD_filter = 20){
  pdata %>%
    filter(maD_atr > maD_filter,
           (maOL3/atr) > 0.5) %>%

    mutate(mxpl = mxH-Close,
           mxplatr = mxpl/atr*100) %>%

    mutate(clpl = CloseAhead - Close) %>%

    mutate(fpl = if_else(mxplatr>100,atr,clpl)) %>%

    ast::rnd_all() %>%

    select(1:8,atr,fpl,clpl,mxpl,mxplatr,trn,rn,LS,OL,maOL,maOL3,maD_atr,mxH,mnL)
}



oh_short_pb <- function(pdata,maD_filter = -20){
  pdata %>%
    filter(maD_atr < maD_filter,
           (maOH3/atr) > 0.5) %>%

    mutate(mxpl = Close - mnL,
           mxplatr = mxpl/atr*100) %>%

    mutate(clpl = Close - CloseAhead) %>%

    mutate(fpl = if_else(mxplatr>100,atr,clpl)) %>%

    ast::rnd_all() %>%

    select(1:8,atr,fpl,clpl,mxpl,mxplatr,trn,rn,LS,OH,maOH,maOH3,maD_atr,mxH,mnL)
}



#' future_value
#'
#' gets a value, min, max etc in next few values
#'
#' @param invec
#' @param n
#' @param pad_val
#' @param FUN     a function such as max
#'
#' @return
#' @export
#'
#'
future_value <- function(invec, n = 3, pad_val = NA, FUN = sum) {
  FUN <- match.fun(FUN)
  apply(embed(c(invec[-1], rep(pad_val, n)), n), 1, {
    function(x) if (all(is.na(x))) NA else FUN(x[!is.na(x)])
  })
}



# Quant Mod Charts --------------------------------------------------------

# wrap getSymbols in a function
get_data <- function(sym, days_back){
  to_dt = today()
  from_dt = to_dt - days_back
  Data <- getSymbols(sym, src="yahoo",
                     from = from_dt,to = to_dt, env=NULL)
  return(Data)
}


# wrap chartSeries function and add Bolinger Band
candle_macd_bb <- function(pData,data_name,sset){
  chartSeries(pData,
              type="candlesticks",
              subset=sset,
              theme=chartTheme('white'),
              TA = 'addMACD()',
              name = data_name)
  addBBands()
}

# get data and chart
get_data_candle_macd_bb <- function(sym,days_back,sset){
  Data <- get_data(sym,days_back)
  candle_macd_bb(Data,sym,sset)
}


candle_macd_ma <- function(pData,data_name,sset,sma_num = 150){
  chartSeries(pData,
              type="candlesticks",
              subset=sset,
              theme=chartTheme('white'),
              TA = 'addMACD()',
              name = data_name)
  addSMA(sma_num)
}

# get data and chart
get_data_candle_macd_ma <- function(sym,days_back,sset,sma_num = 150){
  Data <- get_data(sym,days_back)
  candle_macd_ma(Data,sym,sset)
}


# klkl --------------------------------------------------------------------


validate_input <- function(time_series, under_candles_layers) {
  assert_data_frame(time_series)
  if (!test_subset(c('Time', 'Open', 'High', 'Low', 'Close'),
                   colnames(time_series))) {
    stop("Time series must contain 'Time', 'Open', 'High', 'Low' and 'Close' columns.")
  }
  time <- time_series[, c('Time')]
  assert_posixct(time)
  assert_list(under_candles_layers)
}



timeframe <- function(time_series) {
  most_popular_diff <- NULL
  times <- time_series[, c('Time')]
  candles_time_diffs <- c()
  for (index in 2:length(times)) {
    time_1 <- times[index - 1]
    time_2 <- times[index]
    diff <- as.numeric(difftime(time_2, time_1, units = 'secs'))
    candles_time_diffs <- c(diff, candles_time_diffs)
    most_popular_diff <- as.numeric(names(sort(table(candles_time_diffs), decreasing = TRUE))[1])
  }

  return(most_popular_diff)
}



prettyCandlePlot <- function(time_series,
           chart_title = '',
           under_candles_layers = list()) {
    library(ggplot2)
    library(checkmate)

    validate_input(time_series, under_candles_layers)

    candle_duration <- timeframe(time_series)
    chart_data <- time_series
    chart_data$chg = ifelse(chart_data['Close'] > chart_data['Open'], "up", "dn")
    chart_data$flat_bar <- chart_data[, "High"] == chart_data[, "Low"]

    p <- ggplot(chart_data, aes(x = Time))
    p <- p + theme_bw()
    p <-
      p + theme(
        panel.background = element_rect(fill = 'black'),
        panel.border = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "#121212"),
        plot.margin = unit(c(10, 0, 0, 32), "pt"),
        plot.background = element_rect(fill = "black", colour = 'black'),
        plot.title = element_text(colour = "white"),
        axis.line = element_line(colour = "white"),
        axis.text.y = element_text(colour = 'white'),
        axis.text.x = element_text(
          colour = 'white',
          angle = 45,
          hjust = 1
        ),
        axis.ticks.x = element_line(colour = 'white'),
        axis.ticks.y = element_line(colour = 'white'),
        axis.ticks.length = unit(5, "pt")
      )
    p <- p + guides(fill = FALSE, colour = FALSE)
    p <- p + labs(title = chart_title, colour = 'white')
    for (layer in under_candles_layers) {
      p <- p + layer
    }
    p <-
      p + layer(
        geom = 'linerange',
        mapping = aes(ymin = Low, ymax = High),
        params = list(color = 'white'),
        stat = 'identity',
        position = 'identity'
      )
    p <- p + layer(
      geom = 'rect',
      mapping = aes(
        xmin = Time - candle_duration / 2 * 0.9,
        xmax = Time + candle_duration / 2 * 0.9,
        ymin = pmin(Open, Close),
        ymax = pmax(Open, Close),
        fill = chg
      ),
      stat = 'identity',
      position = 'identity'
    )
    p <-
      p + scale_fill_manual(values = c("dn" = "#656565", "up" = "#ededee"))
    p <-
      p + scale_x_datetime(breaks = scales::pretty_breaks(n = 20),
                           date_labels = "")
    p <-
      p + scale_y_continuous(position = 'right',
                             breaks = scales::pretty_breaks(n = 25))

    # Handle special case of drawing a flat bar where OHLC = Open:
    if (any(chart_data$flat_bar))
      p <- p + geom_segment(data = chart_data[chart_data$flat_bar, ],
                            aes(
                              x = Time - 1 / 2 * 0.9,
                              y = Close,
                              yend = Close,
                              xend = Time + 1 / 2 * 0.9
                            ))
    return(p)
  }
