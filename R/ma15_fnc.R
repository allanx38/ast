
# functions for ma 15 based system ...



#' add_TA_ma15
#' adds TA stuff ... ma15
#'
#' @param x data set (data frame)
#'
#' @return
#' @export
#'
#' @examples
add_TA_ma15 <- function(x){
  # common stuff
  x$ma15 <- round(runMean(x$Close, n=15),2)
  x$maDiff <- x$Close - x$ma15
  x$p_Close <- lag(x$Close)
  x$p_maDiff <- lag(x$maDiff)
  x <- x %>% mutate(rn = row_number())
  x$wday <- lubridate::wday(x$Date,week_start=1)
  x$mth  <- lubridate::month(x$Date)
  x <- x %>% mutate(across(where(is.numeric), round))

  # Add MACD
  macd <- TTR::MACD(x$Close)
  x <- cbind(x,macd)
  x$macd <- round(x$macd,2)
  x$signal <- round(x$signal,2)

  # Add ATR
  atr <- ATR(x[,c("High","Low","Close")], n=14)
  x <- cbind(x,atr)
  x <- x %>% select(-trueHigh,-trueLow)
  x$atr <- round(x$atr,1)
  # incl final atr value
  x$atr_f <- x[nrow(x),"atr"]


  return(x)
}





split_ls_ma15_grp <- function(pdata, tr_type_nm){

  # win loss
  w_l <- pdata %>% group_by(Tick,wday,atr_f,win) %>% tally() %>%
    pivot_wider(names_from = win, values_from = n) %>% mutate(WinPer = round(w/(w+l)*100))

  res_t <- pdata %>% group_by(Tick,wday,atr_f) %>%
    summarise(wpl = sum(pl), wct = n(),Avgpl = round(wpl/wct),1 ) %>%
    as_tibble() %>% mutate(Dataset = tr_type_nm) %>% mutate(Avg_Per = round( (Avgpl / atr_f) * 100,1))

  res_t2 <- inner_join(res_t,
                       w_l,
                       by = c("Tick", "wday", "atr_f"))

  return(res_t2)

}


#' split_ls_ma15
#' takes a TA augmented ma15 data set
#' generates result sets - long and short
#'
#' @param x - data set
#'
#' @return
#' @export
#'
#' @examples
split_ls_ma15 <- function(x){

  # results list
  rlist <- list()

  # Long
  lng <-  x %>% filter(ma15 < Close)
  lng$pl <- lng$Close - lng$p_Close
  lng$p_lpl <- lag(lng$pl)
  lng$win <- ifelse(lng$pl>0,"w","l")

  lng_all      <- split_ls_ma15_grp(lng %>% filter(p_maDiff>0),             "lng_all")
  lng_prev_lng <- split_ls_ma15_grp(lng %>% filter(p_maDiff>0,p_lpl>0),     "lng_prev_lng")
  lng_prev_sht <- split_ls_ma15_grp(lng %>% filter(p_maDiff>0,p_lpl<0),     "lng_prev_sht")
  lng_macd_lng <- split_ls_ma15_grp(lng %>% filter(p_maDiff>0,signal>macd), "lng_macd_lng")

  # Short
  sht <-  x %>% filter(ma15 > Close)
  sht$pl <- sht$p_Close - sht$Close
  sht$p_spl <- lag(sht$pl)
  sht$win <- ifelse(sht$pl>0,"w","l")

  sht_all      <- split_ls_ma15_grp(sht %>% filter(p_maDiff<0),                  "sht_all")
  sht_prev_sht <- split_ls_ma15_grp(sht %>% filter(p_maDiff<0,p_spl<0),     "sht_prev_sht")
  sht_prev_lng <- split_ls_ma15_grp(sht %>% filter(p_maDiff<0,p_spl>0),     "sht_prev_lng")

  rlist <- list(lng_all = lng_all,
                lng_prev_lng = lng_prev_lng,
                lng_prev_sht = lng_prev_sht,
                lng_macd_lng = lng_macd_lng,
                sht_all = sht_all,
                sht_prev_sht = sht_prev_sht,
                sht_prev_lng = sht_prev_lng)

  return(rlist)
}



#' add_ls_num
#' For data sets with column of L and S's (Long/Short)
#' Adds a running number
#'
#' @param f_data
#'
#' @return
#' @export
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


#' rs
#'
#' sums and rounds a data value
#'
#' @param pdata
#'
#' @return
#' @export
#'
#' @examples
rs <- function(pdata){
  round(sum(pdata),0)
}

#' rdf_ma
#'
#' @param x - the ma15 data set
#' @param nm_p - name of dataframe in the results list
#'
#' @return
#' @export
#'
rdf_ma <- function(x, nm_p){
  data.frame(TotPL = rs(x[[nm_p]]$wpl), ct = rs(x[[nm_p]]$wct))
}
