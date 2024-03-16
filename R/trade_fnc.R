
## Trade functions



#' trade_loop
#'
#' @param tr_data
#' @param entryFUN
#' @param tradeFUN
#' @param exitFUN
#'
#' @return
#' @export
#'
#' @examples
trade_loop <- function(pdata = NULL, tradeFUN = NULL){
  res_trade_summary <- NULL
  #res_trade_full_list <- NULL
  bTrade <- FALSE
  fin_row_num <- nrow(pdata)
  # loop through trade data
  for(i in 1: fin_row_num){
    tr_data <- pdata[i:fin_row_num,]
    if(!is.na(pdata$LS[i])){
      if(pdata$LS[i]=="L" & pdata$OL[i] > pdata$maOL[i]){
        bTrade <- TRUE
      }
    }
    if(bTrade == TRUE){
      #browser()
      res_list <- trade_ma_GSV(tr_data)
      res_trade_summary <- bind_rows(res_trade_summary, res_list)
      #res_trade_full_df <- res_list$res_trade_full_df
    }
    bTrade <- FALSE
  }
  #res_trade_list <- list(res_trade_summary)
  return(res_trade_summary)
}


entry_L_GSV <- function(pdata){
  if(pdata$LS[i]=="L" & pdata$OL > pdata$maOL){
    return(TRUE)
  } else {
    return(FALSE)
  }
}


trade_ma_GSV <- function(pdata){
  ptarg <- pdata$atr[1]
  sloss <- -ptarg
  pl <- 0
  maxpl <- 0
  minpl <- 0
  oDate <- NULL
  cDate <- NULL
  oPrice <- 0
  cPrice <- 0

  for(i in 1:nrow(pdata)){
    if(is.null(oDate)){oDate <- pdata$Date[i]}
    cDate <- pdata$Date[i]
    if(oPrice == 0){oPrice <- pdata$Open[i] - pdata$maOL[i]}



    pl <- oPrice - pdata$Close[i]
    if(pl > maxpl){maxpl <- pl}
    if(pl < minpl){minpl <- pl}
    if(pl < sloss){pl <- sloss;break}
    if(pl > ptarg){pl <- ptarg;break}
  }
  res_trade_summary <- data.frame(OpenDate  = oDate,
                    CloseDate = cDate,
                    OpenP     = oPrice,
                    PL        = pl,
                    MaxPL     = maxpl,
                    MinPL     = minpl)
  return(res_trade_summary)
}


# Pair Single -------------------------------------------------------------


