
# Quantmod Plots

# library(quantmod)

#' qm_ma_3_fnc
#'
#' Candlestick chart with ma 25, 50, 125
#'
#' @param sym
#' @param d_back
#'
#' @return
#' @export
#'
qm_ma_3_fnc <- function(sym, d_back = 2000){
  to_dt = today()
  from_dt = to_dt - d_back

  Data <- try(quantmod::getSymbols(sym, src="yahoo",
                         from = from_dt,to = to_dt,
                         env=NULL))

  pl <- quantmod::chartSeries(Data,
              type="candlesticks",
              theme=chartTheme('white'),
              TA = 'addSMA(n=150,col = "blue");
                    addSMA(n=50,col = "green");
                    addSMA(n=25)')
}


qm_simple_atr <- function(sym, d_back = 2000, subset_yr = '2023'){

  to_dt = today()
  from_dt = to_dt - d_back

  Data <- try(quantmod::getSymbols(sym, src="yahoo",
                                   from = from_dt,to = to_dt,
                                   env=NULL))

  chartSeries(Data,
              type="candlesticks",
              subset = subset_yr,
              theme=chartTheme('white'),
              TA = NULL,
              name = sym)
  addATR()
}



#' get_data
#' queries data using quantmod
#'
#' @param sym
#' @param days_back
#'
#' @return
#' @export
#'
get_data <- function(sym, days_back){
  to_dt = today()
  from_dt = to_dt - days_back
  Data <- getSymbols(sym, src="yahoo",
                     from = from_dt,to = to_dt, env=NULL)
  return(Data)
}



