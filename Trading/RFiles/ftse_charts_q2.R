
#'
#' ---
#' title: "FTSE 100"
#' author: ""
#' date: ""
#' output:
#'   html_document:
#'     df_print: paged
#'     toc: true
#'     toc_float:
#'       collapsed: true
#'       smooth_scroll: true
#' ---
#'


#'
#+ setup, include = FALSE
  knitr::opts_chunk$set(echo = F,rows.print=20)
  knitr::opts_chunk$set(warning = F, message = F)


  pt <- function(pData, nm = ""){
    chartSeries(pData,
                type="candlesticks",
                theme=chartTheme('white'),
                TA = "addMACD();addSMA(n=20)",
                name = nm)

  }

  ftse <- read.csv("D:/Allan/R_Files/ast/YahooSymbols.csv")
  ftse <- ftse %>% filter(Index=="FTSE") %>% 
    arrange(Tick)
  
# MCRO.L, MRW.L, RBS.L, RSA.L, SLA.L
  

#' </br>
#'
#' # FTSE 100
#'
#+ second, results = 'asis'

for (i in 47:nrow(ftse)){
  cat("<H3>", ftse$Company.Name[i], " - ", ftse$Tick[i], "</H3>")
  to_dt = lubridate::today()
  from_dt = to_dt - 200
  Data <- quantmod::getSymbols(ftse$Tick[i], src="yahoo",
                     from = from_dt,to = to_dt, env=NULL)

  pt(Data, ftse$Company.Name[i])

}

  # Data <- quantmod::getSymbols(ftse$Tick[4], src="yahoo",
  #                              from = from_dt,to = to_dt, env=NULL)



