
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
  
  ftse <- ftse %>% 
    filter(Tick %in% c("AHT.L", "BP.L", "BRBY.L", "CRDA.L", "CRH.L", "DGE.L",
                       "III.L", "IMB.L","LLOy.L","REL.L","RIO.L","RR.L", "SBRY.L",
                       "SHEL.L", "SKG.L","SPX.L","STJ.L"))
  
 # ANTO.L, BARC.L, FERG.L, GLEN.L, OCDO.L, PSON.L, SDR.L, SMDS.L
 # BNZL.L - seems to move in blocks ...  
  
 # ideas - pull back to MA20?  
 # idea - H/L but on above/beow MA  
  
#' </br>
#'
#' # FTSE 100
#'
#+ second, results = 'asis'

for (i in 1:nrow(ftse)){
  cat("<H3>", ftse$Company.Name[i], " - ", ftse$Tick[i], "</H3>")
  to_dt = lubridate::today()
  from_dt = to_dt - 200
  Data <- quantmod::getSymbols(ftse$Tick[i], src="yahoo",
                     from = from_dt,to = to_dt, env=NULL)

  pt(Data, ftse$Company.Name[i])

}

  # Data <- quantmod::getSymbols(ftse$Tick[4], src="yahoo",
  #                              from = from_dt,to = to_dt, env=NULL)



