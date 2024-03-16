

#' get_folder_ticks
#'
#' @param get_folder_FUN
#'
#' @return
#' @export
#'
#' @examples
get_folder_ticks <- function(get_folder_FUN){
  ftse_ticks <- list.files(path = get_folder_FUN())
  ftse_ticks <- as.data.frame(ftse_ticks)
  ftse_ticks <- ftse_ticks %>% rename(csv = ftse_ticks)
  ftse_ticks$tick <- stringr::str_sub(ftse_ticks$csv, 0,-5)
  return(ftse_ticks)
}

# FTSE 100 ----------------------------------------------------------------


#' get_ftse_ticks
#'
#' @return
#' @export
#'
#'
get_ftse_ticks <- function(){
  ftse_ticks <- get_folder_ticks(get_ftse_folder)
  return(ftse_ticks)
}



#' get_ftse_csv_file
#'
#' @param csv_nm
#'
#' @return
#' @export
#'
#'
get_ftse_csv_file <- function(csv_nm){
  read.csv( paste0("/home/allanx38/ast/Data/FTSE_Data/", csv_nm  ) )
}

get_ftse_ticks_csv_list <- function(){

  c("AAL.L",  "ABF.L",  "ADM.L",  "AHT.L",  "ANTO.L", "AUTO.L", "AV.L",   "AZN.L",  "BA.L",   "BARC.L",
    "BATS.L", "BDEV.L", "BHP.L", "BKG.L",  "BLND.L", "BNZL.L", "BP.L",   "BRBY.L", "BT-A.L", "CCH.L",
    "CCL.L",  "CNA.L",  "CPG.L",  "CRDA.L", "CRH.L",  "DCC.L", "DGE.L",  "DLG.L",  "EXPN.L",
    "EZJ.L",  "FERG.L", "FLTR.L", "FRES.L", "GLEN.L", "GSK.L",  "HIK.L",  "HL.L",   "HLMA.L",
    "HSBA.L", "HSX.L",  "IAG.L",  "IHG.L",  "III.L",  "IMB.L",  "INF.L",  "ITRK.L", "ITV.L",  "JMAT.L",
    "KGF.L",  "LAND.L", "LGEN.L", "LLOY.L", "LSEG.L", "MKS.L",  "MNDI.L", "MRO.L",  "NG.L",   "NXT.L",
    "OCDO.L", "PHNX.L", "PRU.L",  "PSN.L",  "PSON.L", "REL.L", "RIO.L",  "RKT.L",  "RMV.L",  "RR.L",
    "RTO.L",  "SBRY.L", "SDR.L",  "SGE.L",  "SGRO.L", "SKG.L",  "SMDS.L", "SMIN.L", "SMT.L",
    "SN.L",   "SPX.L",  "SSE.L",  "STAN.L", "STJ.L",  "SVT.L",  "TSCO.L", "TUI.L",  "TW.L",
    "ULVR.L", "UU.L",   "VOD.L",  "WPP.L", "WTB.L")

}


get_ig_ticks_csv_list <- function(){

  c("")
}



get_ftse_folder <- function(csv_nm){
  return("/home/allanx38/ast/Data/FTSE_Data/")
}


# Dow ----------------------------------------------------------------

#' get_dow_ticks
#'
#' @return
#' @export
#'
get_dow_ticks <- function(){
  dow_ticks <- get_folder_ticks(get_dow_folder)
  return(dow_ticks)
}

get_dow_ticks_csv_list <- function(){
  c("AAPL", "BA",   "CAT",  "CSCO", "CVX",  "DOW",  "GS",   "HD",   "HON",
    "IBM",  "JNJ",  "JPM",  "KO",   "MCD",  "MMM",  "MRK",  "MSFT",
    "NKE",  "PG",   "TRV",  "UNH",  "V",    "VZ",   "WBA",  "WMT",  "XOM" )
}


#' get_dow_csv_file
#'
#' @param csv_nm
#'
#' @return
#' @export
#'
get_dow_csv_file <- function(csv_nm){
  read.csv( paste0("/home/allanx38/ast/Data/Dow_Data/", csv_nm  ) )
}


get_dow_folder <- function(csv_nm){
  return("/home/allanx38/ast/Data/Dow_Data/")
}



# Dax ----------------------------------------------------------------

get_dax_ticks <- function(){
  dow_ticks <- get_folder_ticks(get_dax_folder)
  return(dow_ticks)
}

get_dax_ticks_csv_list <- function(){

c("1COV.DE", "ADS.DE",  "ALV.DE",  "BAS.DE",  "BAYN.DE", "BEI.DE",  "BMW.DE",  "CON.DE",  "DB1.DE",  "DBK.DE",
  "DHER.DE", "DTE.DE",  "DWNI.DE", "EOAN.DE", "FME.DE",  "FRE.DE",  "HEI.DE",  "HEN3.DE", "IFX.DE",  "LIN.DE",
  "MRK.DE",  "MTX.DE",  "MUV2.DE", "RWE.DE",  "SAP.DE",  "SIE.DE",  "VNA.DE",  "VOW3.DE")
}


get_dax_csv_file <- function(csv_nm){
  read.csv( paste0("/home/allanx38/ast/Data/Dax_Data/", csv_nm  ) )
}

get_dax_folder <- function(csv_nm){
  return("/home/allanx38/ast/Data/Dax_Data/")
}


# SP500 ----------------------------------------------------------------

get_sp500_ticks <- function(){
  dow_ticks <- get_folder_ticks(get_sp500_folder)
  return(dow_ticks)
}


get_sp500_csv_file <- function(csv_nm){
  read.csv( paste0("/home/allanx38/ast/Data/SP500_Data/", csv_nm  ) )
}


get_sp500_folder <- function(csv_nm){
  return("/home/allanx38/ast/Data/SP500_Data/")
}



#' qm_gedata_df
#'
#' Gets data using Quantmod, returns
#' Defaults to Yahoo
#' Defaults to today going back 200 days a DATA FRAME
#'
#' @param sym_p = yahoo symbol, no default
#' @param src_p = source defaults to Yahoo
#' @param to_dt_p = when data ends, default is today
#' @param days_back_p  = days back (subtracted from to date) default is 200 days ago
#'
#' @return
#' @export
#'
qm_gedata_df <- function(sym_p,
                         src_p = "yahoo",
                         to_dt_p = lubridate::today(),
                         days_back_p = 200){

  # set dates
  from_date = to_dt_p - days_back_p

  x <- getSymbols(sym_p, src=src_p,  from = from_date, to = to_dt_p, env=NULL)
  x <- x[,1:4]
  colnames(x) <- c('Open','High','Low','Close')
  x <- na.omit(x)
  x <- data.frame(Date=index(x), coredata(x)[,], stringsAsFactors = F)
  x$Tick <- sym_p
  return(x)
}


#' get_yahoo_fin_tick
#'
#' Gets data using quantmod, returns data frame
#' Renames column names
#'
#' @param sym
#' @param nm
#' @param daysback
#'
#' @return
#' @export
#'
#'
get_yahoo_fin_tick <- function(sym,nm="",daysback){
  to_dt = today()
  fr_dt = to_dt - daysback
  x <- quantmod::getSymbols(sym, from = fr_dt, to = to_dt, src="yahoo", env=NULL)
  x <- x[,1:4]
  colnames(x) <- c('Open','High','Low','Close')
  x <- na.omit(x)
  x <- data.frame(Date=index(x), coredata(x)[,], stringsAsFactors = F)
  x$Tick <- nm
  x <- x %>% select(Tick,everything())
}



#' get_yahoo_fin_tick_tc
#'
#' Same as get_yahoo_fin_tick, but wrapped in a try catch ...
#'
#' @param sym
#' @param nm
#' @param daysback
#'
#' @return
#' @export
#'
#'
get_yahoo_fin_tick_tc <- function(sym,nm,daysback){
  to_dt = lubridate::today()
  fr_dt = to_dt - daysback
  x <- NULL

  tryCatch(
    {
      x <- quantmod::getSymbols(sym, from = fr_dt, to = to_dt, src="yahoo", env=NULL)
    },
      warning = function(w){
      message('A warning occurred')
      print(w)
    },
      error = function(e){
      message('An error occurred')
      print(e)
      x <- NULL
    }
  )

  if(!is.null(x)){
    x <- x[,1:4]
    colnames(x) <- c('Open','High','Low','Close')
    x <- na.omit(x)
    x <- data.frame(Date=index(x), coredata(x)[,], stringsAsFactors = F)
    x$Tick <- nm
    x <- x %>% select(Tick,everything())
    return(x)
  }
}



#' save_qm_data_df
#'
#' Gets data from quantmod
#' SAves it to disk ...
#'
#' @param sym_p
#' @param source_p
#' @param to_date_p
#' @param days_backwards_p
#' @param csv_dir_and_nm
#'
#' @return
#' @export
#'
#'
save_qm_data_df <- function(sym_p,
                            source_p = "yahoo",
                            to_date_p = lubridate::today(),
                            days_backwards_p = 200,
                            csv_dir_and_nm){

  data_set <- NULL
  data_set <- try(qm_gedata_df(sym_p, src_p = source_p, to_dt_p = to_date_p, days_back_p = days_backwards_p))
  if(!is.null(data_set)){
    write_csv(data_set, csv_dir_and_nm)
  }

}

