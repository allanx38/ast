# Data
tlist <- readRDS("/home/allanx38/ast/Data/FTSE100CloseATR.rds")

# Functions
candleStick_plot <- function(pData){

  pData <- pData %>% mutate(greenRed=ifelse(Open-Close>0,"Red","Green"))

  ggplot(pData) +
    geom_segment(aes(x = Date,
                     xend = Date,
                     y = Open,
                     yend = Close,
                     colour = greenRed),
                 size=3)+
    geom_segment(aes(x = Date,
                     xend = Date,
                     y = High,
                     yend = Low,
                     colour=greenRed)) +

    geom_line(aes(x=Date, y = ma25), color="orange", linetype="longdash", group = 1) +
    geom_line(aes(x=Date, y = ma150), color="red", linetype="longdash", group = 1) +

    scale_color_manual(values=c("Forest Green","Red")) +
    scale_x_date(date_breaks = "1 month", date_labels = "%y%m") +
    ggtitle(pData$Tick[1]) +
    theme(legend.position ="none",
          axis.title.y = element_blank(),
          axis.title.x=element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
          plot.title= element_text(hjust=0.5))
}


get_dow_ticks_csv_list <- function(){
  c("AAPL", "BA",   "CAT",  "CSCO", "CVX",  "DOW",  "GS",   "HD",   "HON",
    "IBM",  "JNJ",  "JPM",  "KO",   "MCD",  "MMM",  "MRK",  "MSFT",
    "NKE",  "PG",   "TRV",  "UNH",  "V",    "VZ",   "WBA",  "WMT",  "XOM" )
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

get_dax_ticks_csv_list <- function(){

  c("1COV.DE", "ADS.DE",  "ALV.DE",  "BAS.DE",  "BAYN.DE", "BEI.DE",  "BMW.DE",  "CON.DE",  "DB1.DE",  "DBK.DE",
    "DHER.DE", "DTE.DE",  "DWNI.DE", "EOAN.DE", "FME.DE",  "FRE.DE",  "HEI.DE",  "HEN3.DE", "IFX.DE",  "LIN.DE",
    "MRK.DE",  "MTX.DE",  "MUV2.DE", "RWE.DE",  "SAP.DE",  "SIE.DE",  "VNA.DE",  "VOW3.DE")
}


get_us_ticks_csv_list <- function() {
c(
  "AAPL","MSFT","AMZN","NVDA","GOOGL","GOOG","TSLA","UNH","META","XOM","JNJ",
"JPM","V","PG","MA","CVX","HD","LLY","MRK","ABBV","AVGO","PEP","KO","PFE","TMO","COST"
,"MCD","WMT","CSCO","BAC","CRM","DIS","ABT","ACN","LIN","ADBE","DHR","VZ","TXN","CMCSA",
"NKE","NEE","PM","NFLX","WFC","BMY","RTX","AMD","ORCL")
  # "BRK.B"
}



add_h5_orig <- function(pdata){
  res_all <- NULL
  pd_tick <- pdata %>% select(Tick) %>% distinct()
  for(i in 1:nrow(pd_tick)){
    loop <- pdata %>% filter(Tick==pd_tick$Tick[i])
    if(nrow(loop)>=3){
      res <- loop %>%
        mutate(H1 = lead(High,n=1),
               H2 = lead(High,n=2),
               H3 = lead(High,n=3),
               H4 = lead(High,n=4),
               H5 = lead(High,n=5)) %>%
        group_by(Tick,Date) %>% mutate(mxH = max(H1,H2,H3,H4,H5, na.rm = T)) %>% ungroup()
      res_all <- bind_rows(res_all, res)
    }
  }
  return(res_all)
}

add_h5<- function(pdata){
  res_all <- pdata %>%
    group_by(Tick) %>%
    mutate(H1 = lead(High,n=1),
           H2 = lead(High,n=2),
           H3 = lead(High,n=3),
           H4 = lead(High,n=4),
           H5 = lead(High,n=5)) %>% ungroup() %>% as_data_frame()
  res_all <- res_all %>%
    group_by(Tick,Date) %>%
    mutate(mxH = max(H1,H2,H3,H4,H5, na.rm = T)) %>% ungroup()
  return(res_all)
}


add_l5_orig <- function(pdata){
  res_all <- NULL
  pd_tick <- pdata %>% select(Tick) %>% distinct()
  for(i in 1:nrow(pd_tick)){
    loop <- pdata %>% filter(Tick==pd_tick$Tick[i])
    if(nrow(loop)>=3){
      res <- loop %>%
        mutate(L1 = lead(Low, n=1),
               L2 = lead(Low, n=2),
               L3 = lead(Low, n=3),
               L4 = lead(Low, n=4),
               L5 = lead(Low, n=5)) %>%
        group_by(Tick,Date) %>% mutate(mnL = min(L1,L2,L3,L4,L5, na.rm = T)) %>% ungroup()
      res_all <- bind_rows(res_all, res)
    }
  }
  return(res_all)
}

add_l5 <- function(pdata){
  res_all <- pdata %>%
    group_by(Tick) %>%
    mutate(L1 = lead(Low, n=1),
           L2 = lead(Low, n=2),
           L3 = lead(Low, n=3),
           L4 = lead(Low, n=4),
           L5 = lead(Low, n=5)) %>%
        group_by(Tick,Date) %>% mutate(mnL = min(L1,L2,L3,L4,L5, na.rm = T)) %>% ungroup()
  return(res_all)
}


add_C5_orig <- function(pdata){
  res_all <- NULL
  pd_tick <- pdata %>% select(Tick) %>% distinct()
  for(i in 1:nrow(pd_tick)){
    loop <- pdata %>% filter(Tick==pd_tick$Tick[i])
    if(nrow(loop)>=3){
      res <- loop %>%
        mutate(C1 = lead(Close, n=1),
               C2 = lead(Close, n=2),
               C3 = lead(Close, n=3),
               C4 = lead(Close, n=4),
               C5 = lead(Close, n=5))
      res_all <- bind_rows(res_all, res)
    }
  }
  return(res_all)
}

add_C5 <- function(pdata){
  res_all <-pdata %>%
    group_by(Tick) %>%
    mutate(C1 = lead(Close, n=1),
           C2 = lead(Close, n=2),
           C3 = lead(Close, n=3),
           C4 = lead(Close, n=4),
           C5 = lead(Close, n=5))
  return(res_all)
}


m_25_150_fnc <- function(f_data){

  f_data <- f_data %>% filter(!is.na(Close))
  f_data <- f_data %>% add_ATR()
  f_data$Date <- as.Date(f_data$Date)

  # 1. ma25/150 X over
  f_data <- f_data %>% mutate(ma5   = TTR::runMean(Close,n = 5),   pma5 = lag(ma5),
                              ma25  = TTR::runMean(Close,n = 25),  pma25 = lag(ma25),
                              ma150 = TTR::runMean(Close,n = 150), pma150 = lag(ma150))

  # 2. Add run Max, div / atr at end - 1 digit ...
  f_data <- f_data %>%
    mutate(rmxH5 =  runMax(High,n=5), prmxH5  = lag(rmxH5),
           rmxH20 = runMax(High,n=20),prmxH20 = lag(rmxH20),
           rmnL5  = runMin(Low,n=5)  ,prmnL5  = lag(rmnL5),
           rmnL20 = runMin(Low,n=20) ,prmnL20 = lag(rmnL20),
           # Add diff mx/mn to Close
           rmxH5C  = rmxH5-Close   ,prmxH5C   = lag(rmxH5C),
           rmxH20C = rmxH20-Close  ,prmxH20C = lag(rmxH20C),
           rmnL5C  = Close-rmnL5   ,prmnL5C  = lag(rmnL5C),
           rmnL20C = Close-rmnL20  ,prmnL20  = lag(rmnL20))

  # 3. various stuff
  f_data <- f_data %>%
    mutate(
      # prev Closes
      pC = lag(Close),   pC5=lag(Close,n=5),
      # OH
      OH = High-Open, maOH3 = TTR::runMean(OH,n = 3),
      OL = Open-Low,  maOL3 = TTR::runMean(OL,n = 3),
      OHDiff = maOH3-maOL3)

  # add LS
  f_data <- f_data %>%
    mutate(LS = if_else(ma25 > ma150 & Close > ma25, "L",
                        if_else(ma25 < ma150 & Close < ma25,"S","N")))

  f_data <- f_data %>% mutate(pLS = lag(LS))
  f_data <- f_data %>% filter(!is.na(pLS))

  # add rn, trn based on LS above
  rn <- 0
  trn <- 0
  f_data$rn    <- 0
  f_data$trn <- 0
  for(i in 1:nrow(f_data)){
    if(f_data$LS[i] != f_data$pLS[i]){
      rn <- 1
      trn <- trn + 1
    } else {
      rn <- rn + 1
    }
    f_data$rn[i] <- rn
    f_data$trn[i] <- trn
  }

  # round 1 digit
  f_data <- f_data %>%
    mutate(
      # divid run max/min by atr - 1 digit
      rmxH5Ca   = (rmxH5-Close) /atr ,prmxH5Ca  = lag(rmxH5Ca),
      rmxH20Ca  = (rmxH20-Close)/atr ,prmxH20Ca = lag(rmxH20Ca),
      rmnL5Ca   = (Close-rmnL5) /atr ,prmnL5Ca  = lag(rmnL5Ca),
      rmnL20Ca  = (Close-rmnL20)/atr ,prmnL20Ca = lag(rmnL20Ca),

      # diff between ma25 and Open/Close
      C_C5a   = (Close-pC5) /atr, pC_C5a   = lag(C_C5a),
      O_ma25a = (Open-ma25) /atr, pO_ma25a = lag(O_ma25a),
      C_ma25a = (Close-ma25)/atr, pC_ma25a = lag(C_ma25a),
      # sht versions
      ma25_Oa = (ma25-Open) /atr, pma25_Oa = lag(ma25_Oa),
      ma25_Ca = (ma25-Close)/atr, pma25_Ca = lag(ma25_Ca),

      # OL
      OLa = (Open-Low)/atr,
      OHa = (High-Open)/atr,

      # tr
      tra = tr/atr, ptra=lag(tra),
      ptr = lag(tr), prn = lag(rn),

      # prices
      OLPr50 = Open - (atr*0.5),   OHPr50 = Open + (atr*0.5),
      OLPr75 = Open - (atr*0.75),  OHPr75 = Open + (atr*0.75),
      OCPr75 = Close - (atr*0.75), OCPr75 = Close + (atr*0.75)
    )

  return(f_data)
}


run_m_25_150_fnc <- function(tail_num = 100){
  res_rd <- NULL
  for(i in 1 : nrow(tlist)){
    rra <- m_25_150_fnc( fd <- read.csv( paste0("/home/allanx38/ast/Data/FTSE_Data/",tlist$Tick[i] ,".csv") ) )
    rra <- rra %>% tail(tail_num)
    res_rd <- bind_rows(res_rd, rra %>% tail(tail_num))
  }
  return(res_rd)
}


run_m_25_150_fnc_dax <- function(tail_num = 100){
  dx <- ast::get_dax_ticks()
  #dx <- dx %>% tail(n=3)
  res_rd <- NULL
  for(i in 1 : nrow(dx)){
    rra <- m_25_150_fnc( fd <- read.csv( paste0("/home/allanx38/ast/Data/Dax_Data/",dx$csv[i]) ) )
    rra <- rra %>% tail(tail_num)
    res_rd <- bind_rows(res_rd, rra %>% tail(n=tail_num))
  }
  return(res_rd)
}


run_m_25_150_csv_list <- function(csv_list, tail_num = 100){
  lp_rd <- NULL
  for(i in 1 : length(csv_list)){
    print(paste0(i, " of ", length(csv_list)))
    lp_data <- get_yahoo_fin_tick(csv_list[i],nm = csv_list[i], daysback=300)
    rra <- m_25_150_fnc( lp_data)
    rra <- rra %>% tail(tail_num)
    lp_rd <- bind_rows(lp_rd, rra %>% tail(n=tail_num))
  }
  return(lp_rd)
}


# Long
  filter_lng <- function(pdata){
    pdata %>% filter(LS == "L",
                     between(O_ma25a,0.5,1.5),
                     C_ma25a > 0,
                     C_C5a > 0,
                     rn > 2
                     #OHDiff > 0
                     #tra<1.1
    )
  }

  add_lng <- function(pdata){
    pdata$L <- "N"
    pdata[which(pdata$LS=="L" &
                between(pdata$O_ma25a,0.5,1.5) &
                pdata$C_ma25a > 0 &
                pdata$C_C5a > 0 &
                pdata$rn > 2), "L"] <- "Y"
    return(pdata)
  }

  filter_plng <- function(pdata){
    pdata %>% filter(pLS == "L",
                     between(pO_ma25a,0.5,1.5),
                     pC_ma25a > 0,
                     pC_C5a > 0,
                     prn > 2,
                     #tra<1.1
    )
  }

  add_plng <- function(pdata){
    pdata$pL <- "N"
    pdata[which(pdata$pLS=="L" &
                between(pdata$pO_ma25a,0.5,1.5) &
                pdata$pC_ma25a > 0 &
                pdata$pC_C5a > 0 &
                pdata$prn > 2), "pL"] <- "Y"
    return(pdata)
  }


# Rev long
  filter_rev_lng <- function(pdata){
    pdata %>% filter(LS=="S",         # long
                     rmnL5Ca  <= 0.2, # Close rel to runMax5 high
                     rmnL20Ca <= 0.5, # Close rel to runMax20
                     Close < pma25,   # open higher than ma25
                     rn >= 0)
  }

  add_rev_lng <- function(pdata){
    pdata$Rev_L <- "N"
    pdata[which(pdata$LS=="S" &
                pdata$rmnL5Ca <= 0.2 &
                pdata$rmnL20Ca <= 0.5 &
                pdata$Close < pdata$pma25 &
                pdata$rn >= 0), "Rev_L"] <- "Y"
    return(pdata)
  }

  select_rev_lng <- function(pdata){
    pdata %>%
      select(Tick,LS,Date,Open,High,Low,Close,tr,atr, rmnL5Ca, rmnL5C, rmnL5Ca, ma25_Oa, ma25_Ca, C_C5a) %>%
      adorn_rounding(digits = 0,, c(Open,High,Low,Close,tr,atr,rmnL5C)) %>%
      adorn_rounding(digits = 1,, c(ma25_Oa, ma25_Ca, C_C5a)) %>%
      adorn_rounding(digits = 2,, rmnL5Ca)
  }

  filter_rev_plng <- function(pdata){
    pdata %>% filter(pLS=="S",         # long
                     prmnL5Ca  <= 0.2, # Close rel to runMax5 high
                     prmnL20Ca <= 0.5, # Close rel to runMax20
                     Open < pma25,     # open higher than ma25
                     prn >= 0)
  }

  add_rev_plng <- function(pdata){
    pdata$Rev_pL <- "N"
    pdata[which(pdata$pLS=="S" &
                pdata$prmnL5Ca <= 0.2 &
                pdata$prmnL20Ca <= 0.5 &
                pdata$Open < pdata$pma25 &
                pdata$prn >= 0), "Rev_pL"] <- "Y"
    return(pdata)
  }

  select_rev_plng <- function(pdata){
    pdata %>%
      select(Tick,LS,Date,Open,High,Low,Close,tr,atr, rmnL5Ca, rmnL5C, rmnL5Ca, ma25_Oa, ma25_Ca, C_C5a) %>%
      adorn_rounding(digits = 0,, c(Open,High,Low,Close,tr,atr,rmnL5C)) %>%
      adorn_rounding(digits = 1,, c(ma25_Oa, ma25_Ca, C_C5a)) %>%
      adorn_rounding(digits = 2,, rmnL5Ca)
  }



# Short
  filter_sht <- function(pdata){
    pdata %>% filter(LS == "S",
                     between(ma25_Oa,0.5,1.5),
                     C_ma25a < 0,
                     C_C5a < 0,
                     rn>2,
                     OHDiff<0
                     #tra<1.1
    )
  }

  add_sht <- function(pdata){
    pdata$S <- "N"
    pdata[which(pdata$LS=="S" &
                  between(pdata$ma25_Oa,0.5,1.5) &
                  pdata$C_ma25a < 0 &
                  pdata$C_C5a < 0 &
                  pdata$rn > 2 &
                  pdata$OHDiff<0), "S"] <- "Y"
    return(pdata)
  }

  filter_psht <- function(pdata){
    pdata %>% filter(pLS == "S",
                     between(pma25_Oa,0.5,1.5),
                     pC_ma25a<0,
                     pC_C5a<0,
                     prn>2,
                     #tra<1.1
    )
  }

  add_psht <- function(pdata){
    pdata$pS <- "N"
    pdata[which(pdata$LS=="S" &
                  between(pdata$pma25_Oa,0.5,1.5) &
                  pdata$pC_ma25a < 0 &
                  pdata$pC_C5a < 0 &
                  pdata$prn > 2), "S"] <- "Y"
    return(pdata)
  }

# rev short
  filter_rev_sht <- function(pdata){
    pdata %>% filter(LS == "L",          # long
                     rmxH5Ca  <= 0.2, # Close rel to runMax5 high
                     rmxH20Ca <= 0.5, # Close rel to runMax20
                     Close>ma25,      # open higher than ma25
                     rn >= 0)
  }

  add_rev_sht <- function(pdata){
    pdata$Rev_S <- "N"
    pdata[which(pdata$LS == "L" &
                pdata$rmxH5Ca <= 0.2 &
                pdata$rmxH20Ca <= 0.5 &
                pdata$Close > pdata$ma25 &
                pdata$rn > 2), "Rev_S"] <- "Y"
    return(pdata)
  }

  select_rev_sht <- function(pdata){
    pdata %>%
      select(Tick,LS,Date,Open,High,Low,Close,tr,atr,rmxH5Ca, rmxH5C, O_ma25a, C_ma25a, C_C5a) %>%
      adorn_rounding(digits = 0,, c(Open,High,Low,Close,tr,atr, rmxH5C)) %>%
      adorn_rounding(digits = 1,, c(O_ma25a, C_ma25a, C_C5a)) %>%
      adorn_rounding(digits = 2,, rmxH5Ca)
  }



  filter_rev_psht <- function(pdata){
    pdata %>% filter(pLS=="L",          # long
                     prmxH5Ca   <= 0.2, # Close rel to runMax5 high
                     prmxH20Ca <= 0.5, # Close rel to runMax20
                     pClose>pma25,      # open higher than ma25
                     prn >= 0)
  }

  add_rev_psht <- function(pdata){
    pdata$Rev_pS <- "N"
    pdata[which(pdata$pLS=="L" &
                pdata$prmxH5Ca  <= 0.2 &
                pdata$prmxH20Ca <= 0.5 &
                pdata$pClose > pdata$pma25 &
                pdata$prn > 2), "Rev_pS"] <- "Y"
    return(pdata)
  }







# Select trade functions

Trade_All <- function(pdata, rday) {

  lng_rev <- Trade_Long_Rev_From_Short(pdata, rday)
  lng_pb  <- Trade_Long_PB(pdata, rday)

  sht_rev <- Trade_Short_Rev_From_Long(pdata, rday)
  sht_pb  <- Trade_Short_PB(pdata, rday)

  tr_list <- list(lng_rev = lng_rev,
                  lng_pb = lng_pb,
                  sht_rev = sht_rev,
                  sht_pb = sht_pb)
  return(tr_list)
}


Trade_Short_Rev_From_Long <- function(pdata, rday) {
  sht_rev <- NULL
  sht_rev <- pdata %>% filter(Date==rday) %>%
    filter_rev_sht()
  sht_rev <- sht_rev %>%
    select_rev_sht() %>%
    mutate(ShtRevOpenPr = Close + (atr*0.75)) %>%
    arrange(desc(O_ma25a))
  return(sht_rev)
}


Trade_Short_PB <- function(pdata, rday, atr_off_p = 0.2) {
  sht_pb <- NULL
  atr_off <- atr_off_p
  sht_pb <- pdata %>% filter(Date==rday) %>% filter_sht()
  sht_pb <- sht_pb %>% mutate(ShtOpenPB = Close + (atr*0.5))
  sht_pb <-sht_pb %>% mutate(ShtOpenMn = rmxH5-(atr*atr_off), GapMn = (rmxH5-Close)/atr) %>% adorn_rounding()
  sht_pb <- sht_pb %>% select(Tick,LS,1:6,atr,rmnL5C,rmnL5Ca,rmnL20Ca,rn,ShtOpenPB,ShtOpenMn,GapMn) %>%
    arrange(GapMn)
  return(sht_pb)
}


Trade_Long_Rev_From_Short <- function(pdata, rday) {
  lng_rev <- NULL
  lng_rev <-pdata %>% filter(Date == rday) %>% filter_rev_lng()
  lng_rev <- lng_rev %>%
    select_rev_lng() %>%
    mutate(LngRevOpenPr = Close - (atr*0.75)) %>%
    arrange(desc(ma25_Oa))
  return(lng_rev)
}


Trade_Long_PB <- function(pdata, rday, atr_off_p = 0.2) {
  lng_pb <- NULL
  #rday <- "2024-03-11"
  atr_off <- 0.2
  lng_pb <-pdata %>% filter(Date==rday) %>% filter_lng()
  lng_pb <- lng_pb %>% mutate(LngOpenPB = Close - (atr*0.5))
  lng_pb <- lng_pb %>% mutate(LngOpenMx = rmnL5+(atr*atr_off), GapMx = (Close-rmnL5)/atr) %>% adorn_rounding()
  lng_pb <- lng_pb %>% select(Tick,LS,1:6,atr,rmxH5,rmxH5Ca,rmxH20Ca,rn,LngOpenPB,LngOpenMx,GapMx) %>%
    arrange(GapMx)
}

