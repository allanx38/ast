# Data
# tlist <- readRDS("/home/allanx38/ast/Data/FTSE100CloseATR.rds")

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

add_h5 <- function(pdata){
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


add_l5 <- function(pdata){
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


add_C5 <- function(pdata){
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

# use "previous" values
filter_plng <- function(pdata){
  pdata %>% filter(pLS == "L",
                   between(pO_ma25a,0.5,1.5),
                   pC_ma25a > 0,
                   pC_C5a > 0,
                   prn > 2,
                   #tra<1.1
  )
}


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


filter_psht <- function(pdata){
  pdata %>% filter(pLS == "S",
                   between(pma25_Oa,0.5,1.5),
                   pC_ma25a<0,
                   pC_C5a<0,
                   prn>2,
                   #tra<1.1
  )
}




# rev short
filter_rev_sht <- function(pdata){
  pdata %>% filter(LS=="L",          # long
                   rmxH5Ca   <= 0.2, # Close rel to runMax5 high
                   rmxH20Ca <= 0.5, # Close rel to runMax20
                   Close>ma25,      # open higher than ma25
                   prn >= 0)
}


filter_rev_psht <- function(pdata){
  pdata %>% filter(pLS=="L",          # long
                   prmxH5Ca   <= 0.2, # Close rel to runMax5 high
                   prmxH20Ca <= 0.5, # Close rel to runMax20
                   pClose>pma25,      # open higher than ma25
                   prn >= 0)
}

select_rev_sht <- function(pdata){
  pdata %>%
    select(Tick,LS,Date,Open,High,Low,Close,tr,atr,rmxH5Ca, rmxH5C, O_ma25a, C_ma25a, C_C5a) %>%
    adorn_rounding(digits = 0,, c(Open,High,Low,Close,tr,atr, rmxH5C)) %>%
    adorn_rounding(digits = 1,, c(O_ma25a, C_ma25a, C_C5a)) %>%
    adorn_rounding(digits = 2,, rmxH5Ca)
}



# rev long
filter_rev_plng <- function(pdata){
  pdata %>% filter(pLS=="S",         # long
                   prmnL5Ca  <= 0.2, # Close rel to runMax5 high
                   prmnL20Ca <= 0.5, # Close rel to runMax20
                   Open < pma25,     # open higher than ma25
                   prn >= 0)
}

filter_rev_lng <- function(pdata){
  pdata %>% filter(LS=="S",         # long
                   rmnL5Ca  <= 0.2, # Close rel to runMax5 high
                   rmnL20Ca <= 0.5, # Close rel to runMax20
                   Close < pma25,   # open higher than ma25
                   rn >= 0)
}

select_rev_lng <- function(pdata){
  pdata %>%
    select(Tick,LS,Date,Open,High,Low,Close,tr,atr, rmnL5Ca, rmnL5C, rmnL5Ca, ma25_Oa, ma25_Ca, C_C5a) %>%
    adorn_rounding(digits = 0,, c(Open,High,Low,Close,tr,atr,rmnL5C)) %>%
    adorn_rounding(digits = 1,, c(ma25_Oa, ma25_Ca, C_C5a)) %>%
    adorn_rounding(digits = 2,, rmnL5Ca)
}
