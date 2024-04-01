

library(ggplot2)
ast::load_libs()

tlist <- readRDS("/home/allanx38/ast/Data/FTSE100CloseATR.rds")



# reads data from disc
# adds atr, ma, LS, rn, trn
# CL - when price crosses ma150
m_25_fnc <- function(tck_nm){
  #print(paste0(tlist$Tick[tck_nm], " - ", tck_nm))
  f_data <- read.csv( paste0("/home/allanx38/ast/Data/FTSE_Data/",tlist$Tick[tck_nm] ,".csv") )
  f_data <- f_data %>% filter(!is.na(Close))
  f_data <- f_data %>% add_ATR()
  f_data$Date <- as.Date(f_data$Date)
  f_data <- f_data %>% mutate(nxtClose = lead(Close))

  # 1. ma25/150 X over
  # Add ma25 / ma150, LS, rn,trn
    f_data <- f_data %>% mutate(ma25 = TTR::runMean(Close,n = 25),
                                ma150 = TTR::runMean(Close,n = 50)) %>%
      mutate(MaLS = if_else(ma25 > ma150, "L", "S")) %>%
      mutate(pMaLS = lag(MaLS))
    f_data <- f_data %>% filter(!is.na(pMaLS))
    f_data <- add_trn(f_data, "rn", "trn", "MaLS", "pMaLS")

  #Add Signal
  #f_data <- f_data %>% mutate(Sig = if_else(LS != pLS, 1, 0))

  # 2. ma150 X over
  # Add CL whether Close > ma150, cLS, crn, ctrn
    f_data <- f_data %>% mutate(cLS = if_else(Close > ma150, "L", "S"),
                                pcLS = lag(cLS))
    f_data <- f_data %>% filter(!is.na(pcLS))
    f_data <- add_trn(f_data, "crn", "ctrn", "cLS", "pcLS")

    # 3. ma max X over - pLS
    f_data <- f_data %>% group_by(Date) %>% mutate(mxma = max(ma25,ma150), mnma = min(ma25,ma150)) %>%
      ungroup() %>% as.data.frame()
    f_data <- f_data %>% mutate(pLS = if_else(Close > mxma, "L",
                                                if_else(Close < mnma, "S", "M")),
                                ppLS = lag(pLS))

    f_data <- f_data %>% filter(!is.na(ppLS))
    f_data <- add_trn(f_data, "prn", "ptrn", "pLS", "ppLS")

  # ma25 X over price - ma25LS
    f_data <- f_data %>% mutate(ma25LS = if_else(Close > ma25, "L", "S"),
                                pma25LS = lag(ma25LS))
    f_data <- f_data %>% filter(!is.na(pma25LS))
    f_data <- add_trn(f_data, "rn25", "trn25", "ma25LS", "pma25LS")

  # High / Low and previous
    f_data <- f_data %>% mutate(pHigh = lag(High),
                                pLow = lag(Low),
                                sig_H = if_else(Close>pHigh,1,0),
                                sig_L = if_else(Close<pLow,1,0))

  # Add a row number
  f_data <- f_data %>% mutate(row_num = row_number())
  return(f_data %>% adorn_rounding(digits = 0))
}

# MaLS   - ma25/150 X over MAs - "rn", "trn", "MaLS", "pMaLS"
# cLS    - ma150 X over price - "crn", "ctrn", "cLS", "pcLS"
# pLS    - ma max X over price - "prn", "ptrn", "pLS", "ppLS"
# ma25LS - ma25 X over price - "rn25", "trn25", "ma25LS", "pma25LS"

# HELP FUNCTIOS

# generic fnc to add trn, rn
# needs a LS type column
# sppecify them in paras
# rn_nm, trn_nm - output names
# LS_nm, pLS_nm - in data set to start with
add_trn <- function(pdata, rn_nm, trn_nm, LS_nm, pLS_nm){
  rn_num <- 0
  tr_num <- 0
  pdata[[rn_nm]]    <- 0
  pdata[[trn_nm]] <- 0
  for(i in 1:nrow(pdata)){
    if(pdata[[LS_nm]][i] != pdata[[pLS_nm]][i]){
      rn_num <- 1
      tr_num <- tr_num + 1
    } else {
      rn_num <- rn_num + 1
    }
    pdata[[rn_nm]][i] <- rn_num
    pdata[[trn_nm]][i] <- tr_num
  }
  return(pdata)
}


add_lead_H <- function(pdata){
  pdata <- pdata %>% mutate(
                mH1 = lead(High, n=1),
                mH2 = lead(High, n=2),
                mH3 = lead(High, n=3),
                mH4 = lead(High, n=4),
                mH5 = lead(High, n=5),
                mL1 = lead(Low, n=1),
                mL2 = lead(Low, n=2),
                mL3 = lead(Low, n=3),
                mL4 = lead(Low, n=4),
                mL5 = lead(Low, n=5))
  pdata <- pdata %>% group_by(Date) %>%
    mutate(mxH = max(mH1,mH2,mH3,mH4,mH5, na.rm = T),
           mnL = min(mL1,mL2,mL3,mL4,mL5, na.rm = T)) %>%
    ungroup() %>% as.data.frame()
  return(pdata)
}



add_ma25 <- function(f_data){
  f_data %>% mutate(ma25 = TTR::runMean(Close,n = 25))
}

add_ma150 <- function(f_data){
  f_data %>% mutate(ma150 = TTR::runMean(Close,n = 150))
}


# loop through files
# pass start, end, function
res_all_fnc <- function(st_num, end_num, FUN){
  res_all <- NULL
  for(i in st_num:end_num){
    res <- FUN(i)
    res_all <- bind_rows(res_all, res)
  }
  return(res_all)
}


res_all_fnc_tail <- function(st_num, end_num, FUN, tail_num = 6){
  res_all <- NULL
  for(i in st_num:end_num){
    res <- FUN(i)
    res_all <- bind_rows(res_all, res %>% tail(n = tail_num))
  }
  return(res_all)
}


sum_trn_fnc <- function(pdata, ...){
  pdata %>%
    group_by(...) %>%
    summarise(Tick = first(Tick),
              OpenD = first(Date),
              CloseD = last(Date),
              OpenP = first(Open),
              CloseP = last(nxtClose),
              atr = first(atr),
              mh = max(High),
              lplatr = (CloseP-OpenP)/atr,
              lpl_per = ((CloseP-OpenP)/OpenP)*100,
              hlplatr = (mh-OpenP)/atr,
              hlpl_per = (mh-OpenP)/OpenP*100,
              spl = OpenP-CloseP,
              splatr = spl/atr,
              spl_per = (spl/OpenP)*100,
              ml = min(Low),
              n = n()) %>% ungroup()
}


tlist <- readRDS("/home/allanx38/ast/Data/FTSE100CloseATR.rds")

#rr <- res_all_fnc(1,92,m_25_fnc)
#saveRDS(rr, "/home/allanx38/ast/Data/FTSEna25_150_btest.rds")
# rr <- readRDS("/home/allanx38/ast/Data/FTSEna25_150_btest.rds")


# MaLS   - ma25/150 X over MAs - "rn", "trn", "MaLS", "pMaLS"
# cLS    - ma150 X over price - "crn", "ctrn", "cLS", "pcLS"
# pLS    - ma max X over price - "prn", "ptrn", "pLS", "ppLS"
# ma25LS - ma25 X over price - "rn25", "trn25", "ma25LS", "pma25LS"

ast::update_ftse_tick_saved()
source("/home/allanx38/ast/Trade/macd_fnc.R")
ast::load_libs()

rr_today <- res_all_fnc_tail(1,92,m_25_fnc, tail_num = 20)
unique(rr_today$Date)


# 1. ma25 > ma150
  # a. close above ma25

# 2. close > max ma and ma25 < ma150

pd <- read.csv("/home/allanx38/ast/Data/FTSE_Data/WTB.L.csv")
pd <- pd %>% add_ma25() %>% add_ma150()
pd$Date <- as.Date(pd$Date)

ast::gg_ma25_150(pd %>% tail(n=300))

# Option 1
  rr <- m_25_fnc(75)
  rr <- rr %>% add_h5() %>% add_l5()
  rr <- rr %>% mutate(ln_num = row_number(),
                      LS = if_else(MaLS == "L" & pLS == "L", 1, 0),
                      OL = (Open-Low)/atr,
                      OLPr = Open - (atr/2),
                      pl = mxH - OLPr,
                      platr = pl/atr,
                      mnpl = mnL - OLPr,
                      maO = (Open-ma25)/atr)

# Option 2
  rr <- m_25_150_fnc( fd <- read.csv( paste0("/home/allanx38/ast/Data/FTSE_Data/",tlist$Tick[5] ,".csv") ) )
  rr <- rr %>% add_h5() %>% add_l5()
  rr <- rr %>% mutate(OL = (Open-Low)/atr,
                      OLPr = Open - (atr/2),
                      pl = mxH - OLPr,
                      platr = pl/atr,
                      mnpl = mnL - OLPr,
                      maO = (Open-ma25)/atr)
  rr$Tick[1]
  rr %>% filter(LS == "L", between(maO, 0, 1)) %>% count()
  rr %>% filter(LS == "L", OL >= 0.5, between(maO, 0, 1)) %>% count()

rr_res <- rr %>% filter(LS == "L", OL >= 0.5) %>%
  select(1:8,OL,OLPr,pl,platr,maO,mnpl,mxH,mnL,ma25,ma150,rn) %>% adorn_rounding() %>% arrange(platr)

rr_res <- rr_res %>% mutate(win = cut(platr, breaks=c(-10, 0.5, 1, 100), labels = c("a","b","c")))
# rr_res %>% View()

rr_res %>% group_by(win) %>% tally()
ap <- function(pdata){pdata %>% adorn_percentages(denominator = "col") %>% adorn_pct_formatting()}

rr_res %>% filter(between(maO, 0, 1)) %>% group_by(Tick,win) %>% tally() %>% ap
rr_res %>% filter(between(maO, 1, 2)) %>% group_by(win) %>% tally() %>% ap
rr_res %>% filter(between(maO, 2, 3)) %>% group_by(win) %>% tally() %>% ap
rr_res %>% filter(between(maO, 3, 6)) %>% group_by(win) %>% tally() %>% ap

rr_res %>% filter(between(rn,2, 10)) %>% group_by(win) %>% tally()
rr_res %>% filter(between(rn,10,30)) %>% group_by(win) %>% tally()

rr_res %>% filter(between(maO, 0, 1),between(rn,8,20)) %>% group_by(win) %>% tally()

rr_res %>% group_by() %>% summarise(mn = mean(platr))
rr_res %>% filter(maO <= 1) %>% group_by() %>% summarise(mn = mean(platr))
rr_res %>% filter(between(maO, 1, 2)) %>% group_by() %>% summarise(mn = mean(platr))
rr_res %>% filter(between(maO, 2, 3)) %>% group_by() %>% summarise(mn = mean(platr))
rr_res %>% filter(between(maO, 3, 4)) %>% group_by() %>% summarise(mn = mean(platr))
rr_res %>% filter(between(maO, 4, 6)) %>% group_by() %>% summarise(mn = mean(platr))

rr %>% filter(Date >= "2022-09-15") %>% select(1:6,atr) %>% head(n=20)


# loop long
res_all <- NULL
for(i in 1 :90){
  rr <- m_25_150_fnc( fd <- read.csv( paste0("/home/allanx38/ast/Data/FTSE_Data/",tlist$Tick[i] ,".csv") ) )
  rr <- rr %>% add_h5() %>% add_l5()
  rr <- rr %>% mutate(OL = (Open-Low)/atr,
                      OLPr = Open - (atr/2),
                      pl = mxH - OLPr,
                      platr = pl/atr,
                      mnpl = mnL - OLPr,
                      maO = (Open-ma25)/atr)

  rr_res <- rr %>% filter(LS == "L", OL >= 0.5) %>%
    select(1:8,OL,OLPr,pl,platr,maO,mnpl,mxH,mnL,ma25,ma150,rn) %>% adorn_rounding() %>% arrange(platr)

  rr_res <- rr_res %>% mutate(win = cut(platr, breaks=c(-10, 0.5, 1, 100), labels = c("a","b","c")))
  res <- rr_res %>% filter(between(maO, 0, 1)) %>% group_by(Tick,win) %>% tally() # %>% ap
  res_all <- bind_rows(res_all, res)
}
res_all_lng_pb <- res_all %>% pivot_wider(names_from = win, values_from = n) %>%
  adorn_percentages(denominator = "row") %>% arrange(a) %>%
  adorn_pct_formatting()
res_all_lng_pb %>% View()

res_all %>% filter(!is.na(win)) %>% pivot_wider(names_from = win, values_from = n) %>%
  adorn_percentages(denominator = "row") %>% arrange(a) %>%
  adorn_pct_formatting() %>% View()

# loop short
res_all_s <- NULL
for(i in 1 :90){
  rr <- m_25_150_fnc( fd <- read.csv( paste0("/home/allanx38/ast/Data/FTSE_Data/",tlist$Tick[i] ,".csv") ) )
  rr <- rr %>% add_h5() %>% add_l5()
  rr <- rr %>% mutate(OH = (High-Open)/atr,
                      OHPr = Open + (atr/2),
                      pl = OHPr - mnL,
                      platr = pl/atr,
                      maO = (ma25 - Open)/atr)

  rr_res <- rr %>% filter(LS == "S", OH >= 0.5) %>%
    select(1:8,OH,OHPr,pl,platr,maO,mxH,mnL,ma25,ma150,rn) %>% adorn_rounding() %>% arrange(platr)

  rr_res <- rr_res %>% mutate(win = cut(platr, breaks=c(-10, 0.5, 1, 100), labels = c("a","b","c")))
  res <- rr_res %>% filter(between(maO, 0, 1)) %>% group_by(Tick,win) %>% tally() # %>% ap
  res_all_s <- bind_rows(res_all_s, res)
}

res_all_sht_pb <- res_all_s %>% pivot_wider(names_from = win, values_from = n) %>%
  adorn_percentages(denominator = "row") %>% arrange(s_a) %>%
  adorn_pct_formatting()
res_all_sht_pb %>% View()

# join lng and sht
res_lng_sht_pb <- full_join(
res_all_s %>% pivot_wider(names_from = win,names_prefix = "s_", values_from = n) %>%
  adorn_percentages(denominator = "row"),
res_all %>% filter(!is.na(win)) %>% pivot_wider(names_from = win,names_prefix = "l_", values_from = n) %>%
  adorn_percentages(denominator = "row"), by = join_by(Tick)
)

res_lng_sht_pb <- res_lng_sht_pb %>% arrange(s_a) %>% ungroup() %>% mutate(s_rnk = row_number())
res_lng_sht_pb <- res_lng_sht_pb %>% arrange(l_a) %>% ungroup() %>% mutate(l_rnk = row_number())
res_lng_sht_pb <- as.data.frame(res_lng_sht_pb)

res_lng_sht_pb %>% select(Tick,starts_with("s")) %>% adorn_pct_formatting(,,, s_a,s_b,s_c)


# Save
  saveRDS(res_lng_sht_pb,"/home/allanx38/ast/Data/res_lng_sht_pb.rds")



# today PB ----------------------------------------------------------------

  ast::load_libs()
  library(stringr)
  library(ggplot2)

  tlist <- readRDS("/home/allanx38/ast/Data/FTSE100CloseATR.rds")
  tlist$Tick

  ast::update_ftse_tick_saved()
  read.csv("/home/allanx38/ast/Data/FTSE_Data/HL.L.csv") %>% tail(n=10)

  #debugonce(run_m_25_150_fnc)
  res_rd <- run_m_25_150_fnc(tail_num = 100)
  res_rd <- res_rd %>% mutate(atrper = (atr/Open)*100) %>% adorn_rounding()
  #saveRDS(res_rd, "/home/allanx38/ast/Data/ma150_25_pb.rds")
  #res_rd %>% filter(LS == "S", Date >= "2024-01-01", between(maLO,0.5,1.5))

  # create html file
  # load create res_rd above
  rmarkdown::render(input = "/home/allanx38/ast/Trade/RunRMD/FTSE_ma5_25_150.R",
                    params = list(tday = "2024-02-15"),
                    output_file = "/home/allanx38/ast/Trade/RunRMD/FTSE_ma5_25_150.html")


  candleStick_plot(res_rd %>% filter(Tick=="AAL.L") %>% tail(200))

  # Open
  #res_rd %>% filter(Tick == "AUTO.L") %>% tail(n=9)
  #res_rd %>% filter(Tick == "CNA.L") %>% tail(n=9)
  #res_rd %>% filter(Tick == "BATS.L") %>%

  ail(n=9)
  #res_rd %>% filter(Tick == "SVT.L") %>% tail(n=9)
  #res_rd %>% filter(Tick == "UU.L") %>% tail(n=9)
                                                                                                                                                                                            unique(res_rd$Tick)
  # Today
    # long
    rday <- "2024-02-08"
    #res_lng_sht_pb <- readRDS("/home/allanx38/ast/Data/res_lng_sht_pb.rds")

    lng_td <- res_rd %>% filter(Date==rday) %>% sel_lng() %>% adorn_rounding()
    sht_td <- res_rd %>% filter(Date==rday) %>% sel_sht() %>% adorn_rounding()

    lng_td %>% select(1:11,LS,rn,maLO,maLC,tra,C_ma5,C_C5) %>% adorn_rounding()
    sht_td %>% select(1:11,LS,rn,maSO,maSC,tra,C_ma5,C_C5) %>% adorn_rounding()

    # long
    lng_td %>% select(1:11,rn,maLO,maLC,tra,C_ma5,C_C5,OHDiff) %>% adorn_rounding() %>% select(1:8) %>% View()
    res_rd %>% filter(Tick == lng_td$Tick[4]) %>% select(1:11,LS,rn,maLO,maLC,OL) %>%
      mutate(OC = Close-Open) %>% adorn_rounding() %>% tail(n=10)

  # short
    sht_td %>% select(1:11,LS,rn,maSO,maSC,tra,C_ma5,C_C5) %>% adorn_rounding() %>% select(1:8) %>% View()
    res_rd %>% filter(Tick == sht_td$Tick[3]) %>%
      mutate(OC = Close-Open) %>% adorn_rounding() %>% select(1:11,LS,rn,maSO,maSC,OH,OC) %>% tail(n=9)

  # ma25 outliers ...
    res_rd %>% filter(Date==rday) %>% filter(maLC>3 | maLC < -3) %>%
      filter(pLS!= "N") %>% select(1:8,maLC)



# PB vs Rev ---------------------------------------------------------------

  lng_pb_t <-ftse100 %>% filter_lng()

  sht_rev_t <-ftse100 %>% filter_rev_sht()

  PB_L_Rev_S  <- inner_join(
    lng_pb_t %>% select(Date,Tick),
    sht_rev_t, by = join_by(Date, Tick) ) %>%
    mutate(ShtRevOpenPr = Close + (atr*0.75),
           LngOpenPB = Close - (atr*0.5),
           LngOpenMx = rmnL5+(atr*0.2),
           GapMx = (Close-rmnL5)/atr) %>%
    select(Date,Tick,1:6,atr,ShtRevOpenPr,LngOpenPB,LngOpenMx,GapMx) %>%
    arrange(Date) %>% adorn_rounding()
  PB_L_Rev_S %>% View()

  PB_L_Rev_S$Date[357]

  rn <- 359
  dt <- PB_L_Rev_S$Date[rn]
  dt2 <- PB_L_Rev_S$Date[rn]+1
  tc <- PB_L_Rev_S$Tick[rn]
  left_join(
  ftse100 %>% filter(Date >= dt2, Tick == tc) %>% select(Date,Tick,1:6,atr),
  PB_L_Rev_S %>% filter(Date == dt, Tick == tc) %>% select(Tick,ShtRevOpenPr,LngOpenPB,LngOpenMx,GapMx),
  by = join_by(Tick)) %>% head() %>% adorn_rounding()


# PB Test -----------------------------------------------------------------

    ast::load_libs()
    ftse100 <- run_m_25_150_fnc(tail_num = 500)
    ftse100 <- res_rd %>% mutate(atrper = (atr/Open)*100) %>% adorn_rounding()


  #1. Add H5/L5, filter on Long
  rs <- ftse100 %>% filter(Date >= "2023-10-01") %>% add_h5() %>% add_l5() %>% add_C5() %>%
    ungroup() %>% as_data_frame()

  rs <- add_lng(rs)
  rs <- add_plng(rs)
  rs <- add_rev_lng(rs)
  rs <- add_rev_plng(rs)

  rs <- add_sht(rs)
  rs <- add_psht(rs)
  rs <- add_rev_sht(rs)
  rs <- add_rev_psht(rs)


  # Long, from Open
  # Add some cols
  rs_l <- rs %>%
    mutate(pDate = lag(Date),
           pOpen = lag(Open),
           OPr = pOpen) %>%
    mutate(mxpl = (mxH-OPr)/atr,
           pl0 = (High-OPr)/atr, pm0 = (Low-OPr)/atr,
           pl1 = (H1-OPr)/atr, pm1 = (L1-OPr)/atr,
           pl2 = (H2-OPr)/atr, pm2 = (L2-OPr)/atr,
           pl3 = (H3-OPr)/atr, pm3 = (L3-OPr)/atr,
           pl4 = (H4-OPr)/atr, pm4 = (L4-OPr)/atr,
           pl5 = (H5-OPr)/atr, pm5 = (L5-OPr)/atr) %>%
    filter(Date >= "2024-01-01", Date <= "2024-03-01")

    # pLS=="L" &
    # between(pO_ma25a,0.5,1.5) &
    # pC_ma25a > 0 &
    # pC_C5a > 0 &
    # prn

  rs_l <- rs_l %>%
    select(pLS,pO_ma25a,pC_ma25a,pC_C5a,prn,pL,pDate,LS,pOpen,1:6,atr,OPr,mxpl,
           pl0,pm0,pl1,pm1,pl2,pm2,pl3,pm3,pl4,pm4,pl5,pm5,H1,H2,H3,H4,H5) %>%
    adorn_rounding()

  rs_l %>% View()

  l_trades_all <- rs_l %>% filter(pL=="Y") %>% adorn_rounding()
  l_trades_all %>% select(Tick,Date,pOpen,Open,High,Low,Close,atr,pm0,pl0,pl1,pl2,pl3,pl4,pl5) %>% View()
  l_trades_all <- l_trades_all %>% mutate(Hit = if_else(pm0 <= -1, "Y","N"))
  l_trades_all %>% group_by(Date,Hit) %>% tally() %>%
    pivot_wider(names_from = Hit, values_from = n) %>% View()

  l_trades <- l_trades_all %>% filter(pm0 <= -1)
  l_trades <- l_trades %>% select(Tick,Date,pOpen,Open,High,Low,Close,atr,pm0,pl0,pl1,pl2,pl3,pl4,pl5)
  l_trades %>% View()

  l_trades <- left_join(
    l_trades,
    l_trades %>% group_by(Date,Tick) %>% mutate(mx = max(pl1,pl2,pl3,pl4,pl5)) %>%
      select(Date,Tick,mx), by = join_by(Date, Tick))

  l_trades <- l_trades %>% mutate(PL1 = mx-(-1))
  l_trades <- l_trades %>% mutate(PL2 = if_else(pm0 <= -2, mx-(-2), 0))
  l_trades <- l_trades %>% mutate(PL = PL1+PL2) %>% adorn_rounding()
  l_trades %>% select(1:7,mx,PL,everything()) %>% View()

  ck <- rs_l %>% filter(Tick=="ANTO.L",Date>="2024-02-19") %>%
    select(LS,pLS,pO_ma25a,pC_ma25a,pC_C5a,prn,pL,Tick,Date,Open,High,Low,Close,atr) %>%
    head(n=15) %>% adorn_rounding()
  # %>% View()





  # LONG
  # which were in scope? Use prev values
  rsl <- rs %>% filter_plng()

  # which pulled back
  rsl_pb <- rsl %>% filter(OLa >= 0.5) %>% mutate(Dy = lubridate::wday(Date))
  #rsl_pb %>% select(Date) %>% distinct() %>% arrange(Date)
  rsl_pb2 <- rsl_pb %>%
    filter(Date >= "2023-11-01") %>%
    mutate(mxpl = (mxH-OLPr50)/atr,
           L0a = (Low-OLPr50)/atr, H0a = (High-OLPr50)/atr, C0a = (Close-OLPr50)/atr,
           L1a = (L1-OLPr50)/atr, H1a = (H1-OLPr50)/atr, C1a = (C1-OLPr50)/atr,
           L2a = (L2-OLPr50)/atr, H2a = (H2-OLPr50)/atr, C2a = (C2-OLPr50)/atr,
           L3a = (L3-OLPr50)/atr, H3a = (H3-OLPr50)/atr, C3a = (C3-OLPr50)/atr,
           L4a = (L4-OLPr50)/atr, H4a = (H4-OLPr50)/atr, C4a = (C4-OLPr50)/atr,
           L5a = (L5-OLPr50)/atr, H5a = (H5-OLPr50)/atr, C5a = (C5-OLPr50)/atr) %>%
    adorn_rounding() %>%
    select(pLS,1:6,8,mxpl,OLPr50,L0a,H0a,C0a,L1a,H1a,C1a,L2a,H2a,C2a,L3a,H3a,C3a,L4a,H4a,C4a,L5a,H5a,C5a)
  rsl_pb2 %>%  View()

  rsl_pb2 %>% select(Date,Tick,mxpl,C0a,H1a,H2a,H3a,H4a,H5a) %>% filter(C0a >= -0.5) %>% View()

  rsl_pb %>% select(1:6,8,OL,mxH,rn,ptr,OLPr50,rmxH5,rmxH20,H1,H2,H3,H4,H5) %>%
    mutate(mxpl = (mxH-OLPr50)/atr) %>% filter(mxpl != "-Inf") %>% adorn_rounding() %>% View()

  rsl %>% filter(Tick=="RIO.L", Date>="2023-11-15") %>% head(n=10) %>%
    mutate(PTarg = OLPr50+atr) %>%
    select(1:10,OLPr50,PTarg,rmxH5,rmxH5Ca,rmxH20Ca) %>% adorn_rounding()

  # 5 day low
  atr_off <- 0.2
  rsl_mn <- rsl %>%
    mutate(OpenP = rmnL5+(atr*atr_off),
           mxpl = (mxH-OpenP)/atr,
           Gap = (pC-OpenP)/atr) %>%
    filter(Low <= OpenP, rn >= 3) %>%
    select(1:6,8,pC,OpenP,Gap,mxH,mxpl,rmnL5,rmxH5,rmxH20,rn,H1,H2,H3,H4,H5) %>%
    adorn_rounding()
  rsl_mn %>% View()
  rsl_mn %>% filter(mxpl != "-Inf") %>% group_by() %>% summarise(mn = mean(mxpl))
  round( nrow(rsl_mn) / nrow(rsl), 2)

  # 5 day high
  atr_off <- 0
  rss_mx <- rss %>% mutate(OpenP = rmxH5-(atr*atr_off),
                           mxpl = (OpenP-mnL)/atr) %>%
    filter(High >= OpenP, rn >= 3) %>%
    select(1:6,8,pC,OpenP,mnL,mxpl,rmxH5,rmnL5,rmnL20,rn,L1,L2,L3,L4,L5) %>%
    adorn_rounding()
  rss_mx %>% View()
  rss_mx %>% filter(mxpl != "-Inf") %>% group_by() %>% summarise(mn = mean(mxpl))
  round( nrow(rsl_mn) / nrow(rsl), 2)


  # SHORT
  rss <- rs %>% filter_psht()
  rss_pb <- rss %>% filter(OHa >= 0.5)
  rss_pb %>% select(1:6,8,OL,mnL,rn,ptr,OHPr50,rmnL5Ca,rmnL20Ca,L1,L2,L3,L4,L5) %>%
    mutate(mxpl = (OHPr50-mnL)/atr) %>% filter(mxpl != "-Inf") %>% adorn_rounding() %>% View()






  # run PB function ...
  rsl_pb_res <- pb_lng_res(rsl_pb %>% filter(between(Date,as.Date("2023-10-01"),as.Date("2024-01-31")),
                                             Dy < 9),
                                             ptarg_fac = 1, sloss_fac = 1)

  res_l <- rsl_pb_res %>% filter(mxH != -Inf) %>% mutate(platr = pl/atr) %>%
    select(Dy,atr,platr,pl,OLPr,mxH,mxpl,mxpldy,mnpl,mnpldy, starts_with("H"), everything()) %>%
    mutate(wl = if_else(pl>=0,"w","L")) %>% adorn_rounding()

  res_l %>% View()
  res_l %>% filter(mxH != -Inf) %>% group_by(wl) %>% summarise(wn = n())
  res_l %>% filter(mxH != -Inf) %>% group_by(Dy) %>% summarise(sm = sum(platr), n=n())
  res_l %>% filter(mxH != -Inf) %>%  ungroup() %>% summarise(sm = sum(platr), n=n())

  res_rd %>% filter(Tick=="STAN.L", Date>="2023-12-01") %>% head()

  # results with no PB ... set OLPr to Open
  res <- rsl_s %>%
    filter(between(Date,as.Date("2023-01-10"),as.Date("2024-01-31")), OL >= 0.5) %>%
    #filter(between(Date,as.Date("2023-01-01"),as.Date("2024-01-30"))) %>%
    #mutate(OLPr = Open) %>%
    pb_lng_res(ptarg_fac = 2, sloss_fac = 1) %>% mutate(platr = pl/atr) %>%
    select(pOC,atr,platr,pl,OLPr,mxH,mxpl,mxpldy,mnpl,mnpldy, everything()) %>% adorn_rounding()

   res %>% filter(mxH != -Inf) %>% ungroup() %>% summarise(sm = sum(platr), n=n())
   res %>% View()

   rsl_l %>% filter(mxH != -Inf) %>% mutate(mxpl = (mxH - Open)/atr) %>%
     select(1:11,mxH,mnL,mxpl) %>% adorn_rounding() %>% View()

   res_rd %>% filter(Tick == "AAL.L") %>% select(1:11,LS) %>% View()

   # Short
   #1. Add H5/L5, filter on Long
   rss <- res_rd %>% filter(LS=="S", Date >= "2023-10-01") %>% add_h5() %>% add_l5() %>% add_C5()

   # which were in scope? Use prev values
   rsl_s <- rss %>% sel_psht()

   # which pulled back, UP
   rss_pb <- rsl_s %>% filter(OH >= 0.5, Date < "2024-01-31")
   rss_pb %>% select(1:6,8,OH,OHPr,mnL,LS,rn,maSO,ptr,ptra,pma5,pC5) %>%
     mutate(mxpl = (OHPr-mnL)/atr) %>% adorn_rounding() %>% View()

   # run PB function ...
   rss_pb_res <- pb_sht_res(rss_pb,ptarg_fac = 0.5, sloss_fac = 2)
   res_s <- rss_pb_res %>% mutate(platr = pl/atr) %>% filter(mxH != -Inf) %>%
     select(atr,platr,pl,OHPr,mnL,mxpl,mxpldy,mnpl,mnpldy, everything()) %>% adorn_rounding()
   res_s %>% View()
   res_s %>% filter(mxH != -Inf) %>% ungroup() %>% summarise(sm = sum(platr), n=n())



  # filter on pLS
   res_rd2 <- res_rd %>% mutate(pC = lag(Close),pOC = lag(OC)) %>%
     filter(Date >= "2023-06-01") %>%
     add_h5()  %>% add_l5() %>% add_C5()

   res_rd3 <- res_rd2 %>% filter(pLS=="L") #do last ...

   # Add OH/OL
   res_rd3 <- res_rd2 %>% mutate(wd = lubridate::wday(Date),
                                 OH = High-Open, maOH = TTR::runMean(OH,n = 3),
                                 OL = Open-Low, maOL = TTR::runMean(OL,n = 3),
                                 OHDiff = maOH-maOL, pOHDiff = lag(OHDiff))
   # Add pl
   rsl <- res_rd3 %>% mutate(plO = High-Open,plOatr = plO/atr,
                             plO1 = H1-Open, plOatr1 = plO1/atr,
                             plOmx = if_else(plOatr>plOatr1,plOatr,plOatr1),
                             plC = High-pC,plCatr = plC/atr,
                             plC1 = H1-pC, plCatr1 = plC1/atr)
   # filter
   rsl2 <- rsl %>% mutate(atrper = (atr/Open)*100) %>%
     filter(Date=="2024-02-09", pC_ma5>0, pC_C5>0, atrper>=0, atr >= 0, pOHDiff>0, pOC>0,pmaLC>0.5, pmaLC<2.1) %>%
     select(pmaLC,pLS,LS,maLO,H1,pC,pOC,1:8,plOmx,plOatr,plOatr1,plCatr,plCatr1,
            OHDiff,rn,ma5,atrper,C_C5,wd,C1) %>%
    adorn_rounding() %>% arrange(Date)

   rsl2 %>% group_by(Date) %>% tally()

   lp <- 1
   for(i in 1:nrow(rsl2)){
    tk <- rsl2$Tick[i]
    dt <- rsl2$Date[1] # "2024-02-05"
    if(rsl2$Low[i] < (rsl2$Open[i] - (rsl2$atr[i]/2))){
      print(lp)
      print( (rsl2$Open[i] - (rsl2$atr[i]/2)) )
      lp <- lp+1
      res_rd %>% filter(Tick==tk, Date >= dt) %>% head(n=5) %>% select(1,pC,2:8,LS,ma5,pmaLC,maLC) %>% print()
    }
   }

   res_rd %>% filter(Tick==tk, Date <= dt) %>% tail(n=100) %>%
     candleStick_plot()
   res_rd %>% filter(Tick==tk, Date >= dt) %>% head(n=10) %>%
     select(1:8,maLC,LS) #%>% View()



   rsl2 %>% group_by() %>% summarise(av = mean(plOmx, na.rm = T), n= n())
   rsl2 %>% filter(pmaLC>2) %>% group_by() %>% summarise(av = mean(plOmx, na.rm = T), n= n())
   rsl2 %>% group_by(Tick) %>% summarise(av = mean(plOatr), n= n()) %>% adorn_rounding() %>% View()
   rsl2 %>% group_by(wd) %>% summarise(av = mean(plOatr), n= n()) %>% adorn_rounding() %>% View()

   rsl2 %>% filter(platr >= 0.5) %>% count()

   # move from ma25 ...
   mv25 <- res_rd %>% mutate(pC = lag(Close),pOC = lag(OC)) %>%
     filter(Date >= "2023-06-01") %>%
     add_h5()  %>% add_l5() %>% add_C5()

   mv_res <- mv25 %>% filter(pmaLC>3, pLS== "L")
   mv_res <- mv_res %>% filter(High >= pC+(atr/2))
   mv_res <- mv_res %>%
     mutate(p1 = (pC+(atr/2)-L1)/atr,
            p2 = (pC+(atr/2)-L2)/atr,
            p3 = (pC+(atr/2)-L3)/atr,
            pmx = if_else(p1>p2 & p1>p3,p1,p3),
            pmx = if_else(p2>p1 & p2>p3,p2,pmx)
             ) %>%
     select(1:8,pC,pmaLC,pmx,p1,p2,p3) %>% adorn_rounding()

   mv_res <- mv_res %>% mutate(ct = cut(pmx, breaks = c(-10,0.3,1,2,100), labels = c("l","d","w","w2") ) )

   mv_res %>% ungroup() %>% group_by() %>% mutate(nall = n()) %>%
     group_by(ct) %>% summarise(avg = mean(pmx), n = n(), nall = max(nall)) %>% ungroup() %>%
     adorn_rounding()

   mv_res %>% group_by(pmaLC) %>% summarise(avg = mean(p1), n = n()) %>%
     adorn_rounding() %>% View()


# runMax ------------------------------------------------------------------

   ast::load_libs()
   ast::update_ftse_tick_saved()
   read.csv("/home/allanx38/ast/Data/FTSE_Data/HL.L.csv") %>% tail(n=10)

   ftse100 <- run_m_25_150_fnc(tail_num = 500) %>% filter(Tick!="EVR.L") %>%
     mutate(m5 = Close-ma5)

   # Today
   rday <- "2024-02-22"
   # res_rdmx %>% filter(Date >= rday)
   # Long
   ftse100 %>% filter(mxH5C<=5, Date >= rday, LS == "L", atr>10, Open>ma25) %>%
     select(1:8,LS,rn,ShtClosePr,mxH5C) %>% arrange(mxH5C)
   #,mxH5,mxH5C,ma5,ma25,ma150) %>% arrange(mxH5C)
   # Short
   ftse100 %>% filter(mnL5C<=5, Date >= rday, LS == "S") %>%
     select(1:8,LS,rn,LngClosePr,mnL5C) %>% arrange(mnL5C)
   #mnL5C,ma5,ma25,ma150 %>% arrange(mnL5C)



  # Back Testing PB


   # rev from long

   btest_rev_from_long_to_short <- function(p_hl, atr_frac = 0.5){

     mxs <- p_hl %>% filter_rev_psht()
     mxs <- mxs %>% mutate(OpenP = Open + (atr * atr_frac))
     # mxs$OpenP <- mxs$OHPr50
     mxs5 <- mxs %>% filter(High>OpenP)

     mxs5_r <- mxs5 %>%
       # filter(pO_ma25a >= 4) %>%
       mutate(C0p = (OpenP-Close)/atr,
              L1p = (OpenP-L1)/atr, H1p = (OpenP-H1)/atr, C1p = (OpenP-C1)/atr,
              L2p = (OpenP-L2)/atr, H2p = (OpenP-H2)/atr, C2p = (OpenP-C2)/atr,
              L3p = (OpenP-L3)/atr, H3p = (OpenP-H3)/atr, C3p = (OpenP-C3)/atr,
              L4p = (OpenP-L4)/atr, H4p = (OpenP-H4)/atr, C4p = (OpenP-C4)/atr ) %>%
       select(pLS,1:8,OH,prmxH5Ca,prmxH5C,pO_ma25a,ends_with("p"),starts_with("L"))

     mxs5_r <- inner_join(mxs5_r, mxs5_r %>% group_by(Tick,Date) %>% summarise(mxpl = max(L1p,L2p,L3p,L4p)),by = join_by(Date, Tick))
     mxs5_r <- inner_join(mxs5_r, mxs5_r %>% group_by(Tick,Date) %>% summarise(mnpl = min(H1p,H2p,H3p,H4p)),by = join_by(Date, Tick))
     mxs5_r <- inner_join(mxs5_r, mxs5_r %>% group_by(Tick,Date) %>% summarise(mnL = min(L1,L2,L3,L4)),by = join_by(Date, Tick))

     # mxs5_r %>% select(pLS,1:14,mxpl,mnpl,everything()) %>% arrange(Date) %>% adorn_rounding()

     return(mxs5_r)
   }

   # ln <- length(mxs5_r)
   # mxs5_r %>% arrange(Date) %>%
   #   adorn_rounding(digits = 0,, c(1:8,prmxH5C,OHPr50)) %>%
   #   adorn_rounding(digits = 1,, c(9,13:ln)) %>%
   #   adorn_rounding(digits = 2,, prmxH5Ca) %>%
   #   #filter(L1p <= -0.8) %>%
   #   View()

   # rev from short
   btest_rev_from_short_to_long <- function(p_hl, atr_frac = 0.5){

     mxl5_res <- NULL
     mxl <- p_hl %>% filter_rev_plng()
     mxl <- mxl %>% mutate(OpenP = Open - (atr * atr_frac))
     #mxl$OpenP <- mxl$OLPr75

     if(nrow( mxl %>% filter(Low<OpenP) ) > 0){

       mxl5 <- mxl %>% filter(Low<OpenP)
       mxl5_res <- mxl5 %>%
         #filter(pO_ma25a <= -2) %>%
         mutate(C0Lp = (Close-OpenP)/atr,
                H1p = (H1-OpenP)/atr, L1p = (L1-OpenP)/atr, C1Lp = (C1-OpenP)/atr,
                H2p = (H2-OpenP)/atr, L2p = (L2-OpenP)/atr, C2Lp = (C2-OpenP)/atr,
                H3p = (H3-OpenP)/atr, L3p = (L3-OpenP)/atr, C3Lp = (C3-OpenP)/atr,
                H4p = (H4-OpenP)/atr, L4p = (L4-OpenP)/atr, C4Lp = (C4-OpenP)/atr,) %>%
         mutate(pMv = ptr/atr) %>%
         select(1:6,pMv,atr,pLS,pma25,pO_ma25a,OpenP,L1,H1,ends_with("p"))

       mxl5_res <- inner_join(mxl5_res, mxl5_res %>% group_by(Tick,Date) %>% summarise(mnpl = min(L1p,L2p,L3p,L4p)),by = join_by(Date, Tick))
       mxl5_res <- inner_join(mxl5_res, mxl5_res %>% group_by(Tick,Date) %>% summarise(mxpl = max(H1p,H2p,H3p,H4p)),by = join_by(Date, Tick))

       # mxl5_res %>% select(pLS,1:12,mxpl,mnpl,everything()) %>% adorn_rounding() %>% arrange(Date)
     }
     return(mxl5_res)
   }


   # FTSE 100
     ftse100 <- run_m_25_150_fnc(tail_num = 500)
     ftse100_hl <- ftse100 %>% filter(Date >= "2023-01-01", Date <= "2024-03-01", Tick!="EVR.L") %>%
       add_h5() %>% add_l5() %>% add_C5()

    # Long to Short
     ftse_rev_sht <- btest_rev_from_long_to_short(ftse100_hl, atr_frac = 0.5)
     fvs <- ftse_rev_sht %>%
       # select(pLS,1:12,mxpl,mnpl,everything()) %>%
       select(pLS,1:9,mxpl,C0p,OpenP,ends_with("p")) %>%
       adorn_rounding() %>%
       #select(pLS,1:7,atr,mxpl,mnpl) %>% adorn_rounding() %>%
       #filter(Date == "2024-02-26") %>%
       #filter(C0p >= 0) %>%
       # mutate(pl = (Close-mnL)/atr) %>% adorn_rounding() %>%
       arrange(Date) %>%
       mutate(cpl = mxpl-C0p) %>% filter(C0p>=0.5)
     fvs %>% View()

     fvs %>% filter(between(C0p,0.5,2.5)) %>% group_by() %>% summarise(mn = mean(mxpl, na.rm = T), n=n()) %>% adorn_rounding()


     yt <- ftse100 %>% filter(Date=="2024-02-28") %>% filter_rev_sht() %>% adorn_rounding() %>% select(1:10)
     ftse100_hl %>% filter(Date=="2024-02-29", Tick %in% yt$Tick, High > (Open+(atr*0.5)) ) %>%
       adorn_rounding() %>% select(1:10)

     ftse100 %>% filter(Tick=="REL.L", Date>="2024-02-26") %>% adorn_rounding() %>% select(1:10)


     ##
     ftse100_hl %>% filter(pLS=="L",         # long
                      prmnL5Ca  <= 0.2, # Close rel to runMax5 high
                      #prmnL20Ca <= 0.5, # Close rel to runMax20
                      Open < pma25,     # open higher than ma25
                      prn >= 0) %>% mutate(H1p = (H1-Open)/atr,
                                           H2p = (H2-Open)/atr,
                                           H3p = (H3-Open)/atr) %>%
       select(pLS,1:6,atr,rn,rn,H1p,H2p,H3p) %>%  adorn_rounding() %>% View()


   # Short to Long
     ftse_rev_long <- btest_rev_from_short_to_long(ftse100_hl, atr_frac = 1)
     ftse_rev_long %>%
       #select(pLS,1:12,mxpl,mnpl,everything()) %>% adorn_rounding() %>%
       select(pLS,1:7,atr,mxpl,mnpl,everything()) %>%
       filter(C0Lp >= 0) %>%
       # filter(Date >= "2024-02-19") %>%
       adorn_rounding() %>% arrange(Date) %>% View()



   # Dow
     ast::load_libs()
     library(stringr)
     library(ggplot2)

     ast::get_dow

     rday <- "2024-03-22"
     tr_res <- Trade_All(dow, rday)

     tr_res$sht_pb

    dow <- run_m_25_150_csv_list( get_dow_ticks_csv_list() )
    dow_hl <- dow %>% filter(Date >= "2023-01-01", Date <= "2024-03-31") %>%
      add_h5() %>% add_l5() %>% add_C5()

    dow %>% select(1:8, ma150)

    # Long to Short
    dow_rev_sht <- btest_rev_from_long_to_short(dow_hl, atr_frac = 0.99)
    dow_rev_sht %>% select(pLS,1:12,mxpl,mnpl,everything()) %>% adorn_rounding() %>% View()
    # Short to Long
    dow_rev_long <- btest_rev_from_short_to_long(dow_hl, atr_frac = 0.5)
    dow_rev_long %>% select(pLS,1:12,mxpl,mnpl,everything()) %>% adorn_rounding() %>% View()

  # Dax
   dax <- run_m_25_150_csv_list( get_dax_ticks_csv_list())
   dax_hl <- dax %>% filter(Date >= "2023-01-01", Date <= "2024-03-31") %>%
     add_h5() %>% add_l5() %>% add_C5()

   # Long to Short
   dax_rev_sht <- btest_rev_from_long_to_short(dax_hl, atr_frac = 0.75)
   dax_rev_sht %>% select(pLS,1:12,mxpl,mnpl,everything()) %>% adorn_rounding() %>% arrange(Date) %>%  View()
   # Short to Long
   dax_rev_lng <- btest_rev_from_short_to_long(dax_hl, atr_frac = 0.75)
   dax_rev_lng %>% select(pLS,1:12,mxpl,mnpl,everything()) %>% adorn_rounding() %>% arrange(Date) %>%  View()






   # DAX

   update_dax_tick_saved(d_back = 200)

   #debugonce(run_m_25_150_fnc_dax)
   dax_rd <- run_m_25_150_fnc_dax(tail_num = 500)
   max(dax_rd$Date)
   min(dax_rd$Date)

   hldx <- dax_rd %>%
     filter(Date >= "2023-01-01", Date <= "2023-12-31") %>%
     add_h5() %>% add_l5() # %>% adorn_rounding()

   hldx <- hldx %>% mutate(pbpl  = (mxH-OLPr50)/atr,
                       rvpl  = (OHPr50-mnL)/atr,  # reversing from long
                       rvplO = (Open-mnL)  /atr,
                       rvps  = (mxH-OLPr50)/atr   # reversing from short
   ) %>% adorn_rounding()

   # rev from long
   mxsdx <- hldx %>% filter(pLS=="L",         # long
                        prmxH5C<=5,        # Close rel to runMax5 high
                        prmxH20Ca<=0.5,     # Close rel to runMax20
                        Open>pma25,       # open higher than ma25
                        prn >= 0)

   mxsdx5 <- mxsdx %>% filter(High>OHPr50)

   mxsdx5_r <- mxsdx5 %>%
     filter(pO_ma25 >= 4) %>%
     mutate(L1p = (OHPr50-L1)/atr, L2p = (OHPr50-L2)/atr, L3p = (OHPr50-L3)/atr, L4p = (OHPr50-L4)/atr) %>%
     select(1:8,rvpl,OHPr50,L1p,L2p,L3p,L4p,pO_ma25,pC_ma25,pC_C5) %>% adorn_rounding()

   mxsdx5_r %>%
     #filter(L1p <= -0.8) %>%
     View()

   mxsdx5_r %>% filter(rvpl != "-Inf",
                     pO_ma25 >= 4
                     # atr >= 0,
                     # pC_C5 <= 1
                     # pC_ma25
   ) %>%
     group_by() %>% summarise(mn = mean(rvpl), n=n())







   # Back Testing # mx/mn pull back
   min(res_rdmx$Date)
   res_rdmx_hl <- res_rdmx %>% filter(Date>="2023-01-01") %>% add_h5() %>% add_l5()

   # Add opening price levels
   res_rdmx_hl <- res_rdmx_hl %>% mutate(ShtOpenPr = Open+(atr*0.75),
                                         LngOpenPr = Open-(atr*0.75))

   ### short trades, pmxH5C = 0
   res_rdmx_sht <- res_rdmx_hl %>% filter(pmxH5C<=5, pmxH20C<=0.5, atr>=5, Open>ma25, pLS=="L", prn >= 0)
   res_rdmx_sht %>% filter(High>ShtOpenPr) %>% count()

   # short ShtOpenPr as well as pmH5 = 0
   res_rdmx_sht_res <- res_rdmx_sht %>%
     filter(Date >= "2024-01-01") %>%
     filter(High>ShtOpenPr) %>%
     # Add pl
     mutate(L0p = (ShtOpenPr-Close)/atr,
            L1p = (ShtOpenPr-L1)/atr,
            L2p = (ShtOpenPr-L2)/atr,
            L3p = (ShtOpenPr-L3)/atr,
            C1p = (Close - L1)/atr,
            C2p = (Close - L2)/atr,
            C3p = (Close - L3)/atr) %>%
     mutate(lpl = if_else(L1p>L2p,L1p,L2p)) %>%
     mutate(lpl = if_else(L3p>lpl,L3p,lpl)) %>%
     mutate(cpl = if_else(C1p>C2p,C1p,C2p)) %>%
     mutate(cpl = if_else(C3p>cpl,C3p,cpl)) %>%
     select(Tick,Date,pLS,prn,atr,Open,High,Low,Close,pmxH20C,pmxH5C,ShtOpenPr,lpl,L0p,L1p,L2p,L3p,cpl,C3p,ma5,ma25,ma150) %>%
     adorn_rounding() %>% arrange(Date)

   res_rdmx_sht_res %>% View()



   # summarise
   res_rdmx_sht_res %>%
     filter(pmxH20C>=1.5) %>%
     group_by() %>%
     summarise(mn = mean(lpl, na.rm = T), mnC = mean(cpl, na.rm = T), n=n())



   ### long trades, pmnL5C = 0
   res_rdmx_lng <- res_rdmx_hl %>% filter(pmnL5C<=5, atr>=5, Open<ma25, pLS=="S", prn >= 0)

   # lng LngOpenPr as well as pmH5 = 0
   res_rdmx_lng_res <- res_rdmx_lng %>%
     filter(Date>="2024-01-01") %>%
     filter(Low<LngOpenPr) %>%
     mutate(H1p = (H1-LngOpenPr)/atr,
            H2p = (H2-LngOpenPr)/atr,
            H3p = (H3-LngOpenPr)/atr) %>%
     mutate(spl = if_else(H1p>H2p,H1p,H2p)) %>%
     mutate(spl = if_else(H3p>spl,H3p,spl)) %>%
     select(Tick,Date,pLS,atr,Open,Low,pmnL5C,pmnL20C,ShtOpenPr,spl,H1p,H2p,H3p,ma5,ma25,ma150) %>%
     adorn_rounding() %>% arrange(Date)

   res_rdmx_lng_res %>% View()

  res_rd %>% filter(Tick=="BRBY.L", Date >= "2023-12-26") %>% head(n=10)

  poss <- bind_rows(res_rdmx_sht %>% filter(Date >= "2024-01-01") %>% select(Date,Tick),
            res_rdmx_lng %>% filter(Date >= "2024-01-01") %>% select(Date,Tick))

  tr <- bind_rows(res_rdmx_sht_res %>% filter(Date >= "2024-01-01") %>% select(Date,Tick),
                    res_rdmx_lng_res %>% filter(Date >= "2024-01-01") %>% select(Date,Tick))

  left_join(poss %>% group_by(Date) %>% tally(),
            tr %>% group_by(Date) %>% tally() %>% rename(t = n) ) %>% View()





  # check up Lng - PB Func
    # v1
    res_rd_lng <- res_rd %>% mutate(OL = (Open-Low)/atr, OLPr = Open - (atr/2)) %>% adorn_rounding()
    res_rd_lng <- res_rd_lng %>% add_h5() %>% add_l5()
    # v2
    res_rd_lng <- res_rd %>% mutate(pC = lag(Close), OL = (pC-Low)/atr, OLPr = pC - (atr/2)) %>% adorn_rounding()
    res_rd_lng <- res_rd_lng %>% add_h5() %>% add_l5()
    #unique(res_rd_lng$Date)
    #head(res_rd_lng) %>% View()

    # Function
    rday <- res_rd_lng %>% filter(LS == "L",
                                  !is.na(pC),
                                  #Date >= "2023-04-01",
                                  Date <= "2023-12-28",
                                  maLO <= 1.5, OL >= 0.5)
    res <- pb_lng_res(rday) %>% select(atr,pl,everything())
    res %>% select(atr,pl,OLPr,mxH,mxpl,mxpldy,mnpl,mnpldy, everything()) %>% View()
    res <- res %>% arrange(Date) %>% filter(pl != 0) %>%
      mutate(pl2 = if_else(pl > 0,100,-100),
             win = if_else(pl>0,1,0),
             mth = lubridate::month(Date),
             yr = lubridate::year(Date))
    res %>% select(yr,mth) %>% distinct() %>% arrange(yr,mth) %>% View()
    #res %>% select(1:8,pl2,win,mth) %>% View()
    res %>% filter(yr == 2023) %>% group_by() %>% summarise(sm = sum(pl2))
    res %>% group_by(yr,mth) %>% summarise(sm = sum(pl2)) %>% View()
    res %>% group_by(mth,win) %>% tally() %>% pivot_wider(names_from = win, values_from = n) %>%
      mutate(WinP = `1`/(`1`+`0`)*100) %>% View()
    res %>% mutate(cm = cumsum(pl2)) %>% View()

    sum(res$pl)
    res %>% filter(pl>0) %>% count() / nrow(res) * 100
    res %>% filter(pl<0) %>% count()

    dt <- as.Date("2023-07-05")
    dt2 <- as.Date("2023-07-06")
  # check what happened
    tt <- res_rd_lng %>% filter(LS == "L", Date == dt, maLO <= 1.5) %>% arrange(maLO)
    ttol <- res_rd_lng  %>% filter(Tick %in% tt$Tick,Date == dt2, OL >= 0.5)
    res_rd_lng %>% filter(Tick %in% ttol$Tick,Date >= dt2,Date <= dt2+5) %>%
      select(Tick,Date,atr,OLPr,High,everything()) %>% View()






    # check Long results
    # no PB, looking at long trend, start rn = 3 ...
    min(res_rd$Date)
    max(res_rd$Date)
    ck <- res_rd %>% filter(LS=="L",rn>=3, between(maLO,1,3)) %>%
      mutate(OL = (Open-Low)/atr, OLPr = Open - (atr/2),
             OH = (High-Open)/atr, OHPr = Open + (atr/2)) %>%
      adorn_rounding() %>%
      group_by(Tick,trn) %>%
      summarise(Tick = first(Tick),Date = first(Date), atr = first(atr),
                Open = first(Open),
                High = max(High),
                Low = min(Low),
                OH = High-Open,
                mxpl = (High-Open)/atr,
                n = n()) %>% adorn_rounding()
    #ck %>% View()
    patr = 3
    ck %>% mutate(ct = cut(mxpl, breaks = c(-1,0.5,patr,100), labels = c("loss", "even", "win"))) %>%
      group_by(ct) %>% tally() %>% mutate(n2 = n) %>% adorn_percentages(denominator = "col", ,"n2") %>%
      adorn_pct_formatting(,,,"n2") %>% group_by() %>%
      mutate(sm = sum(n), pr = if_else(ct=="loss",-1,if_else(ct=="even",0.5,patr)),
             pl = n*pr, tpl = sum(pl),per = (tpl/sm)*100, p_mth = sm/30, pl_mth = tpl/30)


# Signals
t_day <- "2023-12-22"
rr_today %>% filter(Date==t_day, sig_H == 1, pLS == "L",prn <= 7) %>%
  select(1:6,atr,MaLS,rn,pLS,prn,ma25,ma150,sig_H) %>% arrange(prn)
rr_today %>% filter(Date==t_day, sig_L == 1, pLS == "S",prn <= 7) %>%
  select(1:6,atr,MaLS,rn,pLS,prn,ma25,ma150,sig_L) %>% arrange(prn)

candleStick_plot_ftse <- function(tick, tail_nm = 75){

  pData <- read.csv(paste0("/home/allanx38/ast/Data/FTSE_Data/",tick,".csv"))
  pData <- pData %>% add_ma25() %>% add_ma150()
  pData <- pData %>% mutate(greenRed=ifelse(Open-Close>0,"Red","Green"))
  pData$Date <- as.Date(pData$Date)
  pData <- pData %>% tail(tail_nm)

  ggplot(pData) +
    geom_segment(aes(x = Date,
                     xend = Date,
                     y = Open,
                     yend = Close,
                     colour = greenRed),
                 size=3)+
    #theme_tq()+
    geom_segment(aes(x = Date,
                     xend = Date,
                     y = High,
                     yend = Low,
                     colour=greenRed)) +

    geom_line(aes(x=Date, y = ma25), color="orange", linetype="longdash", group = 1) +

    scale_color_manual(values=c("Forest Green","Red")) +
    scale_x_date(date_breaks = "1 month", date_labels = "%y%m") +
    ggtitle(tick) +
    theme(legend.position ="none",
          axis.title.y = element_blank(),
          axis.title.x=element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
          plot.title= element_text(hjust=0.5))
}

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

    scale_color_manual(values=c("Forest Green","Red")) +
    scale_x_date(date_breaks = "1 month", date_labels = "%y%m") +
    ggtitle(pData$Tick[1]) +
    theme(legend.position ="none",
          axis.title.y = element_blank(),
          axis.title.x=element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
          plot.title= element_text(hjust=0.5))
}



candleStick_plot_ftse("WTB.L")

pd <- read.csv("/home/allanx38/ast/Data/FTSE_Data/WTB.L.csv")
pd <- pd %>% add_ma25() %>% add_ma150()
pd$Date <- as.Date(pd$Date)
pd <- pd %>% tail(75)
gp <- candleStick_plot(pd)
gp



# pLS/pMA_LS
rr_today %>% filter(Date==t_day, pMaLS == "L", pLS == "L",prn <= 7) %>%
  select(1:6,atr,sig_H,sig_L,prn,MaLS,cLS,pLS,ma25LS,ma25,ma150) %>% arrange(prn)
rr_today %>% filter(Date==t_day, pMaLS == "S", pLS == "S",prn <= 7) %>%
  select(1:6,atr,sig_H,sig_L,prn,MaLS,cLS,pLS,ma25LS,ma25,ma150) %>% arrange(prn)

rr_today %>% filter(Tick=="BRBY.L") %>%
  select(1:6,atr,sig_H,sig_L,cLS,MaLS,rn,pLS,prn,ma25,ma150) #%>% tail(n=9)

rr_today %>% filter(Tick==tlist$Tick[24]) %>%
  mutate(OL = (Open-Low)/atr,OH = (High-Close)/atr)%>%
  select(1:6,atr,sig_H,MaLS,rn,pLS,prn,ma25,ma150,OL,OH) %>% adorn_rounding()

rr_today %>% mutate(OL = (Open-Low)/atr, pClose = lag(Close)) %>%
  #filter(pClose > ma25, OL>= 0.5) %>%
  select(1:6,atr,pClose,OL,pLS,ma25,ma150) %>% adorn_rounding() %>% View()

rr_today %>% mutate(OL = (Open-Low)/atr, pClose = lag(Close)) %>%
  filter(pLS=="L", OL>= 0.5) %>% count()

rr_today %>% mutate(OL = (Open-Low)/atr, pClose = lag(Close)) %>%
  filter(Date=="2023-12-06", OL >= 0.5, pLS=="L") %>%
  select(1:6,atr,pClose,OL,pLS,ma25,ma150) %>% adorn_rounding() %>% View()

rr_today %>% filter(Date>="2023-12-04",Tick=="PHNX.L") %>% select(1:6,atr)
rr_today %>% mutate(OL = (Open-Low)/atr, pClose = lag(Close)) %>%
  filter(Date=="2023-12-04",pLS=="L") %>% slice(4,14,24,34,44,54) %>% select(1:6,atr,OL) %>%
  adorn_rounding()

rr_today %>% filter(Date>="2023-12-06",Tick=="AAL.L") %>% select(Tick,Close,atr) %>%
  mutate(Per = (atr/Close)*100) %>% adorn_rounding() %>% arrange(desc(Per))

# OH / OL check

rr1 <- rr_today %>% add_h5() %>% add_l5()

rr2 <- rr1 %>%
  mutate(OL = (Open-Low)/atr, OH = (High-Open)/atr)%>%
  filter(OL >= 0.5) %>% mutate(plH = (mxH-Open-(0.5*atr))/atr ) %>%
  mutate(wl = if_else(plH>0,"w","l")) %>%
  select(1:6,atr,rn,pLS,MaLS,prn,ma25,ma150,OL,OH,mxH,mnL,plH,wl) %>% adorn_rounding() %>%
  filter(plH != "-Inf", pLS=="L",MaLS=="L")

# prn>=2,prn<=7
rr2 %>% filter(wl=="l") %>% View()

rr2 %>% group_by(wl) %>% summarise(plh = sum(plH, na.rm = T), n = n()) %>% mutate(plh/n)
rr2 %>% group_by() %>% summarise(plh = sum(plH, na.rm = T), n = n()) %>% mutate(plh/n)
nrow(rr2[rr2$wl=="w",])/nrow(rr2) * 100
rr2 %>% group_by(prn) %>% summarise(plh = sum(plH, na.rm = T), n = n()) %>% mutate(avg = plh/n) %>%
  View()


ll2 <- rr1 %>%
  mutate(OL = (Open-Low)/atr, OH = (High-Open)/atr)%>%
  filter(OH >= 0.5) %>% mutate(plL = (Open+(0.5*atr)-mnL)/atr ) %>%
  mutate(wl = if_else(plL>0,"w","l")) %>%
  select(1:6,atr,rn,pLS,MaLS,prn,ma25,ma150,OL,OH,mxH,mnL,plL,wl) %>% adorn_rounding() %>%
  filter(plL != "-Inf", pLS=="S",MaLS=="S")

ll2 %>% select(1:6,atr,rn,pLS,MaLS,prn,ma25,ma150,OL,OH,OH1,mxH,mnL) %>% View()

ll2 %>% group_by(wl) %>% summarise(pll = sum(plL, na.rm = T), n = n()) %>% mutate(pll/n)
ll2 %>% group_by() %>% summarise(pll = sum(plL, na.rm = T), n = n()) %>% mutate(pll/n)
nrow(ll2[ll2$wl=="w",])/nrow(ll2) * 100
ll2 %>% group_by(prn) %>% summarise(pll = sum(plL, na.rm = T), n = n()) %>% mutate(avg = pll/n) %>%
  View()


# Signals ... sig_H / sig_L
# Long - no pull back ...
rr2 <- rr1 %>%
  mutate(OL = (Open-Low)/atr, OH = (High-Open)/atr, psig_H = lag(sig_H))%>%
  filter(psig_H == 1) %>% mutate(plH = (mxH-Open-(0.5*atr))/atr ) %>%
  mutate(wl = if_else(plH>0,"w","l")) %>%
  select(1:6,atr,rn,pLS,MaLS,prn,ma25,ma150,OL,OH,mxH,mnL,plH,wl,psig_H) %>% adorn_rounding() %>%
  filter(plH != "-Inf", pLS=="L",MaLS=="L")

rr2 %>% group_by(wl) %>% summarise(plh = sum(plH, na.rm = T), n = n()) %>% mutate(plh/n)
rr2 %>% group_by() %>% summarise(plh = sum(plH, na.rm = T), n = n()) %>% mutate(plh/n)
nrow(rr2[rr2$wl=="w",])/nrow(rr2) * 100
rr2 %>% group_by(prn) %>% summarise(plh = sum(plH, na.rm = T), n = n()) %>% mutate(avg = plh/n) %>%
  View()

# Long - with pull back ...
rr2pb <- rr1 %>%
  mutate(OL = (Open-Low)/atr, OH = (High-Open)/atr, psig_H = lag(sig_H))%>%
  filter(psig_H == 1, OL >= 0.5) %>% mutate(plH = (mxH-Open-(0.5*atr))/atr ) %>%
  mutate(wl = if_else(plH>0,"w","l")) %>%
  select(1:6,atr,rn,pLS,MaLS,prn,ma25,ma150,OL,OH,mxH,mnL,plH,wl,psig_H) %>% adorn_rounding() %>%
  filter(plH != "-Inf", pLS=="L",MaLS=="L")

rr2pb %>% group_by(wl) %>% summarise(plh = sum(plH, na.rm = T), n = n()) %>% mutate(plh/n)
rr2pb %>% group_by() %>% summarise(plh = sum(plH, na.rm = T), n = n()) %>% mutate(plh/n)
nrow(rr2pb[rr2pb$wl=="w",])/nrow(rr2pb) * 100


# Check Lng res
tc <- rr_today %>% filter(sig_H == 1,MaLS == "L",cLS=="L",pLS=="L",ma25LS=="L",prn <= 7) %>% select(Tick) %>% distinct()

rr_today %>% filter(Tick == tc$Tick[15]) %>% select(1:6,atr,sig_H,MaLS,rn,pLS,prn,ma25,ma150) %>%
  mutate(OL = (Open-Low)/atr) %>% adorn_rounding()


# Check Sht res
ts <- rr_today %>% filter(sig_L == 1,MaLS == "S",cLS=="S",pLS=="S",ma25LS=="S",prn <= 7) %>%
  select(Tick) %>% distinct()

rr_today %>% filter(Tick == ts$Tick[15]) %>%
  select(1:6,atr,sig_L,MaLS,rn,pLS,prn,ma25,ma150,pHigh,pLow) %>%
  mutate(OL = (Open-Low)/atr) %>% adorn_rounding()

tck_d <- rr_today %>% filter(sig_L == 1) %>% select(Tick,Date) %>% distinct()


# step thru each row ...

run_l <- function(pdata, FUN){
  res_all <- NULL
  for(i in 1:nrow(tck_d)){
    r <- FUN(pdata,i)
    res_all <- bind_rows(res_all,r)
  }
  return(res_all)
}

lp_pl <- function(pdata, td_nm){
  loop <- pdata %>% filter(Tick == tck_d$Tick[td_nm],Date >= tck_d$Date[td_nm])
  r_lp <- NULL
  if(nrow(loop)>2){
    pl <- 0
    pTarg <- loop$Close[1] + loop$atr[1]
    sLoss <- loop$Close[1] - loop$atr[1]
    OpenP = loop$Close[1]
    type <- ""
    #browser()
    for (i in 2:nrow(loop)) {
      currH  = loop$High[i]
      currL  = loop$Low[i]
      currpl = currH-OpenP
      if(currH >= pTarg){
        pl <- pTarg - OpenP
        type <- "pTarg"
        break
      }
      if(currL <= sLoss){
        pl <- sLoss - OpenP
        type <- "sLoss"
        break
      }
    }
    r_lp <- data.frame(Tick = loop$Tick[1],
                       Atr = loop$atr[1],
                       OpenD = loop$Date[1],
                       PL = pl,
                       Type = type)
  }
  return(r_lp)
}

# open on pull back
lp_pb_pl <- function(pdata, td_nm){
  loop <- pdata %>% filter(Tick == tck_d$Tick[td_nm],Date >= tck_d$Date[td_nm])
  r_lp <- NULL
  if(nrow(loop)>2){
    pl <- 0
    atr <- loop$atr[1]
    pTarg <- loop$Close[1] + (atr * 0.5)
    sLoss <- loop$Close[1] - (atr * 1.5)
    OpenP <- loop$Close[1] - (atr * 0.5)
    type <- ""
    bTrade <- FALSE
    if(loop$Low[2] < OpenP){
      #browser()
      for (i in 2:nrow(loop)) {
        currH  = loop$High[i]
        currL  = loop$Low[i]
        if(currL <= OpenP){
          bTrade <- TRUE
        }
        if(bTrade == TRUE){
          currpl = currH-OpenP
          if(currH >= pTarg){
            pl <- pTarg - OpenP
            type <- "pTarg"
            break
          }
          if(currL <= sLoss){
            pl <- sLoss - OpenP
            type <- "sLoss"
            break
          }
        }
      }
    }
    r_lp <- data.frame(Tick = loop$Tick[1],
                       Atr = loop$atr[1],
                       OpenD = loop$Date[1],
                       PL = pl,
                       Type = type)
  }
  return(r_lp)
}





today2 <- rr_today %>% add_h5() %>% mutate(mxPL = mxH-Open)
today2 <- today2 %>% mutate(OL = (Open-Low)/atr,
                            nxtOL = lead(OL)) %>% adorn_rounding()
today2 %>% filter(sig_H==1,nxtOL >= 0.5) %>% select(1:8,nxtOL,mxPL,mxH,starts_with("H")) %>% View()

tod_res <- run_l(rr_today %>% filter(Date <= "2023-12-05"),lp_pb_pl)
tod_res2 <- run_l(rr_today %>% filter(Date <= "2023-12-05"),lp_pl)

tdr <- tod_res %>% group_by(Type) %>% summarise(sm = sum(PL), n = n(), at = sm/n)
tdr[tdr$Type=="pTarg","n"]/41
806/41
tdr2 <- tod_res2 %>% group_by(Type) %>% summarise(sm = sum(PL), n = n(), at = sm/n)
tdr2[tdr2$Type=="pTarg","n"]/199
(5843-3423)/199
tod_res %>% group_by(Tick,Type) %>% summarise(sm = sum(PL), n = n(), at = sm/n) %>% View()


# how far below mas?
# all the rn #s


res %>% select(1:8, contains("25")) %>% View()

rr %>% filter(Tick == tlist$Tick[14]) %>% select(1:6,atr,sig_H,sig_L,MaLS,rn,pLS,prn,ma25,pHigh,pLow) %>%
  tail(n=500)

today <- rr %>% filter(Date == "2023-11-20")
tt <- today %>% filter(ma25LS == "L") %>% arrange(rn25) %>% select(1:6,atr,rn25,cLS)
today %>% filter(pLS == "S",MaLS=="S") %>% arrange(prn) %>% select(1:6,atr,MaLS,rn,pLS,prn) %>% View()
t_name <- "STAN.L"
gg_ma_vl(rr %>% filter(Tick == t_name) %>% tail(150),
         t_txt = t_name, v_line_date = "2023-11-22")
rr %>% filter(Date >= "2023-11-14",Tick=="INF.L") %>%
  select(1:6,atr,MaLS,rn,pLS,prn,ma25) #%>% View()

res <- rr %>% filter(Tick == tlist$Tick[11]) %>% mutate(pHigh = lag(High),
                                        sig = if_else(Close>pHigh,1,0),
                                        H1 = lead(High,n=1),
                                        H2 = lead(High,n=2),
                                        H3 = lead(High,n=3),
                                        H4 = lead(High,n=4),
                                        H5 = lead(High,n=5)) %>%
  group_by(Date) %>% mutate(mxH = max(H1,H2,H3,H4,H5)) %>% ungroup() %>%
  mutate(pl = mxH - Close)

res %>% filter(sig==1,ma25LS=="L",pLS=="L") %>% select(1:6,atr,rn25,ma25LS,pLS,starts_with("H"),mxH,pl) %>%
  group_by(rn25) %>% summarise(rn = sum(pl), n = n(), Avg = rn/n) %>%
  View()

res %>% filter(sig == 1,pLS=="L", between(rn25,3,7)) %>%
  select(1:6,atr,sig,rn25,ma25LS,pLS,starts_with("H"),mxH,pl) %>% arrange(pl) %>% View()

res %>% filter(Date >= "2023-04-24") %>% select(1:6,atr,sig,rn25,ma25LS,pLS,starts_with("H"),mxH,pl)

res %>% filter(Tick=="AAL.L", ma25LS=="L", trn25 == resl$trn25[68]) %>%
  select(1:6,atr,MaLS,rn25,pLS,prn,ma25,sig)

rr %>% mutate(lh = lead(Low, n=3)) %>% filter(MaLS == "S", rn == 24, pLS == "S", prn == 4 ) %>%
  select(1:8, lh) %>% mutate(pl = Close - lh) %>% View()

rr %>% filter(Date == "2023-06-01", between(crn, 9, 12)) %>% select(Tick)
gg_ma_vl(rr %>% filter(Date >= "2023-05-15", Tick == "NXT.L"),  v_line_date = "2023-06-01")

res <- m_25_fnc(34)
res %>% filter(ppLS == "L") %>% select(1:8,trn,rn,prn,ptrn) %>% View()
sum_trn_fnc(res %>% filter(ppLS == "L", prn >= 4), Tick,ptrn) %>% select(-starts_with("spl")) %>%
  arrange(hlplatr) %>%
  adorn_rounding() %>% View()
res_sm <- sum_trn_fnc(res %>% filter(ma25LS == "L", rn25 >= 3), Tick, trn25) %>% select(-starts_with("spl")) %>%
  arrange(hlplatr) %>%
  adorn_rounding()
tt <- res_sm %>% tail(n=12) %>% select(trn25) %>% pull()
res %>% filter(trn25 %in% tt) %>% filter(rn25==1) %>% select(cLS, trn25)
res %>% filter(trn25 == 105)

gg_ma(res %>% filter( between(row_num, 1100, 1600) ) )
gg_ma(m_25_fnc(43) %>% filter( between(row_num, 1100, 1600) ) )

res <- m_25_fnc(18)
res <- res %>% add_lead_H()

res %>% filter(pLS == "L", CL == "L") %>% mutate(pl = (mxH - Open)/atr) %>%
  #filter(rn) %>%
  group_by(rn) %>% summarise(sm = sum(pl), n = n(), sm/n) %>% adorn_rounding(digits = 2) %>% View()

sm <- res %>% filter(pLS == "L", CL == "L") %>%
  select(-tr,-row_num,-starts_with(c("mH","mL")),-mnL) %>%
  mutate(pl = (mxH - Open)/atr) %>%
  mutate(OL = (Open-Low)/atr ) %>%
  mutate(OLP = Open - (atr),
         OLpl = (mxH - OLP)/atr) %>%
  filter(OL >= 1) %>%
  select(1:5,OLP,OLpl,mxH,everything()) %>% arrange(OLpl) %>%
  adorn_rounding()

inner_join(res,
           sm %>% select(Date,OLpl,OLP),
           by = join_by(Date)) %>%
  select(Tick,1:5,atr,mxH,OLpl,OLP,pLS,CL) %>% arrange(OLpl) %>%
  View()

res_all_fnc(21,26) %>% filter(crn >= 10) %>% group_by(Tick,ctrn) %>%
  summarise(Tick = first(Tick),
            OpenD = first(Date),
            CloseD = last(Date),
            OpenP = first(Open),
            CloseP = last(nxtClose),
            atr = first(atr),
            CL = first(CL),
            mh = max(High),
            #lpl = CloseP-OpenP,
            lplatr = (CloseP-OpenP)/atr,
            lpl_per = ((CloseP-OpenP)/OpenP)*100,
            hlplatr = (mh-OpenP)/atr,
            hlpl_per = (mh-OpenP)/OpenP*100,
            spl = OpenP-CloseP,
            splatr = spl/atr,
            spl_per = (spl/OpenP)*100,
            ml = min(Low),
            n = n()) %>%
  filter(CL == "L") %>%
  select(-starts_with("spl")) %>% adorn_rounding() %>% arrange(lplatr) %>% View()

res %>% filter(Date >= "2018-07-16") %>% head(n=70)



res_all_l <- NULL
for(i in 1 : nrow(tlist)){
  res <- NULL
  res <- m_ta_fnc(i)
  res_all_l <- bind_rows(res_all_l, res )
}

res_set_lng <- res_all_l %>% filter(row_nm >= 5) %>% filter(LDiff > 4)
res_set_lng <- inner_join(res_set_lng, lng_rnk %>% select(Tick,lng_rnk)) %>% adorn_rounding()

res_set_sht <- res_all_l %>% filter(row_nm >= 5) %>% filter(SDiff > 4)
res_set_sht <- inner_join(res_set_sht, sht_rnk %>% select(Tick,sht_rnk)) %>% adorn_rounding()


#' </br>
#'
#' ## Long Summary
#'
#' ### with OH Trend
#'
#+ results = 'asis'

res_set_lng %>% filter(!is.na(LDiff), row_nm == 6, maDiff > 0) %>% arrange(Tick,rn) %>% knitr::kable()

res_set_lng %>% filter(!is.na(LDiff), row_nm == 5, maDiff > 0) %>% arrange(Tick,rn) %>% knitr::kable()



#' </br>
#'
#' ### WITHOUT OH Trend
#'
#+ results = 'asis'

res_set_lng %>% filter(!is.na(LDiff), row_nm == 6, maDiff < 0) %>% arrange(Tick,rn) %>% knitr::kable()

res_set_lng %>% filter(!is.na(LDiff), row_nm == 5, maDiff < 0) %>% arrange(Tick,rn) %>% knitr::kable()


#' </br>
#'
#' ## Short Summary
#'
#' ### with OH Trend
#'
#+ results = 'asis'

res_set_sht %>% filter(!is.na(SDiff),row_nm == 6, maDiff < 0) %>% arrange(Tick,rn) %>% knitr::kable()

res_set_sht %>% filter(!is.na(SDiff),row_nm == 5, maDiff < 0) %>% arrange(Tick,rn) %>% knitr::kable()


#'
#' </br>
#'
#' ### WITHOUT OH Trend
#'
#+ results = 'asis'

res_set_sht %>% filter(!is.na(SDiff),row_nm == 6, maDiff > 0) %>% arrange(Tick,rn) %>% knitr::kable()

res_set_sht %>% filter(!is.na(SDiff),row_nm == 5, maDiff > 0) %>% arrange(Tick,rn) %>% knitr::kable()


#'
#' </br>
#'
#' ## Details
#'
#+ results = 'asis'

res_set_comb <- bind_rows(res_set_lng, res_set_sht)
res_tick <- unique(res_set_comb$Tick)
for(i in seq_along(res_tick)){
  loop_set <- NULL
  loop_set <- res_all_l %>% filter(Tick == res_tick[i]) %>% adorn_rounding()
  print( loop_set %>% knitr::kable() )
  print( gg_ma(res_tick[i], t_txt = res_tick[i]) )

}


1800 448888
331 807 241

