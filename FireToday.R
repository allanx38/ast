

# Update FTSE 100
  ast::load_libs()
  ast::update_ftse_tick_saved()
  read.csv("/home/allanx38/ast/Data/FTSE_Data/HL.L.csv") %>% tail(n=10)

# Current
  ftse100 <- NULL
  ftse100 <- run_m_25_150_fnc(tail_num = 500)



# Today
  rday <- "2024-03-27"
  tr_res <- Trade_All(ftse100, rday)
  tr_res$lng_rev #Trade Long
  tr_res$lng_pb  #Trade Long
  tr_res$sht_rev #Trade Short
  tr_res$sht_pb  #Trade Short

  # Lng Overlap
  tr_res$lng_pb %>% filter(Tick %in% tr_res$sht_rev$Tick) %>% pull(Tick)
  # Sht Overlap
  tr_res$sht_pb %>% filter(Tick %in% tr_res$lng_rev$Tick) %>% pull(Tick)


# Dow
  rday <- "2024-03-22"
  dow <- NULL
  #debugonce(run_m_25_150_csv_list)
  dow <- run_m_25_150_csv_list(get_dow_ticks_csv_list())
  usa <- run_m_25_150_csv_list(get_us_ticks_csv_list())
  tr_res_dw <- Trade_All(dow, rday)
  tr_res_dw$lng_rev #Trade Long
  tr_res_dw$lng_pb  #Trade Long
  tr_res_dw$sht_rev #Trade Short
  tr_res_dw$sht_pb  #Trade Short

  lng_rev <- Trade_Long_Rev_From_Short(dow, rday)

  # Lng Overlap
  tr_res_dw$lng_pb %>% filter(Tick %in% tr_res_dw$sht_rev$Tick) %>% pull(Tick)
  # Sht Overlap
  tr_res_dw$sht_pb %>% filter(Tick %in% tr_res_dw$lng_rev$Tick) %>% pull(Tick)



# Long Reverse ------------------------------------------------------------
# Long trend - Reverse Short
  sht_rev <- NULL
  sht_rev <- ftse100 %>% filter(Date==rday) %>%
    filter_rev_sht()
  sht_rev <- sht_rev %>%
    select_rev_sht() %>%
    mutate(ShtRevOpenPr = Close + (atr*0.75)) %>%
    arrange(desc(O_ma25a))
  sht_rev


# Short trend - Reverse Long
  lng_rev <- NULL
  lng_rev <-ftse100 %>% filter(Date == rday) %>% filter_rev_lng()
  lng_rev <- lng_rev %>%
    select_rev_lng() %>%
    mutate(LngRevOpenPr = Close - (atr*0.75)) %>%
    arrange(desc(ma25_Oa))
  lng_rev





# Pull Back in trend ----------------------------------------------------------------------

  # Combine PB / Mx
  # Long
  lng_pb <- NULL
  #rday <- "2024-03-11"
  atr_off <- 0.2
  lng_pb <-ftse100 %>% filter(Date==rday) %>% filter_lng()
  lng_pb <- lng_pb %>% mutate(LngOpenPB = Close - (atr*0.5))
  lng_pb <- lng_pb %>% mutate(LngOpenMx = rmnL5+(atr*atr_off), GapMx = (Close-rmnL5)/atr) %>% adorn_rounding()
  lng_pb %>% select(Tick,LS,1:6,atr,rmxH5,rmxH5Ca,rmxH20Ca,rn,LngOpenPB,LngOpenMx,GapMx) %>%
    arrange(GapMx)

  # Short
  sht_pb <- NULL
  atr_off <- 0.2
  sht_pb <- ftse100 %>% filter(Date==rday) %>% filter_sht()
  sht_pb <- sht_pb %>% mutate(ShtOpenPB = Close + (atr*0.5))
  sht_pb <-sht_pb %>% mutate(ShtOpenMn = rmxH5-(atr*atr_off), GapMn = (rmxH5-Close)/atr) %>% adorn_rounding()
  sht_pb %>% select(Tick,LS,1:6,atr,rmnL5C,rmnL5Ca,rmnL20Ca,rn,ShtOpenPB,ShtOpenMn,GapMn) %>%
    arrange(GapMn)


# Lng Overlap
  sht_rev %>% filter(Tick %in% lng_pb$Tick) %>% pull(Tick)
# Sht Overlap
  lng_rev %>% filter(Tick %in% sht_pb$Tick) %>% pull(Tick)

  rday = "2024-02-05"
  tk <- ftse100 %>% filter(Date==rday) %>% filter_lng() %>% pull(Tick)
  ftse100 %>% filter(Date >= rday, Tick %in% tk[3]) %>%
    select(LS,1:6,atr) %>% adorn_rounding() %>% head()

-------------------------------------------
# Check week ...
# Reverse trades

# Rev from Lng trade SHORT

  rday  <- "2024-03-20"
  rday2 <- "2024-03-21"
  sht_rev <- NULL
  sht_frms <- ftse100 %>% filter(Date == rday) %>% filter_rev_sht() %>% pull(Tick)

  sht_rev_lng <- ftse100 %>% filter(Date==rday2, Tick %in% sht_frms) %>%
    mutate(ShtRevOpenPr = pC + (atr*0.75)) %>% filter(High > ShtRevOpenPr)
  unique(sht_rev_lng$Tick)

  left_join(
    ftse100 %>% filter(Date>=rday2, Tick %in% sht_rev_lng$Tick),
    sht_rev_lng %>% select(Tick,ShtRevOpenPr),by = join_by(Tick)
  ) %>%  adorn_rounding(digits = 1) %>%
    mutate(mxpl = (ShtRevOpenPr-Low)/atr) %>% adorn_rounding() %>%
    select(pLS,Date,Tick,1:6,atr,mxpl,ShtRevOpenPr)


  # Rev from Sht trade Long
  rday  <- "2024-03-19"
  rday2 <- "2024-03-20"
  lng_rev <- NULL
  Lng_frms <- ftse100 %>% filter(Date == rday) %>% filter_rev_lng() %>% pull(Tick)

  # triggered
  lng_rev_sht <- ftse100 %>% filter(Date == rday2, Tick %in% Lng_frms) %>%
    mutate(LngRevOpenPr = pC - (atr*0.75)) %>%
    filter(Low < LngRevOpenPr)
  unique(lng_rev_sht$Tick)
  # join
  left_join(
    ftse100 %>% filter(Date>=rday2, Tick %in% lng_rev_sht$Tick),
    lng_rev_sht %>% select(Tick,LngRevOpenPr),by = join_by(Tick)
  ) %>%  adorn_rounding(digits = 0) %>%
    mutate(mxpl = (High-LngRevOpenPr)/atr) %>% adorn_rounding() %>%
    select(pLS,Date,Tick,1:6,atr,mxpl,LngRevOpenPr)


# PB trades
  rday  <- "2024-03-15"
  rday2 <- "2024-03-18"
  atr_off <- 0.2
  lng_pb <- ftse100 %>% filter(Date==rday) %>% filter_lng()
  tck_lst <- lng_pb$Tick # trade LONG

  # PB Long - 0.5 atr
  pb_lng_day <- ftse100 %>% filter(Date==rday2, Tick %in% tck_lst, (pC-Low)/atr >= 0.5) %>%
    mutate(LngOpenPB = pC - (atr*0.5))
  unique(pb_lng_day$Tick)
  # join
  left_join(
    ftse100 %>% filter(Date>=rday2, Tick %in% pb_lng_day$Tick),
    pb_lng_day %>% select(Tick,LngOpenPB), by = join_by(Tick)
  ) %>% adorn_rounding(digits = 0) %>%
    mutate(mxpl = (High-LngOpenPB)/atr,
           mnpl = (Low-LngOpenPB)/atr) %>% adorn_rounding(digits = 1) %>%
    select(pLS,Date,LngOpenPB,pC,1:6,atr,mxpl,mnpl)


  # PB LongMx ...
  mx_pb_lng_day <- ftse100 %>% filter(Date == rday2, Tick %in% tck_lst, Low <= prmnL5+(atr*atr_off)) %>%
    mutate(LngOpenMx = prmnL5+(atr*atr_off), GapMx = (pC-prmnL5)/atr)
  unique(mx_pb_lng_day$Tick)
  # join
  left_join(
  ftse100 %>% filter(Date>=rday2, Tick %in% mx_pb_lng_day$Tick) %>% adorn_rounding(digits = 0),
  mx_pb_lng_day %>% select(Tick,LngOpenMx,GapMx), by = join_by(Tick)
  ) %>% select(pLS,Date,LngOpenMx,GapMx,pC,1:6,atr) %>%
    mutate(mxpl = (High-LngOpenMx)/atr,mnpl = (Low-LngOpenMx)/atr) %>% adorn_rounding()


  # PB Short
  sht_pb <-ftse100 %>% filter(Date==rday) %>% filter_sht()
  tck_lst_sh <- sht_pb$Tick # trade SHORT

  pb_sht_day <- ftse100 %>% filter(Date==rday2, Tick %in% tck_lst_sh, (High-pC)/atr >= 0.5) %>%
    mutate(ShtOpenPB = pC + (atr*0.5))
  unique(pb_sht_day$Tick)
  # join
  left_join(
    ftse100 %>% filter(Date>=rday2, Tick %in% pb_sht_day$Tick),
    pb_sht_day %>% select(Tick,ShtOpenPB), by = join_by(Tick)
  ) %>% adorn_rounding(digits = 0) %>%
    mutate(mxpl = (ShtOpenPB-Low)/atr) %>% adorn_rounding(digits = 1) %>%
    select(pLS,Date,ShtOpenPB,pC,1:6,atr,mxpl)

  # PB ShortMn ...
  mn_pb_sht_day <- ftse100 %>% filter(Date == rday2, Tick %in% tck_lst_sh, High >= prmxH5-(atr*atr_off)) %>%
    mutate(ShtOpenMn = prmxH5-(atr*atr_off), GapMn = (prmxH5-pC)/atr)
  unique(mn_pb_sht_day$Tick)
  # join
  left_join(
    ftse100 %>% filter(Date>=rday2, Tick %in% mn_pb_sht_day$Tick) %>% adorn_rounding(digits = 0),
    mn_pb_sht_day %>% select(Tick,ShtOpenMn,GapMn), by = join_by(Tick)
  ) %>% select(pLS,Date,ShtOpenMn,GapMn,pC,1:6,atr) %>%
    mutate(mxpl = (ShtOpenMn-Low)/atr) %>% adorn_rounding()











# Long PB
  lng_pb <- NULL
  lng_pb <-ftse100 %>% filter(Date==rday) %>%
    filter_lng() %>% adorn_rounding()
  lng_pb %>% mutate(LngOpenP = Open - (atr*0.5)) %>% select(Tick,LS,1:6,atr,rmxH5,rmxH5Ca,rmxH20Ca,rn,LngOpenP)

# Short PB
  sht_pb <- NULL
  sht_pb <-ftse100 %>% filter(Date==rday) %>%
    filter_sht() %>% adorn_rounding()
  sht_pb %>% select(LS,1:6,atr,rmnL5C,rmnL5Ca,rmnL20Ca,rn)



# Trend - 5 day Mx PB -----------------------------------------------------
  # 5 day low
  atr_off <- 0.2
  lng_mn <- ftse100 %>% filter(Date==rday, rn >= 3) %>%
    filter_lng() %>%
    mutate(OpenP = rmnL5+(atr*atr_off), Gap = (Close-rmnL5)/atr) %>%
    select(1:6,8,rmnL5,rmxH5,rmxH20,rn,OpenP,Gap) %>%
    adorn_rounding() %>% arrange(Gap)
  lng_mn


  # 5 day high
  # rday <- "2024-03-06"
  atr_off <- 0.2
  sht_mx <- ftse100 %>% filter(Date==rday, rn >= 3) %>%
    filter_sht() %>%
    mutate(OpenP = rmxH5-(atr*atr_off), Gap = (rmxH5-Close)/atr) %>%
    select(LS,1:6,8,rmxH5,rmnL5,rmnL20,rn,OpenP,Gap) %>%
    adorn_rounding()
  sht_mx





