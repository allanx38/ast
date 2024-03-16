
# Pair Single Functions

# create a series and pairs
# use ma of diff to go lng/short in "main" one ..


#' get_yahoo_fin_tick
#'
#' Get data from Yahoo using Quantmod
#' Returns a data frame
#'
#' @param sym
#' @param nm
#' @param daysback
#'
#' @return a data frme
#' @export
#'
get_yahoo_fin_tick <- function(sym,nm,daysback){
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


#' get_data_from_file
#'
#' Get data from csv file
#' SAME interface as get_yahoo_fin_tick
#'
#' @param sym
#' @param nm
#' @param daysback
#'
#' @return
#' @export
#'
get_data_from_file <- function(sym,nm,daysback){
  sym_data <- read.csv( paste0("/home/allanx38/ast/Data/FTSE_Data/", sym,".csv") )
  sym_data$Tick <- nm
  sym_data <- tail(sym_data, n = daysback)
  return( sym_data %>% select(Tick,everything()) %>% rnd_all() )
}


#' gen_pair_open_pr
#' create an accurate grouping number ... or just use lag actually ...
#' so 2 is long, -2 is short ...
#'
#' @param p1      Symbol of first stock
#' @param p1_nm   Name of first stock
#' @param p2      Symbol of second stock
#' @param p2_nm   Name of second stock
#' @param daysback Num of days to go back
#' @param FUN     function to get data
#'
#' @return
#' @export
#'
gen_pair_open_pr <- function(p1,p1_nm,p2,p2_nm,daysback,FUN){
  # dax is first pair, cac 2nd ...
  dax <- FUN(p1,p1_nm,daysback)
  cac <- FUN(p2,p2_nm,daysback)
  pair <- inner_join(dax %>% select(Date,Open,High,Low,Close,Tick) %>%
                       rename(D_O = Open,D_H = High,D_L = Low,D_C = Close),
                     cac %>% select(Date,Open,High,Low,Close,Tick) %>%
                       rename(C_O = Open,C_H = High,C_L = Low,C_C = Close,p2Tick = Tick),
                     by = "Date")
  pair$diff <- round(pair$D_O - pair$C_O,2)
  pair$diffma10 <- round(runMean(pair$diff, n = 10),2)
  pair$diffma25 <- round(runMean(pair$diff, n = 25),2)
  pair$LS10 <- pair$diff - pair$diffma10
  pair$LS25 <- pair$diff - pair$diffma25

  pair$L10 <- 0
  pair$L25 <- 0

  pair$L10 <- ifelse(pair$LS10>0,1,-1)
  pair$L25 <- ifelse(pair$LS25>0,1,-1)
  pair$LSum <- pair$L10 + pair$L25
  pair$LSum_lag <- lag(pair$LSum)

  pair$grp <- 0
  grp_id <- 0
  for (i in 27:nrow(pair)) {
    if(pair$LSum[i] != pair$LSum_lag[i]) {
      grp_id <- grp_id + 1
    }
    pair$grp[i] <- grp_id
  }

  pair$LSum2 <- paste0(pair$LSum,"_",pair$LSum_lag)

  pair <- pair %>% group_by(grp) %>% mutate(grp_rn = row_number(),
                                            grp_mx = max(grp_rn),
                                            d_mx = max(D_H),
                                            d_mn = min(D_L))

  # add MA of Dax alone
  pair$Daxma10 <- round(runMean(pair$D_O, n = 10),2)
  pair$Daxma25 <- round(runMean(pair$D_O, n = 25),2)

  pair$DS10 <- pair$D_O - pair$Daxma10
  pair$DS25 <- pair$D_O - pair$Daxma25

  # ATR
  atr <- ATR(pair[,c("D_H","D_L","D_C")], n=14)
  atr <- as.data.frame(atr)
  pair <- bind_cols(pair,atr %>% select(atr,tr))

  return(pair)
}



#' gen_res_set
#'
#' take output from gen_pair_open_pr and create "results" set ...
#'
#' @param res
#'
#' @return
#' @export
#'
gen_res_set <- function(res){
  res <- as.data.frame(res)
  res2 <- res %>% select(Tick,Date,LSum,atr,starts_with("D"),starts_with("grp"),LS10,LS25) %>% rnd_all()
  res2$LS <- "N"
  res2[which(res2$LSum==2),"LS"] <- "L"
  res2[which(res2$LSum==-2),"LS"] <- "S"
  res2 <- add_ls_num(res2)
  res2 <- res2 %>% select(Tick,Date,LSum,LS,rn,trn,atr,D_O,D_H,D_L,D_C,LS10,LS25)
  res2$OL <- res2$D_O-res2$D_L
  res2$OH <- res2$D_H-res2$D_O
  res2$maOL <- runMean(res2$OL,n=5)
  res2$maOH <- runMean(res2$OH,n=5)
  return(res2)
}




#' trade_long
#'
#' test pull back before entering ...
#' Takes output from gen_res_set
#'
#'
#' @param pdata       output from gen_res_set
#' @param lower_rn    initial trade day
#' @param upper_rn    last trade day
#'
#' @return
#' @export
#'
trade_long <- function(pdata=NULL, lower_rn=2, upper_rn=6){

  lng <- pdata %>% filter(rn>lower_rn, trn>0, LS=="L") %>% group_by(trn) %>%  filter(rn < upper_rn) %>%

    mutate(mx = max(D_H),
           mn = min(D_L),
           atr_f = first(atr),
           oprice = first(D_O),
           cprice = last(D_C),
           pbprice = oprice - (atr_f * 0.4)) %>%

    mutate(tr = case_when(pbprice < first(D_L) ~ 1,
                          pbprice > first(D_L) ~ 0) ) %>%

    mutate(pl      = oprice-pbprice,
           plpb    = cprice-pbprice,
           plpbmx    = mx-pbprice,
           plpbmxatr = round((plpbmx/atr_f)*100,0),
           plpbatr   = round((plpb/atr_f)*100,0)) %>%

    mutate(plpb_s = case_when(plpbmxatr > 100 ~ 1,
                              plpbmxatr < 100 ~ 0) )

  rlng <- lng %>% group_by() %>% summarise(Tick = first(Tick), Dir = "L",
                                           sm = sum(pl),
                                           sm_pb = sum(plpb),
                                           n = n(),
                                           atr = mean(atr_f),
                                           avg_pb = sm_pb/n)

  return( res_list <- list(lng_data = lng, res_lng = rlng) )
}




#' trade_short
#'
#' test pull back before entering ...
#' Takes output from gen_res_set
#'
#'
#' @param pdata       output from gen_res_set
#' @param lower_rn    initial trade day
#' @param upper_rn    last trade day
#'
#' @return
#' @export
#'
trade_short <- function(pdata=NULL, lower_rn=2, upper_rn=6){

  sht <- pdata %>% filter(rn>lower_rn, trn>0, LS=="S") %>% group_by(trn) %>%  filter(rn < upper_rn) %>%

    mutate(mx = max(D_H),
           mn = min(D_L),
           atr_f = first(atr),
           oprice = first(D_O),
           cprice = last(D_C),
           pbprice = oprice + (atr_f * 0.4)) %>%

    # set whether trade price (pullback) has been hit
    mutate(tr = case_when(pbprice > first(D_H) ~ 1,
                          pbprice < first(D_H) ~ 0) ) %>%

    # calc various pls ...
    mutate(pl      = pbprice - oprice, #sht
           plpb    = pbprice - cprice,
           plpbmx    = pbprice - mn,
           plpbmxatr = round((plpbmx/atr_f)*100,0),
           plpbatr   = round((plpb/atr_f)*100,0)) %>%

    mutate(plpb_s = case_when(plpbmxatr > 100 ~ 1,
                              plpbmxatr < 100 ~ 0) )

  rsht <- sht %>% group_by() %>% summarise(Tick = first(Tick), Dir = "S",
                                           sm = sum(pl),
                                           sm_pb = sum(plpb),
                                           n = n(),
                                           atr = mean(atr_f),
                                           avg_pb = sm_pb/n)

  return( res_list <- list(sht_data = sht, res_sht = rsht) )
}






#' gen_pair_res
#'
#' Helper function
#' Just calls gen_pair_open_pr,gen_res_set,trade_long/short ...
#'
#' @param p1
#' @param p2
#'
#' @return
#' @export
#'
gen_pair_res <- function(p1,p2,bLong = TRUE, lower_rn=2, upper_rn=6, FUN){
  res <- gen_pair_open_pr(p1,p1,p2,p2,1000,FUN) %>% gen_res_set()
  if(bLong){
    rr_lst <- trade_long(res,lower_rn,upper_rn)
  } else {
    rr_lst <- trade_short(res,lower_rn,upper_rn)
  }
  return(rr_lst$res_lng)
}


#' loop_gen_pair_res
#'
#' Loop thru a series of pairs and collate results set
#'
#' @param pairs a series of tick pairs
#'
#' @return results sets
#' @export
#'
loop_gen_pair_res <- function(pairs,bLong = TRUE, lower_rn=2, upper_rn=6,FUN){
  all_res <- NULL
  for(i in 1:nrow(pairs)){
    #for(i in 50:80){
    result <- gen_pair_res(pairs$p1[i],pairs$p2[i],bLong,lower_rn, upper_rn,FUN)
    result$p2 <- pairs$p2[i]
    result <- result %>% mutate(lw_n = lower_rn, up_n = upper_rn)
    all_res <- bind_rows(all_res,result)
  }
  return(all_res)
}


#' gen_pairs
#'
#' Helper function
#' Generates pairs of ticks with a particular sector
#' Pass in an index with a sector column
#' Output can be used to check which pairs are good ...
#'
#' @param pdata
#'
#' @return
#' @export
#'
#' @examples
gen_pairs <- function(pdata){
  sec <- pdata %>% select(Sector) %>% distinct() %>% arrange()
  res_set <- NULL
  for(i in seq_along(sec$Sector)){
    loop_data <- pdata %>% filter(Sector == sec$Sector[i])
    if(nrow(loop_data)>1){

      for(j in 1:(nrow(loop_data)-1)){
        for(k in (j+1):nrow(loop_data)){
          pair <- data.frame(p1=loop_data$Tick[j],p2=loop_data$Tick[k])
          res_set <- bind_rows(res_set,pair)
        }
      }

    }
  }
  return(res_set)
}
