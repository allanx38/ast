
# pull back functions

# Long Trades
# res_rd_lng <- res_rd %>% mutate(OL = (Open-Low)/atr, OLPr = Open - (atr/2)) %>% adorn_rounding()
# res_rd_lng <- res_rd_lng %>% add_h5() %>% add_l5()

# pb_lng_res <- function(rd){
#   #browser()
#   rd$pl <- 0
#   rd$mxpl <- 0
#   rd$mxpldy <- 0
#   rd$mnpl <- 0
#   rd$mnpldy <- 0
#   for(i in 1:nrow(rd)){
#     for(j in 1:5){
#       xpl <- rd[[paste0("H",j)]][i] - rd$OLPr[i]
#       if(xpl > rd$mxpl[i]){
#         rd$mxpl[i] <- xpl
#         rd$mxpldy[i] <- j
#       }
#       npl <- rd[[paste0("L",j)]][i] - rd$OLPr[i]
#       if(npl < rd$mnpl[i]){
#         rd$mnpl[i] <- npl
#         rd$mnpldy[i] <- j
#       }
# 
#       if(rd$pl[i]==0){
#         if(rd[[paste0("L",j)]][i] < (rd$OLPr[i] - rd$atr[i])){
#           rd$pl[i] = 0 - rd$atr[i]
#         }
#       }
#       if(rd$pl[i]==0){
#         if(rd[[paste0("H",j)]][i] > (rd$OLPr[i] + rd$atr[i])){
#           rd$pl[i] = rd$atr[i]
#         }
#       }
#     }
#   }
#   return(rd)
# }
# 
# rday <- res_rd_lng %>% filter(LS == "L",
#                               #Date >= "2023-04-01",
#                               Date <= "2023-12-28",
#                               maLO <= 1.5, OL >= 0.5)
# res <- pb_lng_res(rday) %>% select(atr,pl,everything())
# #res %>% select(atr,pl,OLPr,mxH,mxpl,mxpldy,mnpl,mnpldy, everything()) %>% View()
# res <- res %>% arrange(Date) %>% filter(pl != 0) %>%
#   mutate(pl2 = if_else(pl > 0,100,-100),
#          win = if_else(pl>0,1,0),
#          mth = lubridate::month(Date),
#          yr = lubridate::year(Date))
# res %>% select(yr,mth) %>% distinct() %>% arrange(yr,mth) %>% View()
# #res %>% select(1:8,pl2,win,mth) %>% View()
# res %>% group_by(yr,mth) %>% summarise(sm = sum(pl2)) %>% View()
# res %>% group_by(mth,win) %>% tally() %>% pivot_wider(names_from = win, values_from = n) %>%
#   mutate(WinP = `1`/(`1`+`0`)*100) %>% View()
# res %>% mutate(cm = cumsum(pl2)) %>% View()
# 
# sum(res$pl)
# res %>% filter(pl>0) %>% count() / nrow(res) * 100
# res %>% filter(pl<0) %>% count()
# 
# dt <- as.Date("2023-07-05")
# dt2 <- as.Date("2023-07-06")
# # check what happened
# tt <- res_rd_lng %>% filter(LS == "L", Date == dt, maLO <= 1.5) %>% arrange(maLO)
# ttol <- res_rd_lng  %>% filter(Tick %in% tt$Tick,Date == dt2, OL >= 0.5)
# res_rd_lng %>% filter(Tick %in% ttol$Tick,Date >= dt2,Date <= dt2+5) %>%
#   select(Tick,Date,atr,OLPr,High,everything()) %>% View()
