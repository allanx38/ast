





#' candle_macd_bb
#' plots data using quantmod
#' adds macd and bolinger bands
#'
#' @param pData
#' @param data_name
#' @param sset sub set e.g. '2022-07::2023-01'
#'
#' @return
#' @export
#'
candle_macd_bb <- function(pData,data_name,sset){
  chartSeries(pData,
              type="candlesticks",
              subset=sset,
              theme=chartTheme('white'),
              TA = 'addMACD()',
              name = data_name)
  addBBands()
}



#' get_data_candle_macd_bb
#' gets data and calls candle_macd_bb
#' adds macd and Bolinger bands
#'
#' @param sym
#' @param days_back
#' @param sset
#'
#' @return
#' @export
#'
get_data_candle_macd_bb <- function(sym,days_back,sset){
  Data <- get_data(sym,days_back)
  candle_macd_bb(Data,sym,sset)
}



# GGPlot ------------------------------------------------------------------


gg <- function(pdata, t_txt=""){
  ggplot(pdata, aes(x=Date)) +
    geom_line(aes(y = Close), group = 1) +
    scale_x_date(date_breaks = "3 month", date_labels = "%y%m") +
    theme(axis.text.x = element_text(angle = 70, hjust = 1)) +
    ggtitle(t_txt)
}

# ggplot - ma25, ma150
# needs data set with ma's
gg_ma25_150 <- function(pdata, t_txt=""){
  ggplot(pdata, aes(x=Date)) +
    geom_line(aes(y = Close), group = 1) +
    geom_line(aes(y = ma150), color="darkred", group = 1) +
    #geom_line(aes(y = ma50), color="steelblue", linetype="twodash", group = 1) +
    geom_line(aes(y = ma25), color="orange", linetype="longdash", group = 1) +
    scale_x_date(date_breaks = "3 month", date_labels = "%y%m") +
    theme(axis.text.x = element_text(angle = 70, hjust = 1)) +
    ggtitle(t_txt)
}


# ggplot vline ------------------------------------------------------------


gg_vline <- function(gg_data, t_txt="", v_line_date){
  ggplot(gg_data, aes(x=Date)) +
    geom_line(aes(y = Close), group = 1) +
    scale_x_date(date_breaks = "3 month", date_labels = "%y%m") +
    theme(axis.text.x = element_text(angle = 70, hjust = 1)) +
    ggtitle(t_txt) +
    geom_vline(xintercept = as.Date(v_line_date))
}

gg_vline_ma25 <- function(gg_data, t_txt="", v_line_date){
  ggplot(gg_data, aes(x=Date)) +
    geom_line(aes(y = Close), group = 1) +
    geom_line(aes(y = ma25), color="orange", linetype="longdash", group = 1) +
    scale_x_date(date_breaks = "3 month", date_labels = "%y%m") +
    theme(axis.text.x = element_text(angle = 70, hjust = 1)) +
    ggtitle(t_txt) +
    geom_vline(xintercept = as.Date(v_line_date))
}

gg_vline_ma25_150 <- function(gg_data, t_txt="", v_line_date){
  ggplot(gg_data, aes(x=Date)) +
    geom_line(aes(y = Close), group = 1) +
    geom_line(aes(y = ma25), color="orange", linetype="longdash", group = 1) +
    geom_line(aes(y = ma150), color="red", linetype="longdash", group = 1) +
    scale_x_date(date_breaks = "3 month", date_labels = "%y%m") +
    theme(axis.text.x = element_text(angle = 70, hjust = 1)) +
    ggtitle(t_txt) +
    geom_vline(xintercept = as.Date(v_line_date))
}



# ggplot candlestick ------------------------------------------------------

gg_candleStick_plot_ftse <- function(tick, tail_nm = 75){

  pData <- read.csv(paste0("/home/allanx38/ast/Data/FTSE_Data/",tick,".csv")) %>% tail(tail_nm)
  pData <- pData %>% mutate(greenRed=ifelse(Open-Close>0,"Red","Green"))
  pData$Date <- as.Date(pData$Date)

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
    scale_color_manual(values=c("Forest Green","Red")) +
    scale_x_date(date_breaks = "1 month", date_labels = "%y%m") +
    ggtitle(tick) +
    theme(legend.position ="none",
          axis.title.y = element_blank(),
          axis.title.x=element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
          plot.title= element_text(hjust=0.5))
}


gg_add_ma25_150 <- function(){
  gg +
  geom_line(aes(y = ma25), color="orange", linetype="longdash", group = 1) +
  geom_line(aes(y = ma150), color="red", linetype="longdash", group = 1)
}


