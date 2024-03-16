
load_libs <- function(){
  library(dplyr)
  library(lubridate)
  library(quantmod)
  library(janitor)
  library(ast)
  library(readr)
  library(tidyr)
  library(TTR)
}

rnd_all <- function(pdata, rnd_num = 0){
  pdata %>% mutate(across(where(is.numeric), round,rnd_num))
}


add_ATR <- function(pdata, rnd_num = 1){
  # Add ATR
  atr <- ATR(pdata[,c("High","Low","Close")], n=14)
  pdata <- cbind(pdata,atr)
  pdata <- pdata %>% select(-trueHigh,-trueLow)
  pdata$atr <- round(pdata$atr,rnd_num)
  return(pdata)
}



add_MACD <- function(pdata, col_nm){

  # macd The price (volume, etc.) oscillator.
  # signal The oscillator signal line (a moving average of the oscillator).

  macd <- TTR::MACD(pdata[[col_nm]])
  pdata <- cbind(pdata,macd)
  return(pdata)
}


add_BB <- function(pdata, col_nm){

  # Bollinger Bands

  bb <- TTR::BBands(pdata[[col_nm]])
  pdata <- cbind(pdata,bb)
  return(pdata)
}



add_ADX <- function(pdata, rnd_num = 1){

  # The DIp/DIn (positive/negative) is the percentage of the true range that is up/down.
  # DIp The positive Direction Index.
  # DIn The negative Direction Index.
  # DX The Direction Index.
  # ADX The Average Direction Index (trend strength).

  # Add ADX
  adx <- ADX(pdata[,c("High","Low","Close")]) %>% ast::rnd_all(1)
  pdata <- cbind(pdata,adx)
  return(pdata)
}

add_Aroon <- function(pdata, rnd_num = 1){

  # Aroon up (down) is the elapsed time, expressed as a percentage, between today and the highest
  # (lowest) price in the last n periods. If today’s price is a new high (low) Aroon up (down) will be
  # 100. Each subsequent period without another new high (low) causes Aroon up (down) to decrease
  # by (1 / n) x 100.

  # aroonUp The Aroon up indicator.
  # aroonDn The Aroon down indicator.
  # oscillator The Aroon oscillator (aroonUp - aroonDn).

  # Strong trends are
  # indicated when when the aroonUp(Dn) is above 70 while the aroonDn(Up) is below 30.

  aroon <- aroon(pdata[,c("High","Low")], n=20)
  pdata <- cbind(pdata,aroon)
  return(pdata)
}
