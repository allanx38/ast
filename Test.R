

library(dplyr)
library(ast)
ast::add_ATR()

df <- data.frame(A = c(1,2,3,4,5), B= c(6,7,8,9,10))

df %>% filter(A < 3)

