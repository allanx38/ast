

ast::load_libs()
run_dir <- "D:/Allan/R_Files/ast/Trading/"

# use Quantmod, print out FTSE plots
rmarkdown::render(input = paste0(run_dir, "RFiles/ftse_charts_q.R"),
                  output_file = paste0(run_dir, "Reports/ftse_charts_q.html"))

rmarkdown::render(input = paste0(run_dir, "RFiles/ftse_charts_q2.R"),
                  output_file = paste0(run_dir, "Reports/ftse_charts_q2.html"))




# Run with para
rmarkdown::render(input.R, params = list(inst = instnm), output.html)

rmarkdown::render(input = "/home/allanx38/ast/Trade/RunRMD/TestWithParam.R",
                  params = list(inst = "AllanTest"),
                  output_file = "/home/allanx38/ast/Trade/RunRMD/TestWithParam.html")


rmarkdown::render(input = "/home/allanx38/ast/Trade/RunRMD/FTSE_ma5_25_150.R",
                  params = list(tday = "2024-02-08"),
                  output_file = "/home/allanx38/ast/Trade/RunRMD/FTSE_ma5_25_150.html")


# run no para
rmarkdown::render(input.R, out_put.html)


rmarkdown::render(input = "/home/allanx38/ast/Trade/RunRMD/TestNoParam.R",
                  output_file = "/home/allanx38/ast/Trade/RunRMD/TestNoParam.html")


rmarkdown::render(input       = "/home/allanx38/ast/Trade/RunRMD/FTSE_BTest_MA.R",
                  output_file = "/home/allanx38/ast/Trade/RunRMD/FTSE_BTest_MA.html")

rmarkdown::render(input       = "/home/allanx38/ast/Trade/RunRMD/FTSE_MA_ALL.R",
                  output_file = "/home/allanx38/ast/Trade/RunRMD/FTSE_MA_ALL.html")

rmarkdown::render(input       = "/home/allanx38/ast/Trade/RunRMD/FTSE_MA25.R",
                  output_file = "/home/allanx38/ast/Trade/RunRMD/FTSE_MA25.html")

rmarkdown::render(input       = "/home/allanx38/ast/Trade/RunRMD/FTSE_MA25_rn5.R",
                  output_file = "/home/allanx38/ast/Trade/RunRMD/FTSE_MA25_rn5.html")

rmarkdown::render(input       = "/home/allanx38/ast/Trade/RunRMD/portfolio_etf.R",
                  output_file = "/home/allanx38/ast/Trade/RunRMD/portfolio_etf.html")

rmarkdown::render(input       = "/home/allanx38/ast/Trade/RunRMD/plot_quantmod.R",
                  output_file = "/home/allanx38/ast/Trade/RunRMD/plot_quantmod.html")

rmarkdown::render(input       = "/home/allanx38/ast/Trade/RunRMD/FTSE_HL.R",
                  output_file = "/home/allanx38/ast/Trade/RunRMD/FTSE_HL.html")

# looks for when Close is above/below both ma25/ma150
rmarkdown::render(input       = "/home/allanx38/ast/Trade/RunRMD/ma25_150.R",
                  output_file = "/home/allanx38/ast/Trade/RunRMD/ma25_150.html")

rmarkdown::render(input       = "/home/allanx38/ast/Trade/RunRMD/ma25_150_1_BTest.R",
                  output_file = "/home/allanx38/ast/Trade/RunRMD/ma25_1501_BTest.html")

rmarkdown::render(input       = "/home/allanx38/ast/Trade/RunRMD/ftse_plot1.R",
                  output_file = "/home/allanx38/ast/Trade/RunRMD/ftse_plot1.html")

rmarkdown::render(input       = "/home/allanx38/ast/Trade/RunRMD/ma25_150_btest.R",
                  output_file = "/home/allanx38/ast/Trade/RunRMD/ma25_150_btest.html")




# Load Data ---------------------------------------------------------------

ast::load_libs()

# Add Data from scratch
  f100 <- ast::get_ftse_ticks_csv_list()
  for(i in 1:length(f100)){
    fn <- paste0("D:/Allan/R_Files/Data/FTSE100/", f100[i], ".csv")
    ast::save_qm_data_df(f100[i],csv_dir_and_nm = fn, days_backwards_p = 400)
  }

  
  
# Update existing data
  update_tick_on_disc <- function(data_dir){
    ftse_file_names <- list.files(path=data_dir)
    for(i in 1 : length(ftse_file_names)){
      print(paste0(i, " of ", length(ftse_file_names)))
      x <- NULL
      tdata <- NULL
      dt <- NULL
      x <- read.csv( paste0(data_dir, "/", ftse_file_names[i]) )
      if(!is.null(x)){
        # read data from Yahoo
        ticker <- stringr::str_sub(ftse_file_names[i],0,-5)
        #dt <- ast::get_yahoo_fin_tick_tc(sym = ticker,nm = ticker,daysback = 50)
        try(dt <- get_yahoo_fin_tick(sym = ticker,nm = ticker, daysback = 50))
        if(!is.null(dt)){
          dt <- dt %>% select(Date,Open,High,Low,Close,Tick)
          dt$Date <- as.character(dt$Date)
          # read data saved on disc
          cs <- read.csv( paste0(data_dir, "/", ftse_file_names[i]) )
          cs <- cs %>% filter(Open != 0)
          # remove 0 values ...
          cs <- cs %>% filter(Open != 0)
          # combine rows and save
          tdata <- bind_rows(cs,dt %>% filter(!Date %in% cs$Date)) %>% distinct() %>% arrange(Date)
          readr::write_csv(tdata, paste0(data_dir, "/", ftse_file_names[i]))
        }
      }
    }
  }
  
  update_tick_on_disc("D:/Allan/R_Files/Data/FTSE100")
  

# Ideas -------------------------------------------------------------------

  data_dir <- "D:/Allan/R_Files/Data/FTSE100"
  ftse_file_names <- list.files(data_dir)
  tdata <- read.csv(paste0(data_dir, "/", ftse_file_names[2]))
  
  tdata$ma20 <- runMean(tdata$Close, n=20)
  tdata <- tdata %>% 
    mutate(Diff = Close - ma20,
           DiffPer = round((Diff/Close)*100,1)) %>% 
    adorn_rounding(digits = 1)

  write.csv(tdata,"D:/Allan/R_Files/Data/test.csv")  
  
  