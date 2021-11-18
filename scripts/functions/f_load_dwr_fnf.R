# load data function

library(dplyr)
library(contentid)
library(readr)
library(glue)

f_load_dwr_fnf <- function(site="TLG") {
  
  # get raw data ID:
  (dwr_fnf_daily <- contentid::store(glue("data_clean/clean_fnf_daily_dwr_{site}_cdec.csv")))
  (dwr_fnf_ann <- contentid::store(glue("data_clean/clean_fnf_ann_stats_{site}_cdec.csv")))
  dwr_daily_file <- contentid::resolve(dwr_fnf_daily)
  dwr_annual_file <- contentid::resolve(dwr_fnf_ann)
  
  # read in data, use global super assignment <<
  dwr_daily_df <<- read_csv(dwr_daily_file, show_col_types = FALSE)
  dwr_ann_df <<- read_csv(dwr_annual_file, show_col_types = FALSE)
  
  print("Data loading complete.")
  
  # can use a list
  #return(list(dwr_daily_df, dwr_ann_df))
  
}
