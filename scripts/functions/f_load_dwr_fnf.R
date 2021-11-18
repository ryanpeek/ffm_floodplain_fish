# load data function

library(dplyr)
library(contentid)
library(readr)
library(glue)

f_load_dwr_fnf <- function(site="TLG") {
  
  # get raw data ID:
  (dwr_fnf <- contentid::store(glue("data_clean/clean_dwr_fnf_{site}.csv")))
  
  dwr_file <- contentid::resolve("hash://sha256/e38981ce3149756bc3498e84b523caa77eab5bc82b81701f128f21f2c70e0745")
  
  # read in data
  dwr_dat <- read_csv(dwr_file)
  
  print("Data loading complete.")
  
  return(dwr_dat)
  
}
