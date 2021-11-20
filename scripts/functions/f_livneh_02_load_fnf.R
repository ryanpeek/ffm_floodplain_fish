# load data function

library(dplyr)
library(contentid)
library(readr)
library(glue)
library(here)

f_load_livneh_fnf <- function(siteID="TUO") {
  
  # get raw data ID:
  (liv_fnf_daily <- contentid::store(here(glue("data_clean/clean_fnf_daily_{siteID}_livneh.csv"))))
  (liv_fnf_ann <- contentid::store(here(glue("data_clean/clean_fnf_ann_stats_{siteID}_livneh.csv"))))
  # resolve (make sure id exists and matches)
  liv_daily_file <- contentid::resolve(liv_fnf_daily)
  liv_annual_file <- contentid::resolve(liv_fnf_ann)
  
  # read in data, use global super assignment <<
  liv_daily_df <<- read_csv(liv_daily_file, show_col_types = FALSE)
  liv_ann_df <<- read_csv(liv_annual_file, show_col_types = FALSE)
  
  print("Data loading complete.")
  
}
