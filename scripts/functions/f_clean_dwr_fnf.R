# f_clean_dwr_fnf

# clean flow data for ffcalc
library(dplyr)
library(readr)
library(glue)
library(contentid)
library(janitor)

f_clean_flow_verona <- function(siteID) {
  
  # get raw data ID:
  flow <- contentid::store(glue("data_raw/raw_dwr_fnf_{siteID}.csv"))
  
  flow_file <- contentid::resolve("hash://sha256/ac986e4bba591e050e68705e48dbc9002ce1151158d0bcab8a979c37aaf2b8b3")
  
  # read in data
  flowdat <- read_csv(flow_file) %>%
    clean_names() %>%
    # filter to 1996 to current
    filter(date >= as.Date("1996-10-01"))
  
  write_csv(flowdat, file=glue("data_clean/clean_dwr_fnf_{siteID}.csv"))
}
