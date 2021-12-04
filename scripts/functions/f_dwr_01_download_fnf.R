# Download and clean data from DWR FNF at TLG

library(glue)
library(dplyr)
library(lubridate)
library(readr)
library(janitor)
library(wateRshedTools) 
# custom package: devtools::install_github("ryanpeek/wateRshedTools")


# pull data: defaults to Verona daily discharge
f_dwr_01_download_fnf <- function(siteID="TLG", sensor=8) {
  
  # get data:
  print("Downloading data...")
  flowdat <- get_cdec(siteID, sensor, "D", start="1986-04-01", end="2021-11-01") %>% 
    mutate(date = as_date(datetime), .after=sensor_type) %>% 
    select(-datetime)

  print("Data downloaded!")
  
  # write out
  write_csv(flowdat, file=glue("data_raw/fnf_daily_dwr_{siteID}_downloaded_cdec.csv"))

}
