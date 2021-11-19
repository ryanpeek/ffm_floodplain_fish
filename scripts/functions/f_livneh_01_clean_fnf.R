# Import and Clean data from Livneh

library(glue)
library(dplyr)
library(lubridate)
library(readr)
library(janitor)
library(fasstr)
library(wateRshedTools) 
# custom package: devtools::install_github("ryanpeek/wateRshedTools")


# get data
f_clean_livneh_fnf <- function(siteID="TUO") {
  
  # get raw data ID:
  flowdat <- contentid::store(glue("data_raw/fnf_daily_livneh_{siteID}_1950_2013.csv"))
  print(glue("The hash for current data is: {flowdat}"))
  
  # this is hard coded so if something changes we need to rerun to get new hash
  flow_file <- contentid::resolve("hash://sha256/74912be74cf9dc9a8c8de3357a2a70804392d2bf983332e2272384a53e02643f")
  
  # read in data
  flowdat <- read_csv(flow_file, show_col_types = FALSE) %>%
    # add water year, dowy columns
    mutate(date = mdy(date)) %>% 
    add_WYD(., "date") %>% 
    rename(flow_cfs=flow) %>% 
    # convert and add acreft
    mutate(flow_cms = 0.028316847*flow_cfs, .after=flow_cfs) %>% 
    group_by(WY) %>% 
    mutate(ann_tot_vol_acft = sum((flow_cfs*1.983)/1000),
           station_id = "TUO")
  
  # calc timing
  ann_flow_timing <- calc_annual_flow_timing(data = flowdat, dates = date, 
                                             values = flow_cms, 
                                             groups = station_id,
                                             water_year_start = 10, 
                                             start_year = 1987, end_year = 2021, 
                                             percent_total = c(25, 50, 75)) %>%
    # clean names
    clean_names() %>% 
    rename(dowy_25pct_tot_q = 3, date_25pct_tot_q = 4,
           dowy_50pct_tot_q = 5, date_50pct_tot_q = 6,
           dowy_75pct_tot_q = 7, date_75pct_tot_q = 8) 
  
  # left join back to clean_df
  clean_df_timing <- left_join(flowdat, ann_flow_timing[,-1], by=c("WY"="year"))
  
  # calc ALL annual stats
  all_ann_stats <- calc_all_annual_stats(data = clean_df_timing, 
                                         dates = date, 
                                         values = flow_cms, 
                                         groups = station_id,
                                         water_year_start = 10, 
                                         start_year = 1987, end_year = 2021) %>% 
    mutate(tot_vol_acft = (Total_Volume_m3*0.000810714)/1000) %>% 
    
    # add ann percentile
    group_by(station_id) %>%
    mutate(
      ann_vol_percentile = round((stats::ecdf(tot_vol_acft))(tot_vol_acft), 4) * 100) %>% 
    ungroup() %>% 
    # drop mm cols
    select(-c(ends_with("_mm")))
    
    # warning ok!  
  
  
  ## SAVE OUT CSV -------------------------------------------
  
  # write out
  readr::write_csv(clean_df_timing, glue("data_clean/clean_fnf_daily_{siteID}_livneh.csv"))
  
  # print message!
  print(glue("Daily data saved here: 'data_clean/clean_fnf_daily_{siteID}_livneh.csv'"))
  
  # write out
  readr::write_csv(all_ann_stats, glue("data_clean/clean_fnf_ann_stats_{siteID}_livneh.csv"))
  
  # print message!
  print(glue("Annual data saved here: 'data_clean/clean_fnf_daily_{siteID}_livneh.csv'"))
  
}
