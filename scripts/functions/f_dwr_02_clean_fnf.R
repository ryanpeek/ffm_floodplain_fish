# Function to clean the FNF data from DWR site

library(dplyr) # data wrangling
library(fs) # platform independent handling of file systems
library(readr) # import/export
library(glue) # pasting stuff together
library(contentid) # using hash id to see if content changed
library(lubridate) # datetimes
library(janitor) # cleaning data
library(ggplot2) # plotting
library(imputeTS) # time-series interpolation
library(wateRshedTools) # custom functions
library(fasstr) # water/stream functions

f_dwr_02_clean_fnf <- function(siteID="TLG") {
  
  # get data:
  print("loading raw data...")
  
  # get raw data ID:
  flowdat <- contentid::store(glue("data_raw/fnf_daily_dwr_{siteID}_downloaded_cdec.csv"))
  print(glue("The hash for current data is: {flowdat}"))
  
  # pull unique hash of data
  flow_file <- contentid::resolve(flowdat)
  
  # read in data
  flowdat <- read_csv(flow_file, show_col_types = FALSE) %>%
    # add water year, dowy columns
    wateRshedTools::add_WYD(., "date")
  
  flowdat <- flowdat %>%
    rename(flow=value) 
    
  # Interpolate Values ------------------------------------------------------
  
  # use imputeTS: http://steffenmoritz.github.io/imputeTS/
  
  clean_df <- flowdat %>%
    # first fill all negatives with NA then interpolate
    mutate(flow_na = ifelse(flow<0, NA_integer_, flow),
           # if zero flow days, fill with NA as well
           flow_na = ifelse(flow==0, NA_integer_, flow_na),
           # now interpolate only na's
           ## structural time series model (Kalman)
           flow_interp_cfs = imputeTS::na_kalman(flow_na), 
           flow_interp_cms = 0.028316847*flow_interp_cfs,
           # mean average over 7 days
           flow_ma7_cfs = imputeTS::na_ma(flow_na, k = 7, weighting = "exponential"))
  
  #  how do these compare?
  clean_df %>% filter(is.na(flow_na)) %>% ggplot() + geom_point(aes(x=flow_interp_cfs, y=flow_ma7_cfs, color=WY)) + geom_abline(slope=1, intercept = 0)
  
  ## ADD ANNUAL VOLUME in thousand Acre Feet per day (sum(1 cfs * 1.983)/1000) ------
  
  clean_df <- clean_df %>% 
    group_by(WY) %>% # convert to acre feet per day
    mutate(ann_tot_vol_taf = sum((flow_interp_cfs*1.983)/1000)) %>% 
    # add percentile value for each flow value
    group_by(station_id) %>% 
    mutate(
      percentile = round((stats::ecdf(flow_interp_cfs))(flow_interp_cfs), 4) * 100) %>% 
    ungroup()
  
  ## Calc Centroid Timing with fasstr ----------------------------------------
  
  ## see here: (https://bcgov.github.io/fasstr/index.html)
  
  # calc timing
  ann_flow_timing <- calc_annual_flow_timing(data = clean_df, dates = date, 
                                             values = flow_interp_cms, groups = station_id,
                                             water_year_start = 10, 
                                             start_year = 1987, end_year = 2021, 
                                             percent_total = c(25, 50, 75)) %>%
    # clean names
    clean_names() %>% 
    rename(dowy_25pct_tot_q = 3, date_25pct_tot_q = 4,
           dowy_50pct_tot_q = 5, date_50pct_tot_q = 6,
           dowy_75pct_tot_q = 7, date_75pct_tot_q = 8) 
    
  # left join back to clean_df
  clean_df_timing <- left_join(clean_df, ann_flow_timing[,-1], by=c("WY"="year"))
    
  # calc ALL annual stats
  all_ann_stats <- calc_all_annual_stats(data = clean_df, dates = date, 
                                         values = flow_interp_cms, groups = station_id,
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
  
  ## SAVE OUT CSV -------------------------------------------
  
  fs::dir_create("data_clean")
  
  # write out
  readr::write_csv(clean_df_timing, glue("data_clean/clean_fnf_daily_dwr_{siteID}_cdec.csv"))
  
  # print message!
  print(glue("Daily data saved here: 'data_clean/clean_fnf_daily_dwr_{siteID}_cdec.csv'"))
  
  # write out
  readr::write_csv(all_ann_stats, glue("data_clean/clean_fnf_ann_stats_{siteID}_cdec.csv"))
  
  # print message!
  print(glue("Annual data saved here: 'data_clean/clean_fnf_daily_dwr_{siteID}_cdec.csv'"))

}
