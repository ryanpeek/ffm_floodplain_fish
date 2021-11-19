# f_clean_dwr_fnf

# clean flow data for ffcalc
library(dplyr)
library(readr)
library(glue)
library(contentid)
library(lubridate)
library(janitor)
library(ggplot2)
library(imputeTS)
library(wateRshedTools)

f_clean_dwr_fnf <- function(siteID="TLG") {
  
  # get data:
  print("loading raw data...")
  
  # get raw data ID:
  flowdat <- contentid::store(glue("data_raw/fnf_daily_dwr_{siteID}_downloaded_cdec.csv"))
  print(glue("The hash for current data is: {flowdat}"))
  
  # this is hard coded so if something changes we need to rerun to get new hash
  flow_file <- contentid::resolve("hash://sha256/bc46aefc7a96f6ca46652177a124afc793d64ff63cd832891d4e755d15406ba1")
  
  # read in data
  flowdat <- read_csv(flow_file, show_col_types = FALSE) %>%
    # add water year, dowy columns
    add_WYD(., "date")
  
  flowdat <- flowdat %>%
    rename(flow=value) 
    
  # visualize the negatives
  g1 <- flowdat %>% 
    # replace NAs with -1 to plot
    mutate(flow_rev = ifelse(is.na(flow), -1, flow)) %>% {
      ggplot() +
        geom_col(data = ., aes(x=date, y=flow_rev),
                 color=ifelse(.$flow_rev >0, "cyan4", "red3")) +
        labs(title=glue("DWR Full Natural Flow for {siteID}: Raw Data"),
             caption="Negative values are in red",
             x="", y="Flow (cfs)") +
        theme_classic() +
        scale_y_continuous(labels = scales::comma)}
  print(g1)
  
  
  ## Calc Centroid Timing with fasstr (https://cran.r-project.org/web/packages/fasstr/vignettes/fasstr_users_guide.html)
  library(fasstr)
  # screen for missing data
  #tst <- screen_flow_data(data = clean_df, date, flow_kalman_cms, groups = station_id, water_year_start = 10, start_year = 1987, end_year = 2021)
  #plot_missing_dates(data = clean_df, dates = date, values = flow_kalman_cms, groups = station_id, water_year_start = 10, start_year = 1987, end_year = 2021)
  
  # calc ALL annual stats
  all_ann_stats <- calc_all_annual_stats(data = clean_df, dates = date, 
                                 values = flow_kalman_cms, groups = station_id,
                                 water_year_start = 10, 
                                 start_year = 1987, end_year = 2021) %>% 
    mutate(tot_vol_acft = (Total_Volume_m3*0.000810714)/1000) %>% 
    # drop mm cols
    select(-c(ends_with("_mm")))
  
  # cumulative ann stats
  cumulative_ann_stats <- calc_annual_cumulative_stats(data = clean_df, dates = date,
                                               values = flow_kalman_cms, 
                                               groups = station_id,
                                               water_year_start = 10, 
                                               start_year = 1987, end_year = 2021) %>% 
    mutate(tot_vol_acft = (Total_Volume_m3*0.000810714)/1000)
  
  # daily stats
  daily_stats <- calc_daily_stats(data = clean_df, dates = date, 
                                  values = flow_kalman_cms, groups = station_id,
                                  water_year_start = 10, 
                                  start_year = 1987, end_year = 2021)
  
  # calc timing
  ann_flow_timing <- calc_annual_flow_timing(data = clean_df, dates = date, 
                                             values = flow_kalman_cms, groups = station_id,
                                             water_year_start = 10, 
                                             start_year = 1987, end_year = 2021, 
                                             percent_total = c(25, 33.3, 50, 75))
  
  # plots   
  plot_annual_flow_timing(data = clean_df, dates = date, 
                          values = flow_kalman_cms, groups = station_id,
                          water_year_start = 10, 
                          start_year = 1987, end_year = 2021, 
                          percent_total = c(25, 33.3, 50, 75))
  
  
  plot_daily_stats(data = clean_df, dates = date, 
                   values = flow_kalman_cms, groups = station_id,
                   water_year_start = 10, 
                   start_year = 1987, end_year = 2021, add_year = 2014)
  plot_daily_cumulative_stats(data = clean_df, dates = date, 
                              values = flow_kalman_cms, groups = station_id,
                              water_year_start = 10, 
                              start_year = 1987, end_year = 2021)
  
  #flow duration curve:
  plot_flow_duration(data = clean_df, dates = date, values = flow_kalman_cms,
                     groups = station_id, water_year_start = 10, 
                     start_year = 1987, end_year = 2021)
  
  
  ## VISUALIZE --------------------------------------------------------
  
  # visualize the nas and interpolated versions
  #imputeTS::ggplot_na_distribution(clean_df$flow_na)
  #imputeTS::ggplot_na_distribution(clean_df$flow_kalman)
  
  # quick plot
  # p1 <- ggplot(clean_df) +
  #   geom_line(aes(x=date, y=flow_kalman)) +
  #   labs(
  #     title=glue("DWR Full Natural Flow for {siteID}: Interpolated {min(year(clean_df$date))}-{max(year(clean_df$date))}"),
  #     caption="Negative values interpolated with Kalman structural time series {imputeTS}",
  #     x="", y="Flow (cfs)") +
  #   scale_y_continuous(labels = scales::comma)+
  #   theme_classic()
  # print(p1)
  
  ## SAVE OUT CSV -------------------------------------------
  
  # write out
  readr::write_csv(clean_df, glue("data_clean/clean_fnf_daily_dwr_{siteID}_cdec.csv"))
  
  # print message!
  print(glue("Data saved here: 'data_clean/clean_fnf_daily_dwr_{siteID}_cdec.csv'"))
  
  return(clean_df)

}
