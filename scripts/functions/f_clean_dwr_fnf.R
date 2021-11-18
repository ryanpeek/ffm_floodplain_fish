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
    rename(flow=value) %>% 
    # replace NAs with -1 to plot
    mutate(flow_rev = ifelse(is.na(flow), -1, flow))
  
  # visualize the negatives
  g1 <- ggplot() + 
    geom_col(data=flowdat, aes(x=date, y=flow_rev), 
             color=ifelse(flowdat$flow_rev>0, "cyan4", "red3")) +
    labs(title=glue("DWR Full Natural Flow for {siteID}: Raw Data"),
         caption="Negative values are in red",
         x="", y="Flow (cfs)") +
    theme_classic() +
    scale_y_continuous(labels = scales::comma)
  print(g1)
  
  # Interpolate Values ------------------------------------------------------
  
  # use imputeTS: http://steffenmoritz.github.io/imputeTS/
  
  clean_df <- flowdat %>%
    # first fill all negatives with NA then interpolate
    mutate(flow_na = ifelse(flow<0, NA_integer_, flow),
           # now interpolate only na's
           ## structural time series model (Kalman)
           flow_kalman = imputeTS::na_kalman(flow_na), 
           # mean average over 7 days
           flow_ma7 = imputeTS::na_ma(flow_na, k = 7, weighting = "exponential"))
  
  ## VISUALIZE --------------------------------------------------------
  
  # visualize the nas and interpolated versions
  #imputeTS::ggplot_na_distribution(clean_df$flow_na)
  #imputeTS::ggplot_na_distribution(clean_df$flow_kalman)
  
  # write out
  readr::write_csv(clean_df, glue("data_clean/clean_fnf_daily_dwr_{siteID}_cdec.csv"))
  
  # print message!
  print(glue("Data saved here: 'data_clean/clean_fnf_daily_dwr_{siteID}_cdec.csv'"))
  
  # quick plot
  p1 <- ggplot(clean_df) +
    geom_line(aes(x=date, y=flow_kalman)) +
    labs(
      title=glue("DWR Full Natural Flow for {siteID}: Interpolated {min(year(clean_df$date))}-{max(year(clean_df$date))}"),
      caption="Negative values interpolated with Kalman structural time series {imputeTS}",
      x="", y="Flow (cfs)") +
    scale_y_continuous(labels = scales::comma)+
    theme_classic()
  print(p1)
  
  return(clean_df)

}
