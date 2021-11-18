# Download data from DWR FNF

library(glue)
#library(dataRetrieval)
library(dplyr)
library(ggplot2)
library(janitor)

# pull data: defaults to Verona daily discharge
f_get_dwr_fnf <- function(siteID) {
  
  # get data:
  print("Downloading data...")
  
  flowdat <- read_csv('data_raw/TLG_2005_2020.csv',
                      show_col_types = FALSE,
                      col_select=c('OBS DATE', 'VALUE', 'DATA_FLAG')) %>%
    rename(date=1, flow=2) # rename columns

  print("Data downloaded!")
  
  # make YMD date columns
  WY2005to2020_FNF$date <- lubridate::ymd(WY2005to2020_FNF$date)
  
  # check class
  class(WY2005to2020_FNF$date) # check class (should be Date)
  class(WY2005to2020_FNF$flow) # check class (should be numeric)
  
  # make clean data version and replace NAs with zero to plot
  clean_df <- WY2005to2020_FNF %>%
    mutate(flow_rev = ifelse(is.na(flow), 0, flow))
  
  summary(clean_df)
  
  (g1 <-ggplot() + geom_col(data=clean_df, aes(x=date, y=flow_rev), color=ifelse(clean_df$flow_rev>0, "blue", "red")))
  
  # interactive plot with plotly
  plotly::ggplotly(g1)
  
  # Interpolate Values ------------------------------------------------------
  
  # see website for more info on imputeTS:
  # http://steffenmoritz.github.io/imputeTS/
  
  clean_df <- clean_df %>%
    # first fill all negatives with NA then interpolate
    mutate(flow_na = ifelse(flow<0, NA_integer_, flow),
           # now interpolate only na's
           flow_kalman = imputeTS::na_kalman(flow_na), # structural time series model
           flow_ma7 = imputeTS::na_ma(flow_na, k = 7, weighting = "exponential")) # mean average over 7 days
  
  summary(clean_df)
  
  # plot the nas and interpolated versions
  imputeTS::ggplot_na_distribution(clean_df$flow_na)
  imputeTS::ggplot_na_distribution(clean_df$flow_kalman)
  
  # update plot
  (g1 <-ggplot() + geom_col(data=clean_df, aes(x=date, y=flow_rev), color=ifelse(clean_df$flow_rev>0, "blue", "red")) +
      geom_point(data=clean_df, aes(x=date, y=flow_kalman), fill="gray", pch=21) +
      geom_point(data=clean_df, aes(x=date, y=flow_ma7), fill="cyan4", pch=22))
  

  # write out
  readr::write_csv(flowdat, glue("data_raw/raw_dwr_fnf_{siteID}.csv"))
  
  # print message!
  print(glue("Data saved here: 'data_raw/raw_dwr_fnf_{siteID}.csv'"))
  
  # quick plot
  p1 <- ggplot(flowdat) +
    geom_line(aes(x=date, y=flow)) +
    labs(title=glue("DWR Full Natural Flow for {siteID}: {min(flowdat$water_year)}-{max(flowdat$water_year)}"))
  
  return(p1)
  
}
