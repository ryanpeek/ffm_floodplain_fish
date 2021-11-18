library(tidyverse)
library(lubridate)
library(ffcAPIClient)
library(glue)
# change sci notation to make things readable
options(scipen = 999)


# Save your Token to REnviron ---------------------------------------------

# save token in your Renviron with usethis::
# library(usethis)
# run this, then paste your code in as EFLOWS_TOKEN="yourlongtokenid"
# usethis::edit_r_environ()
# then save and close, restart R session and below should work.

# Set up Processor --------------------------------------------------------

# set/get the token for using the FFC
ffctoken <- set_token(Sys.getenv("EFLOWS_TOKEN", ""))

ffcAPIClient::clean_account(ffctoken)

# Lat-Long obtained from CDEC TLG site
# gageCOMID <- ffcAPIClient::get_comid_for_lon_lat(longitude = -120.441000,
#                                                  latitude = 	37.666000) 

COMID <- 2823750L # TLG CDEC

# Import data -------------------------------------------------------------

source("scripts/functions/f_clean_dwr_fnf.R")
# run function
clean_df <- f_clean_dwr_fnf()

# Run FFC -----------------------------------------------------------------

ffc <- FFCProcessor$new()

# make clean dataframe
clean_df_ffc <- clean_df %>%
  select(flow=flow_kalman, date) %>% as.data.frame()
class(clean_df_ffc) # make sure it's "data.frame

ffc$flow_field = "flow" # name of column in your dataframe
ffc$date_field = "date" # name of column in your dataframe
ffc$date_format_string <- "%Y-%m-%d" # match format of your date

# run setup: # gives error if not dataframe, and will drop years with out data
ffc$set_up(timeseries = clean_df_ffc, token = ffctoken, comid=COMID) 

# run step one first
ffc$step_one_functional_flow_results(timeseries = clean_df_ffc,
                                     comid=COMID,
                                     token = ffctoken,
                                     output_folder = "output/ffc_TLG")

# Look at Results ---------------------------------------------------------

ffc$ffc_percentiles %>% View()
ffc$ffc_results %>% View()
write_csv(ffc$predicted_percentiles, file = glue("output/ffc_TLG/{COMID}_ffc_predicted_percentiles.csv"))

# get year range:
yr_range <- unlist(ffc$raw_ffc_results$yearRanges)

# what other stuff (SD, CV, avgAnnFlow)
ann_metrics <- map_df(ffc$raw_ffc_results$allYear, ~unlist(.x)) %>% 
  bind_cols(., "year"=yr_range) %>% 
  rename(ann_sd = 1, ann_mean_flow_cfs = 2, ann_cv = 3)

# write out 
write_csv(ann_metrics, file = glue("output/ffc_TLG/{COMID}_ffc_ann_metrics.csv"))
