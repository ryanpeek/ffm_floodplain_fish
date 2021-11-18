library(tidyverse)
library(ffcAPIClient)
library(lubridate)
library(plotly)
library(imputeTS)

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

gageCOMID <- ffcAPIClient::get_comid_for_lon_lat(longitude = -120.441000,
                                                 latitude = 	37.666000) # Lat-Long obtained from CDEC TLG site
COMID <- 2823750L #obtained from gageCOMID

# Import data -------------------------------------------------------------

source("scripts/functions/f_get_dwr_fnf.R")
# run function
clean_df <- f_get_dwr_fnf()

# Run FFC -----------------------------------------------------------------

ffc<- FFCProcessor$new()

# make clean dataframe
clean_df_ffc <- clean_df %>%
  select(flow=flow_kalman, date) %>% as.data.frame()
class(clean_df_ffc)

ffc$flow_field = "flow" # name of column in your dataframe
ffc$date_field = "date" # name of column in your dataframe
ffc$date_format_string <- "%Y-%m-%d" # match format of your date
ffc$set_up(timeseries = clean_df_ffc, token = ffctoken, comid=COMID) # gives error if not dataframe

# run step one first
ffc$step_one_functional_flow_results(timeseries = clean_df_ffc,
                                     comid=COMID,
                                     token = ffctoken,
                                     output_folder = "~/Downloads/ffc_sj_tuo")

# change sci notation to make things readable
options(scipen = 999)


# Look at Results ---------------------------------------------------------

ffc$ffc_percentiles %>% View()
ffc$ffc_results %>% View()
ffc$predicted_percentiles %>% View()
ffc$doh_data # dimensionless hydroraph

# run additional steps if you want...but step one gives pretty much everything
ffc$step_two_explore_ecological_flow_criteria()
ffc$step_three_assess_alteration()

#  this breaks for some reason...still troubleshooting error
# ffcAPIClient::evaluate_alteration(
#   timeseries_df = clean_df_ffc,
#   date_format_string = "%Y-%m-%d",return_processor = TRUE,
#   token = ffctoken,
#   plot_output_folder = "~/Downloads/tst_api_ffc",
#   comid=COMID)

# see error below
#INFO [2021-11-10 16:09:04] ffcAPIClient Version 0.9.8.2
#Error in `[<-.data.frame`(`*tmp*`, timeseries$calendar_month < 10, "water_year",  :
#                            missing values are not allowed in subscripted assignments of data #frames


