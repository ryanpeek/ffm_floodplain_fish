library(dplyr)
library(tidyr)
library(forcats)
library(readr)
library(vroom)
library(glue)
library(janitor)
library(wateRshedTools)

# import CDEC water year type for sac and san joaquin indices:
# https://cdec.water.ca.gov/reportapp/javareports?name=WSIHIST
# and TNC wyt, based on terciles, and filter to specific comid

f_import_water_year_types <- function(comids=c(2823750L)){
  
  # data for TNC wyt is a zipped file that is ~80MB, goes to 2015
  df_tnc <- vroom::vroom("data_raw/WYT_ALL_COMIDS.csv.zip", show_col_types = FALSE)
  df_tnc <- df_tnc %>% select(COMID:WYT)
  
  df_tnc_filt <- df_tnc %>% 
    dplyr::filter(COMID %in% comids) %>% 
    janitor::clean_names() %>% 
    dplyr::select(comid, year, mean_annual_flow, count_wateryears, wyt_tnc=wyt)

  # get CDEC wyt
  df_cdec <- wateRshedTools::ca_wytypes %>%
    janitor::clean_names() %>% 
    select(wy, wyt_sac_cdec=sv_w_ytype, wyt_sj_cdec=sj_w_ytype) %>% 
    mutate(wyt_sj_cdec = ifelse(is.na(wyt_sj_cdec), "C", wyt_sj_cdec),
           wyt_sac_cdec = ifelse(is.na(wyt_sac_cdec), "C", wyt_sac_cdec))
  
  dat_out <- full_join(df_tnc_filt, df_cdec, by=c("year"="wy")) %>% 
    mutate(across(c(wyt_sj_cdec, wyt_sac_cdec), 
                  ~forcats::fct_relevel(.x, c("W","AN","BN", "D","C")))) %>%
    mutate(wyt_tnc = forcats::fct_relevel(wyt_tnc, c("Wet","Moderate","Dry"))) %>% 
    tidyr::fill(comid, .direction = "down")
  
  # write out:
  write_csv(dat_out, file = glue("data_clean/wyt_cdec_tnc_{first(comids)}.csv"))
  write_rds(dat_out, file = glue("data_clean/wyt_cdec_tnc_{first(comids)}.rds"))
  return(dat_out)
}
