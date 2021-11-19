library(dplyr)
library(readr)
library(vroom)
library(glue)

df <- vroom::vroom("data_raw/WYT_ALL_COMIDS.csv.zip")

df <- df %>% select(COMID:WYT)

# using this COMID:
comid_tlg <- 2823750

df_filt <- df %>% filter(COMID == comid_tlg) %>% 
  janitor::clean_names() %>% 
  select(comid, year, mean_annual_flow, count_wateryears, wyt)

# write out:
write_csv(df_filt, file = glue("data_clean/tnc_wyt_{comid_tlg}.csv"))
