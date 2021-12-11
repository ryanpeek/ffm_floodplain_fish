# get FNF for DWR site

# load functions
source("scripts/functions/f_dwr_01_download_fnf.R")
source("scripts/functions/f_dwr_02_clean_fnf.R")
source("scripts/functions/f_dwr_03_load_fnf.R")


# run functions
f_dwr_01_download_fnf()
f_dwr_02_clean_fnf() # warning is ok
f_dwr_03_load_fnf()
