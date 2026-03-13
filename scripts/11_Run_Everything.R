################# Bromecast: 11_Run_Everthing ##########################
############# created 9-22-25 ######################
############# Last modified: 9-22-25 ##########################
######## Runs full workflow ################################

### Process data 
source("scripts/00_Merge.R")
source("scripts/01_soils.R")
source("scripts/02_Coefs.R")
source("scripts/03_landscape_genomics.R")

### fit and analyze models
source("scripts/04_setup.R")
source("scripts/05_prepare_data.R")
source("scripts/06_prepare_standata_emg.R")
source("scripts/07_prepare_standata_rep.R")
source("scripts/08_prepare_standata_fec.R")
source("scripts/09_Fit_Models.R")  ## takes a long time to run 
source("09.1_run_single_model.R") ##wrapper script for High Performance Computer 
source("scripts/10_CRPS_and_figures.R")


