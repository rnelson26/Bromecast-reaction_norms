######## SMAP Data Extraction ##############################################
##### get soil moisture info from SMAP #########
######## for bromecast reaction norm paper ########
######## R. Nelson, M. Vahsen, & P. Adler ######
########### code created on 2/6/25 #######
############ last modified: 2/11/25 ########################
#https://github.com/ropensci/smapr/issues/81

#### load packages #####
library(terra)
library(httr)
library(jsonlite)
library(rvest)
library(smapr)

# Install rvest 0.3.2
devtools::install_version("rvest", version = "0.3.2")
#https://github.com/ropensci/smapr/issues/33
# doesn't fix issue

packageVersion("rvest")
available_data <- find_smap(id = "SPL3SMP", date = "2017-01-01", version = 4)
#learn more here:
#https://docs.ropensci.org/smapr/
  

###### to use smap we need a NASA Earth data account which links to Rstudio #######

## See the instructions in the BromeCast Google drive (in manuscripts -> reaction norm model folder) for how to do this. I am avoiding putting this info directly in our public github repo to comply with NASA's data security guidelines....  

## still waiting for NASA to approve the account for data access...

##### get the relevant SMAP dataset ####
  # Define timeframe of interest 
  start_date <- as.Date("2021-01-01")
  end_date <- as.Date("2024-12-31")
  # Generate a sequence of dates
  date_sequence <- seq(start_date, end_date, by = "day")
  # Query SMAP data
  available_data <- find_smap(id = "SPL3SMA", dates = date_sequence, version = 3)
  

str(available_data)

downloads <- download_smap(available_data)

list_smap(downloads, all = FALSE)

sm_raster <- extract_smap(downloads, "Soil_Moisture_Retrieval_Data/soil_moisture")
plot(sm_raster, main = "Level 3 soil moisture: May 25, 2015")
writeRaster(sm_raster, "sm_raster.tif")
