######## SMAP Data Extraction ##############################################
##### get soil moisture info from SMAP #########
######## for bromecast reaction norm paper ########
######## R. Nelson, M. Vahsen, & P. Adler ######
########### code created on 2/6/25 #######
############ last modified: 2/6/25 ########################

#### load packages #####
library(smapr)
library(terra)

#learn more here:
#https://docs.ropensci.org/smapr/
  

###### to use smap we need a NASA Earth data account which links to Rstudio #######

## See the instructions in the BromeCast Google drive (in manuscripts -> reaction norm model folder) for how to do this. I am avoiding putting this info directly in our public github repo to comply with NASA's data security guidelines....  

## still waiting for NASA to approve the account for data access...

##### get the relevant SMAP dataset ####
available_data <- find_smap(id = "SPL3SMAP", date = "2015-05-25", version = 3)
str(available_data)

downloads <- download_smap(available_data)

list_smap(downloads, all = FALSE)

sm_raster <- extract_smap(downloads, "Soil_Moisture_Retrieval_Data/soil_moisture")
plot(sm_raster, main = "Level 3 soil moisture: May 25, 2015")
writeRaster(sm_raster, "sm_raster.tif")
