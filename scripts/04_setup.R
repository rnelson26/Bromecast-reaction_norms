################# Bromecast: 00.Setup ##########################
############# created 3-25-25 ######################
############# Last modified: 3-17-26 ##########################
######## Loads packages & data files ################################

#rm(list = ls())

###### Load packages #####
library(tidyverse)
library(bayesplot)
library(cmdstanr)
library(reshape2)
library(FactoMineR)   
library(factoextra)
library(verification)
library(VGAM)
library(scoringRules)
library(hypergeo)
library(posterior)
library(RColorBrewer)
library(reshape2)
library(patchwork)
library(scoringRules)
library(ggpointdensity)
library(flextable)
library(officer)
library(readr)

#library(ggplot2) #if you don't want to load the whole tidyverse
#library(dplyr)

##### Load Data #########

data <- read.csv("data/combined_clean_climate_SOS_updated.csv", header = TRUE)

kinshipIDs <- read.csv("data/common_gardens/93cg_genotypes.csv")

kinship <- read.table("data/BRTE307_IBSmatrix.txt", sep = ",")

#assigned_genotypes <- read.csv("assigned_genotypes.csv")

tips <- read.csv("data/307tips.csv")

cg_WC <- read.csv("data/common_gardens/dailyVWCdata_allgardens_allyears.csv")

cg_temp <- read.csv("data/common_gardens/dailytempdata_allgardens_allyears.csv")

BRTE <- read.csv("data/BRTE_NorthAmerica.csv", header = TRUE)

soil_clean <- read.csv("data/sat_sites/soil_clean.csv")
