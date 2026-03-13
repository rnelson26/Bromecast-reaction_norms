########## Updated Kinship ##########
####### Michael's Landscape Genomics Model ########
####### code created 12-23-25 by R. Nelson ############
######### Last updated: 12-23-25 ###################
####### for methods to generate K see: Schwob et al.: https://arxiv.org/abs/2512.19035 #############

## load new kinship information from Michael's landscape genomics model:
load("/Users/Becca/Desktop/Adler Lab/Bromecast-reaction_norms/kinship_for_Becca.RData") 

## file metadata from Michael:
#K_mean_Becca and K_median_Becca have the posterior mean and median kinship predictions
#K_lo_Becca and K_hi_Becca have the lower and upper bounds for 95% credible intervals (in case we want to quantify uncertainty at a later time)
#the first 1:n rows and columns are the observed genotypes and the last 43 rows and columns are the unobserved genotypes, so this is the 129x129 kinship matrix
### unobserved genotypes are listed in the kinship matrix by lat and long

### to add these to the our reaction norm model we would update  n_g = 129 and 
#K = K_mean_Becca or K_median_becca and also check that the order of sites in the kinship matrix matches are current workflow for the reaction norm models. We could also propagate the uncertainty from these new kinship predictions into our reaction norm model, but this would require additional modifications to our model structure.

