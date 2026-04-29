######### Landscape Genomics with Michael's Model ##############
########### Bromecast Reaction Norms ############
######## Create K and assign genotypes ##########
######## code Becca Nelson ###############
############ created 4-3-26 #############
############ last modified 4-9-26 ##########################

######### Load packages ##########
library(tidyverse)

###### Load files ############

 
K_Michael_PD <- read.csv("data/LG_informed_kinship_PD.csv", header = TRUE)

K_Michael_nonPD <- read.csv("data/LG_informed_kinship.csv", header = TRUE)

##### PD version for model #####
K_Michael_PD$X <- NULL

####### non PD version ######

#rownames(K_Michael) <- make.unique(K_Michael$X)
#K_Michael$X <- NULL

#info_Michael <- read.csv("data/LG_ordered_climate.csv", header = TRUE)

## remove extra Symstads since CG symstad and sat symstad genotypes have the same values now
# find columns with .1 suffix
#remove_ids <- c("Symstad1.1", "Symstad2.1")
#K_Michael <- K_Michael[, !colnames(K_Michael) %in% remove_ids]
#dup_rows <- duplicated(K_Michael$X)
#K_Michael <- K_Michael[!dup_rows, ]


#rownames(K_Michael) <- K_Michael$X
#K_Michael$X <- NULL

#dim(K_Michael)
## 119 x 119

#min(eigen(K_Michael)$values)
#[1] -0.852567

#eig <- eigen(K_Michael)

#neg_idx <- which(eig$values < 0)
#neg_evecs <- eig$vectors[, neg_idx, drop=FALSE]


#df <- data.frame(
 # genotype = rownames(K_Michael),
  #contribution = neg_evecs[,1]
#)

#ggplot(df, aes(x = reorder(genotype, contribution), y = contribution)) +
 # geom_bar(stat="identity") +
  #coord_flip() +
  #ggtitle("Contribution to first negative eigenvalue")
