###### Compare PCA Spaces  ########
compare_pca <- function(L1, L2) {
  k <- ncol(L1)
  out <- matrix(NA, k, k)
  
  for (i in 1:k) {
    for (j in 1:k) {
      out[i, j] <- abs(cor(L1[, i], L2[, j]))
    }
  }
  out
}

subspace_similarity <- function(L1, L2) {
  sv <- svd(t(L1) %*% L2)$d
  mean(sv)
}
#Subspace similarity measures how much two PCA solutions span the same multidimensional ecological space, independent of rotation, axis ordering, or sign.

####### Climate ##############
##Lambda_SOS: fecundity
## Lambda_rep_SOS: reproduced
## Lambda_emg_SOS: emerged

emg_rep <- compare_pca(Lambda_SOS , Lambda_emg_SOS)
       #[,1]       [,2]
#[1,] 0.9917714 0.09755767
#[2,] 0.2333975 0.97269889
#The emergence PCA and reproduction PCA are capturing essentially the same 2-dimensional environmental structure.

emg_fec <- compare_pca(Lambda_rep_SOS, Lambda_emg_SOS)
#[,1]       [,2]
#[1,] 0.99981382 0.03148892
#[2,] 0.02619247 0.99997307
#The emergence PCA and fecundity PCA are capturing essentially the same 2-dimensional environmental structure.

fec_rep <- compare_pca(Lambda_rep_SOS, Lambda_SOS)
#[,1]      [,2]
#[1,] 0.99130387 0.2379224
#[2,] 0.09889754 0.9720754
#The emergence PCA and fecundity PCA are capturing essentially the same 2-dimensional environmental structure.

subspace_similarity(Lambda_rep_SOS, Lambda_emg_SOS) #0.999897
subspace_similarity(Lambda_SOS, Lambda_emg_SOS) #0.9976246
subspace_similarity(Lambda_rep_SOS, Lambda_SOS) #0.9975809
## almost identical ecological spaces since above 0.9

####### Soil ########
emg_rep <- compare_pca(Lambda_soil , Lambda_soil_emg)
#[,1]       [,2]
#[1,] 0.9991369 0.1170591
#[2,] 0.1851664 0.9992147
#The emergence PCA and reproduction PCA are capturing essentially the same 2-dimensional environmental structure.

emg_fec <- compare_pca(Lambda_soil_rep, Lambda_soil_emg)
#[,1]       [,2]
#[1,] 0.9999181 0.1463217
#[2,] 0.1594050 0.9999073
#The emergence PCA and fecundity PCA are capturing essentially the same 2-dimensional environmental structure.

fec_rep <- compare_pca(Lambda_soil_rep, Lambda_soil)
#[,1]      [,2]
#[1,] 0.9993458 0.1798037
#[2,] 0.1247867 0.9994852
#The emergence PCA and fecundity PCA are capturing essentially the same 2-dimensional environmental structure.

subspace_similarity(Lambda_soil_rep, Lambda_soil_emg) # 0.9999361
subspace_similarity(Lambda_soil, Lambda_soil_emg) #0.9996645
subspace_similarity(Lambda_soil_rep, Lambda_soil) #0.9996859
## almost identical ecological spaces since above 0.9

#For all life stages, climate and soil PCA constructions recover a nearly identical low-dimensional environmental manifold (environmental PCA space). Individual loadings and biologically interpretations of axes could still vary though since subspace doesn't measure this. 

####### Do projections differ by site-year or site? #######
## climate:
W_emg_shared <- scale(pca_data_emg_SOS %>% select(-site_year)) %*% Lambda_emg_SOS
W_rep_projected <- scale(pca_data_rep_SOS %>% select(-site_year)) %*% Lambda_emg_SOS
W_fec_projected <- scale(pca_data_SOS %>% select(-site_year)) %*% Lambda_emg_SOS

W_rep_native <- scale(pca_data_rep_SOS %>% select(-site_year)) %*% Lambda_rep_SOS
W_fec_native <- scale(pca_data_SOS %>% select(-site_year)) %*% Lambda_SOS


cor(W_rep_projected[,1], W_rep_native[,1]) # 0.9999716
cor(W_fec_projected[,1], W_fec_native[,1]) #0.9918563

cor(W_rep_projected[,2], W_rep_native[,2]) # 0.9999948
cor(W_fec_projected[,2], W_fec_native[,2]) #0.9793954

rank_correlation <- function(W1, W2) {
  cor(W1[,1], W2[,1], method = "spearman")
}
rank_correlation(W_rep_projected, W_rep_native) #0.9997388
rank_correlation(W_fec_projected, W_fec_native) #0.9842784

## soil:
W_emg_shared <- scale(soil_data_emg %>% select(-site)) %*% Lambda_soil_emg
W_rep_projected <- scale(soil_data_rep %>% select(-site)) %*% Lambda_soil_emg
W_fec_projected <- scale(soil_data %>% select(-site)) %*% Lambda_soil_emg

W_rep_native <- scale(soil_data_rep %>% select(-site)) %*% Lambda_soil_rep
W_fec_native <- scale(soil_data %>% select(-site)) %*% Lambda_soil


cor(W_rep_projected[,1], W_rep_native[,1]) # 0.9999633
cor(W_fec_projected[,1], W_fec_native[,1]) #0.9996803

cor(W_rep_projected[,2], W_rep_native[,2]) # 0.9999279
cor(W_fec_projected[,2], W_fec_native[,2]) #0.9991514

rank_correlation <- function(W1, W2) {
  cor(W1[,1], W2[,1], method = "spearman")
}
rank_correlation(W_rep_projected, W_rep_native) #0.9998842
rank_correlation(W_fec_projected, W_fec_native) #0.998147


#We could use a shared latent environmental representation because empirical PCA #structure is invariant across demographic outcomes and site-year or site subsets for climate and soil 

