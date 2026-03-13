
######### Landscape Genomics with GLMNET ##############
########### Bromecast Reaction Norms ############
######## Create K and assign genotypes ##########
######## code by Justin Van Ee and Becca Nelson ###############
############ created 8-19-25 #############
############ last modified 10-27-25 ##########################

### use row and column names that correspond to actual genotypes in kinship matrix

## row.names  <- (c") and colnames

## at end of landscape genomics code go ahead and assign row vs column and then remove seed source sites

## then input a K where the seed source sites not common garden are filtered out 

## keeps <- m(c, a)

#mat <- matrix(1:16, nrow = 4, ncol = 4)
#mat
#row.names(mat) <- c("a", "b", "c", "d")
#colnames(mat) <- c("a", "b", "c", "d")
#keeps <- c("a", "b")
#mat[keeps, keeps ]


## throughout the code "SNPS" refers to each individual genotype

rm(list = ls())

### Load Packages ################
library(rwc) 
library(ape)
library(MASS) 
library(tidyverse)
library(poppr)
library(pegas)
library(sf)
library(RcppCNPy)
library(ggridges)
library(nlme)
library(statgenGWAS)
library(glmnet)
library(purrr)
library(dplyr)
library(Matrix)
library(stringr)

######## Part 1: PC Exp Decay Method with known source sites ###########
######## Load data ###################
## seed source daymet (all WNA seed source sites except for 4 that were too far north in BC for daymet)
clim <- read.csv("data/seed_climate_info.csv", header = TRUE)
sat_clim_raw <- read.csv("data/sat_climate_info.csv", header = TRUE)


# do to level of site
sat_clim <- sat_clim_raw %>%
  distinct(lat, lon, .keep_all = TRUE) %>%       # one row per location
  mutate(
    site_simple = str_remove(site_code, "\\s\\d{4}$")  # remove space + 4-digit year at end
  )




### Get genotype key matrix for connecting with genotype matrix 
genotype_codes <- read.csv("https://raw.githubusercontent.com/pbadler/bromecast-data/main/traits/data/rawdata/gamba_growthchamber/BRTEcg_genotypesCode.csv", header = TRUE) %>%
  arrange(genotype) %>%
  filter(!is.na(SNPmatrix_column))  # only for common garden

### assign genotypes to full list of genotypes for WNA, using existing genotype codes where appropriate:
clim_with_geno <- clim %>%
  left_join(genotype_codes %>% dplyr::select(NewSiteCode, genotype), by = "NewSiteCode")

missing_idx <- which(is.na(clim_with_geno$genotype))

if (length(missing_idx) > 0) {
  max_existing <- max(genotype_codes$genotype, na.rm = TRUE)
  new_genos <- seq(from = max_existing + 1, length.out = length(missing_idx))
  
  clim_with_geno$genotype[missing_idx] <- new_genos
}

clim_with_geno$genotype <- as.integer(clim_with_geno$genotype)

### Get number of genotypes
n_g <- nrow(clim_with_geno)

### Connect to genotype/SNP matrix
SNPs <- as.data.frame(read.table("data/BRTE127_LDfilteredSNPs.bed", header = FALSE, sep = ",", stringsAsFactors = FALSE))  ## all western North American genotypes

## make a new column for order of genotypes in the snp matrix (.bed file)
clim_with_geno <- clim_with_geno %>%
  mutate(SNPmatrix_column = ibs_id + 3)

SNPs <- SNPs[, c(1:3, clim_with_geno$SNPmatrix_column)] ## columns 1:3 = line name, reference, alt allele

## Calculate principal components of genotype matrix 
PC_out <- prcomp(t(SNPs[,-c(1:3)]))

# Calculate proportion of variance explained
var_explained <- PC_out$sdev^2 / sum(PC_out$sdev^2)

# Create elbow plot (visual check)
plot(var_explained, type = "b", 
     xlab = "Principal Component", 
     ylab = "Proportion of Variance Explained",
     main = "Elbow Plot of Principal Components")

# Select number of PCs
n_pc <- 60  # 99% of variance explained
PCs <- PC_out$x[, 1:n_pc]

# Augment dataset (PCs + environmental predictors)
data <- cbind(PCs, clim_with_geno)

# Standardize predictors if needed
data <- data %>%
  mutate(across(c(Latitude, Longitude, starts_with("bioclim_")), scale))

# Predictor variables for glmnet
predictor_vars_LM <- c("Latitude", "Longitude", paste0("bioclim_", 1:19))  

# Predictor and response matrices
X <- as.matrix(data[, predictor_vars_LM])        # predictors: lat/lon coords + bioclim
Y <- as.matrix(data[, paste0("PC", 1:n_pc)])     # response PCs

###
### Fit glmnet models for each PC (all data)
### and store them for prediction on satellite sites
###
glmnet_models <- purrr::map(1:n_pc, function(l) {
  y <- Y[, l]
  cv_fit <- cv.glmnet(X, y, alpha = 0)  # Ridge regression, switch to 1 for lasso, ridge seemed slightly better at reducing rmse
  return(cv_fit)                        # Keep the fitted model for prediction
})

###
### Predict PCs for satellite sites
###

# Standardize satellite predictors the same as training data
colnames(sat_clim)[colnames(sat_clim) == "lat"] <- "Latitude"
colnames(sat_clim)[colnames(sat_clim) == "lon"] <- "Longitude"

sat_X <- sat_clim %>%
  mutate(across(c(Latitude, Longitude, starts_with("bioclim_")), scale)) %>%
  dplyr::select(all_of(predictor_vars_LM)) %>%
  as.matrix()

# Predict PCs
predPC_list <- purrr::map(glmnet_models, function(model) {
  predict(model, newx = sat_X, s = "lambda.min") %>% as.vector()
})

# Combine into matrix and assign synthetic genotype IDs
PCs_new <- do.call(cbind, predPC_list)
colnames(PCs_new) <- paste0("PC", 1:n_pc)
n_new <- nrow(PCs_new)
new_ids <- paste0("G", 200:(199 + n_new))
rownames(PCs_new) <- new_ids

########## Part 2: Assign synthetic genotypes for satellite sites ######################
# Combine observed PCs and new PCs
rownames(PCs) <- as.character(clim_with_geno$genotype)
PCs_all <- rbind(PCs, PCs_new)

#  Compute pairwise Euclidean distances in PC space
D_all <- as.matrix(dist(PCs_all, method = "euclidean"))


####  make sure kinship and PC row/col names correspond to genotype IDs,
#### then remove seed-source genotypes not used in the common garden (keeps)

# ---------- compute IBS kinship from SNPs (original genotypes in SNP matrix) ----------
# The SNPs data frame has columns:
#   1:3 metadata, and then one column per genotype in the SNPmatrix order.
# We already constructed SNPs <- SNPs[, c(1:3, clim_with_geno$SNPmatrix_column)]

# create genotype id vector matching the SNP columns (the same order as columns 4:ncol(SNPs))
geno_ids_in_snp_cols <- as.character(clim_with_geno$genotype)    # order should match clim_with_geno$SNPmatrix_column
stopifnot(length(geno_ids_in_snp_cols) == (ncol(SNPs) - 3))

# compute kinship on SNP matrix (transpose so rows = individuals)
K_raw <- SNPs[ , -c(1:3) ] %>% t() %>% kinship(method = "IBS", MAF = 0.10) %>% cov2cor()

# assign row/col names so K_raw's dimensions are labelled by the genotype IDs
rownames(K_raw) <- colnames(K_raw) <- geno_ids_in_snp_cols

# ---------- assign rownames for the original PCs (observed genotypes) ----------
# You already set rownames(PCs) <- as.character(clim_with_geno$genotype) earlier; ensure it matches:
rownames(PCs) <- as.character(clim_with_geno$genotype)
stopifnot(all(rownames(PCs) == rownames(K_raw)))  # sanity: same set & order for observed genotypes

# ---------- fit exponential decay model in PC distance vs log(kinship) ----------
D_obs <- as.matrix(dist(PCs, method = "euclidean"))    # pairwise distances among observed genotypes
# ensure D_obs has same row/col names
rownames(D_obs) <- colnames(D_obs) <- rownames(PCs)

# vectorize and fit
log_kinship <- c(log(K_raw))
distance    <- c(D_obs)
opt_range <- lm(log_kinship ~ distance + I(distance^2) - 1)

# ---------- predict kinship for all genotypes (observed + synthetic) ----------
# Make sure PCs_all rownames are already set:
# earlier you created PCs_new with rownames new_ids
rownames(PCs)       <- as.character(clim_with_geno$genotype)  # observed
rownames(PCs_new)   <- new_ids                               # synthetic
PCs_all <- rbind(PCs, PCs_new)                               # all genotypes (observed then synthetic)
# ensure PCs_all rownames are unique
stopifnot(any(duplicated(rownames(PCs_all))) == FALSE)

# compute pairwise distances for all genotypes
D_all <- as.matrix(dist(PCs_all, method = "euclidean"))
rownames(D_all) <- colnames(D_all) <- rownames(PCs_all)

# predict kinship for every pair using the fitted relationship (distance -> kinship)
K_new_raw <- matrix(
  exp(predict(opt_range, newdata = data.frame(distance = c(D_all)))),
  nrow = nrow(D_all),
  ncol = ncol(D_all),
  dimnames = list(rownames(D_all), colnames(D_all))
)

# ---------- ensure positive definiteness and retain names ----------
K_all <- Matrix::nearPD(K_new_raw)$mat %>% as.matrix()
rownames(K_all) <- colnames(K_all) <- rownames(D_all)  # preserve genotype IDs

# ---------- create genotype index (observed + synthetic) ----------
genotype_index_new <- tibble(
  site     = sat_clim$site,
  genotype = new_ids
)

genotype_index_all <- bind_rows(
  tibble(
    site = clim_with_geno$NewSiteCode,
    genotype = as.character(clim_with_geno$genotype)
  ),
  genotype_index_new
)

# ---------- identify genotypes to keep ----------

### ----------- KEEP ONLY COMMON GARDEN + SYNTHETIC GENOTYPES -----------

keeps_common_garden <- c(
  1, 2, 3, 5, 6, 7, 8, 9, 12, 13, 14, 16, 17, 18, 19, 20, 21, 24, 25, 26, 27, 28, 29, 30, 31, 32,
  33, 34, 35, 36, 37, 38, 39, 40, 42, 43, 44, 45, 46, 47, 48, 49, 51, 52, 53, 54, 55, 56, 57, 58,
  59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 79, 81, 82, 83, 84,
  86, 87, 88, 89, 90, 91, 92, 93, 95, 96, 97, 98, 99, 100, 102, 103, 104, 105, 150
)

# Convert to character so matches rownames in K_all (which are characters)
keeps_common_garden <- as.character(keeps_common_garden)

# Add synthetic genotype IDs (all the new ones generated for satellites)
keeps_synthetic <- new_ids  #synthetic genotypes always start with "G"

# Combine both
keeps <- c(keeps_common_garden, keeps_synthetic)

# Make sure the IDs actually exist in K_all
keeps <- intersect(keeps, rownames(K_all))
length(keeps)
cat("Number of genotypes retained in filtered kinship matrix:", length(keeps), "\n")

# ----------- Filter matrices and index objects -----------
K_all_filtered <- K_all[keeps, keeps, drop = FALSE]
PCs_all_filtered <- PCs_all[keeps, , drop = FALSE]

genotype_index_all_filtered <- genotype_index_all %>%
  filter(genotype %in% keeps) %>%
  arrange(match(genotype, keeps))

# Sanity checks
stopifnot(all(rownames(K_all_filtered) == colnames(K_all_filtered)))
stopifnot(all(rownames(K_all_filtered) == rownames(PCs_all_filtered)))
stopifnot(all(rownames(K_all_filtered) == genotype_index_all_filtered$genotype))

# ----------- Save filtered outputs -----------
save(
  K_all_filtered,
  genotype_index_all_filtered,
  PCs_all_filtered,
  file = "data/K_all_genotypes_filtered_common_garden_plus_satellites.RData"
)
write.csv(K_all_filtered, "data/K_all_genotypes_filtered_common_garden_plus_satellites.csv", row.names = TRUE)

cat("✅ Saved filtered kinship matrix and genotype index for common garden + synthetic genotypes.\n")







# ---------- small plot to compare distributions (optional, keep your existing plot code if you like) ----------
K_obs_off <- K_raw[upper.tri(K_raw)]
Knew_off_filtered <- K_all_filtered[upper.tri(K_all_filtered)]

df <- data.frame(
  value = c(K_obs_off, Knew_off_filtered),
  Method = rep(c("IBS (observed)", "PC - Exponential Decay (filtered)"),
               times = c(length(K_obs_off), length(Knew_off_filtered)))
)

ggplot(df, aes(x = value, fill = Method)) +
  geom_density(alpha = 0.5, position = "identity") +
  theme_bw() +
  labs(x = "Kinship", y = "Density", title = "Kinship distribution: observed vs predicted (filtered)")




########## Figures ####################
### geographic map
library(maps)

# Get US state boundaries
state_map <- map_data("state")

state_map_filtered <- state_map %>%
  filter(long >= -128 & long <= -95 & lat >= 30 & lat <= 52)

# Combine seed source and satellite locations
map_df <- bind_rows(
  clim_with_geno %>% distinct(Latitude, Longitude, .keep_all = TRUE) %>% mutate(Type = "Seed Source"),
  sat_clim %>% mutate(Type = "Satellite")
)

# Plot map
ggplot() +
  geom_polygon(data = state_map_filtered, aes(x = long, y = lat, group = group),
               fill = "gray90", color = "black") +
  geom_point(data = map_df, aes(x = Longitude, y = Latitude, color = Type),
             size = 3, alpha = 0.7) +
  scale_color_manual(values = c("Seed Source" = "blue", "Satellite" = "red")) +
  coord_cartesian(xlim = c(-128, -95), ylim = c(30, 52)) +
  theme_minimal() +
  labs(title = "Seed Source vs Satellite Locations in Western North America",
       x = "Longitude", y = "Latitude") +
  theme(legend.position = "bottom")

### bioclimatic heatmap
# Combine datasets and mark type
bio_df <- bind_rows(
  clim_with_geno %>% distinct(NewSiteCode, .keep_all = TRUE) %>%
    mutate(Type = "Seed Source") %>%
    dplyr::select(site_code = NewSiteCode, Type, starts_with("bioclim_")),
  sat_clim %>%
    mutate(Type = "Satellite") %>%
    dplyr::select(site_code = site_code, Type, starts_with("bioclim_"))
)

bio_long <- bio_df %>%
  pivot_longer(cols = starts_with("bioclim_"), names_to = "Bioclim", values_to = "Value")

# Heatmap
ggplot(bio_long, aes(x = Bioclim, y = site_code, fill = Value)) +
  geom_tile() +
  facet_wrap(~Type, scales = "free_y") +
  scale_fill_viridis_c(option = "C") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 6)) +
  labs(title = "Bioclimatic Variables: Seed Source vs Satellite",
       x = "Bioclim Variable",
       y = "Site",
       fill = "Value")

#### Geographic map + heatmap

map_df <- bind_rows(
  clim_with_geno %>% 
    distinct(Latitude, Longitude, .keep_all = TRUE) %>% 
    mutate(Type = "Seed Source"),
  sat_clim %>% 
    mutate(Type = "Satellite")
)

# List of bioclimatic variables
bioclim_vars <- paste0("bioclim_", 1:19)

# create one map per bioclim variable
bioclim_maps <- purrr::map(bioclim_vars, function(var) {
  ggplot() +
    geom_polygon(data = state_map_filtered, 
                 aes(x = long, y = lat, group = group),
                 fill = "gray90", color = "black") +
    geom_point(data = map_df,
               aes(x = Longitude, y = Latitude, shape = Type, color = .data[[var]]),
               size = 3, alpha = 0.8) +
    scale_shape_manual(values = c("Seed Source" = 16, "Satellite" = 17)) +
    scale_color_viridis_c(option = "plasma", na.value = "gray80") +
    coord_cartesian(xlim = c(-128, -95), ylim = c(30, 52)) +
    theme_minimal() +
    labs(title = paste("Bioclimatic variable:", var),
         x = "Longitude", y = "Latitude", color = var) +
    theme(legend.position = "bottom")
})

# preview the first map
bioclim_maps[[12]]
##key to what climate variables the numbers correspond to: https://www.worldclim.org/data/bioclim.html

#walk2(bioclim_maps, bioclim_vars, ~ ggsave(filename = paste0("maps/", .y, ".png"), plot = .x, width = 7, height = 5))

####### Save graphs as combined pdf:
# Open PDF
pdf("all_maps_combined.pdf", width = 8, height = 6)

# 1. Seed source vs satellite locations map
print(
  ggplot() +
    geom_polygon(data = state_map_filtered, aes(x = long, y = lat, group = group),
                 fill = "gray90", color = "black") +
    geom_point(data = map_df, aes(x = Longitude, y = Latitude, color = Type, shape = Type),
               size = 3, alpha = 0.8) +
    scale_color_manual(values = c("Seed Source" = "blue", "Satellite" = "red")) +
    scale_shape_manual(values = c("Seed Source" = 16, "Satellite" = 17)) +
    coord_cartesian(xlim = c(-128, -95), ylim = c(30, 52)) +
    theme_minimal() +
    labs(title = "Seed Source vs Satellite Locations in WNA",
         x = "Longitude", y = "Latitude", color = "Type", shape = "Type") +
    theme(legend.position = "bottom")
)

# 2. Bioclimatic maps (1 per variable)
for (i in seq_along(bioclim_maps)) {
  print(bioclim_maps[[i]])
}

# Close PDF
dev.off()


###### Sites and Kinship #######

K_df <- as.data.frame(K_all_filtered)
K_df$genotype <- rownames(K_all_filtered)
K_df <- K_df %>%
  left_join(genotype_index_all_filtered, by = "genotype")

ggplot(K_df, aes(x = site, y = G200)) +  # example: kinship to G200
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(y = "Kinship with G200", x = "Site")


genotype_table <- tibble(
  row_index = 1:nrow(K_all_filtered),                     # row/column number
  genotype  = rownames(K_all_filtered)                   # genotype ID
) %>%
  left_join(genotype_index_all_filtered, by = "genotype") # add site info


head(genotype_table)

#write.csv(genotype_table, "data/genotype_site_row_lookup.csv", row.names = FALSE)

## map of site and kinship 
library(tidyverse)
library(sf)
library(ggplot2)
library(viridis)


#write.csv(clim_with_geno,
 #         "data/climate_genotype_info.csv",
  #        row.names = FALSE)

lookup <- read.csv("data/genotype_site_row_lookup.csv")  # row_index, genotype, site


GPS_sites <- read.csv("data/gps_sites.csv")  # columns: site, Latitude, Longitude

# Satellite site coordinates
sat_sites <- read.csv("data/site_list.csv")  # columns: site, Latitude, Longitude



#sat_sites <- sat_sites %>%
 # rename(site = ID,
  #       Latitude = Latitude,
   #      Longitude = Longitude) %>%
  #dplyr::select(site, Latitude, Longitude)


#GPS_sites <- GPS_sites %>%
 # rename(Latitude = lat,
  #       Longitude = lon) %>%
  #dplyr::select(site, Latitude, Longitude)


#all_sites <- bind_rows(GPS_sites, sat_sites)



#K <- K_all_filtered

#site_pairs <- expand.grid(site1 = unique(lookup$site),
 #                         site2 = unique(lookup$site),
  #                        stringsAsFactors = FALSE) %>%
  #rowwise() %>%
  #mutate(
   # kinship_avg = mean(
    #  K[lookup$row_index[lookup$site == site1],
    # #   lookup$row_index[lookup$site == site2]]
    #)
  #) %>%
  #ungroup()


#site_pairs <- site_pairs %>%
 # left_join(all_sites %>% rename(lat1 = Latitude, lon1 = Longitude), by = c("site1" = "site")) %>%
  #left_join(all_sites %>% rename(lat2 = Latitude, lon2 = Longitude), by = c("site2" = "site"))


#write.csv(site_pairs, "data/site_pairs.csv", row.names = FALSE)

#state_map <- map_data("state") %>%
 # filter(long >= -128 & long <= -95 & lat >= 30 & lat <= 52)

#ggplot() +
 # geom_polygon(data = state_map, aes(x = long, y = lat, group = group),
         #      fill = "gray90", color = "black") +
  # lines of kinship
 # geom_segment(data = site_pairs, 
  #             aes(x = lon1, y = lat1, xend = lon2, yend = lat2,
   #                color = kinship_avg, size = kinship_avg),
    #           alpha = 0.7) +
  #scale_color_viridis_c(option = "plasma", name = "Avg Kinship") +
  #scale_size(range = c(0.2, 1.5), guide = "none") +
  # Site points
 # geom_point(data = all_sites, aes(x = Longitude, y = Latitude,
  #                                 shape = ifelse(site %in% GPS_sites$site, "Seed Source", "Satellite")),
 #            size = 3, color = "black") +
#  scale_shape_manual(values = c("Seed Source" = 16, "Satellite" = 17), name = #"Site Type") +
 # theme_minimal() +
  #coord_cartesian(xlim = c(-128, -95), ylim = c(30, 52)) +
  #labs(title = "Relatedness among sites (Average Kinship)",
   #    x = "Longitude", y = "Latitude")

###### K all look up
# Create lookup table for FULL kinship matrix (K_all)
#genotype_table_all <- tibble(
 # row_index = 1:nrow(K_all),                 # matrix row/column position
  #genotype  = rownames(K_all)                # genotype ID
#) %>%
 # left_join(genotype_index_all, by = "genotype") %>%
  #arrange(row_index)


#head(genotype_table_all)


#write.csv(genotype_table_all,
 #         "data/genotype_site_row_lookup_full_K_all.csv",
  #        row.names = FALSE)



#sat_sites2 <- sat_sites %>%
 # rename(site = ID,
  #       lat = Latitude,
   #      lon = Longitude)


#all_sites <- bind_rows(
 # GPS_sites %>% select(site, lat, lon),
  #sat_sites2 %>% select(site, lat, lon)
#)


#lookup_coords <- genotype_table_all %>%
 # left_join(all_sites, by = "site")

#K_all_df <- as.data.frame(K_all)
#K_all_df$row_index1 <- rownames(K_all_df)

#K_all_long <- K_all_df %>%
 # pivot_longer(-row_index1,
  #             names_to = "row_index2",
   #            values_to = "relatedness")
#K_all_map <- K_all_long %>%
 # left_join(lookup_coords, by = c("row_index1" = "row_index")) %>%
  #rename(lat1 = lat, lon1 = lon, site1 = site) %>%
  #left_join(lookup_coords, by = c("row_index2" = "row_index")) %>%
  #rename(lat2 = lat, lon2 = lon, site2 = site)



#usa <- map_data("state")

#K_all_map_clean <- K_all_map %>%
 # filter(row_index1 < row_index2) %>%
#  filter(!is.na(lat1), !is.na(lat2))

#ggplot() +
 # geom_polygon(data = usa,
  #             aes(x = long, y = lat, group = group),
   #            fill = "gray95",
  #             color = "white") +
  #geom_segment(data = K_all_map_clean,
   #            aes(x = lon1, y = lat1,
    #               xend = lon2, yend = lat2,
     #              alpha = relatedness),
      #         color = "blue") +
  #geom_point(data = lookup_coords,
   #          aes(x = lon, y = lat),
    #         color = "red",
     #        size = 2) +
  #coord_fixed(1.3) +
  #theme_classic() +
  #labs(alpha = "Relatedness",
   #    title = "Geographic Relatedness Network (K_all)")