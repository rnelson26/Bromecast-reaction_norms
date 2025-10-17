
######### Landscape Genomics with GLMNET ##############
########### Bromecast Reaction Norms ############
######## Create K and assign genotypes ##########
######## code by Justin Van Ee and Becca Nelson ###############
############ created 8-19-25 #############
############ last modified 10-17-25 ##########################


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

# Step 4: Compute pairwise Euclidean distances in PC space
D_all <- as.matrix(dist(PCs_all, method = "euclidean"))

# Step 5: Calculate kinship from original SNPs
K <- SNPs[,-c(1:3)] %>% t() %>% kinship(method="IBS", MAF=0.10) %>% cov2cor()
D <- dist(PCs, method = "euclidean", diag = TRUE, upper = TRUE) %>% as.matrix()
log_kinship <- c(log(K))
distance <- c(D)
opt_range <- lm(log_kinship ~ distance + I(distance^2) - 1)

# Step 6: Predict kinship for all genotypes including synthetic
K_new_raw <- matrix(
  exp(predict(opt_range, newdata = data.frame(distance = c(D_all)))),
  nrow = nrow(D_all),
  ncol = ncol(D_all)
)

# Step 7: Enforce positive definiteness
K_all <- Matrix::nearPD(K_new_raw)$mat %>% as.matrix()
summary(c(abs(K_all - K_new_raw)))

# Step 8: Combine genotype index
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


# Optional checks
stopifnot(all(rownames(PCs_all) == genotype_index_all$genotype))
stopifnot(all(rownames(K_all) == genotype_index_all$genotype))

#### Step 9: Save for analysis
# Save as .RData (can save multiple objects)
save(K_all, genotype_index_all, PCs_all, file = "data/K_all_genotypes.RData")
write.csv(K_all, "data/K_all_genotypes.csv", row.names = TRUE)

# Plot kinship distributions
K_off <- K[upper.tri(K)]
Knew_off <- K_all[upper.tri(K_all)]

df <- data.frame(
  value = c(K_off, Knew_off),
  Method = rep(c("IBS", "PC - Exponential Decay"),
               times = c(length(K_off), length(Knew_off)))
)

ggplot(df, aes(x = value, fill = Method)) +
  geom_density(alpha = 0.5, position = "identity") +
  theme_bw() +
  labs(
    x = "Kinship",
    y = "Density",
    title = "Distribution of Kinship Coefficients"
  ) +
  theme(legend.position = "bottom")

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






