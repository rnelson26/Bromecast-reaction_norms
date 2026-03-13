## load landscape-genomics informed kinship matrices
load("kinship_for_Becca.RData")
LGmat <- K_mean_Becca

## load and use lookup table for LG observed genotypes
LG_lookup <- read.csv("Matrices/all_genotypes_bioclim.csv")[, c(1, 2)]
colnames(LG_lookup) <- c("ID", "Genotype")
LG_lookup_ordered <- LG_lookup[match(rownames(LGmat), LG_lookup$ID), ]
genotype_vec <- LG_lookup_ordered$Genotype
rownames(LGmat)[1:86] <- paste0("X", genotype_vec[1:86])
colnames(LGmat)[1:86] <- paste0("X", genotype_vec[1:86])

## load lookup table for LG satellites
LG_sat <- read.csv("Matrices/assigned_genotypes.csv")[, c(1:3)]

## parse "(lon, lat)" strings for satellite sites
parse_coords <- function(coord_string) {
  coord_string <- gsub("[()]", "", coord_string)
  parts <- strsplit(coord_string, ",")
  coords <- do.call(rbind, parts)
  coords <- apply(coords, 2, as.numeric)
  colnames(coords) <- c("X", "Y")
  return(coords)
}
LG_coords <- parse_coords(rownames(LGmat))

## match coordinates and get site names
match_index <- match(
  paste(LG_coords[,1], LG_coords[,2]),
  paste(LG_sat$X, LG_sat$Y)
)
site_names <- LG_sat$site[match_index]

## map satellite names
rownames(LGmat)[87:129] <- site_names[87:129]
colnames(LGmat)[87:129] <- site_names[87:129]

## lookup matrix from new site names to "G" format of Becca
lookup_G <- read.csv("Matrices/genotype_site_row_lookup.csv")[80:121,-1]
lookup_G$site <- substr(
  lookup_G$site,
  1,
  nchar(lookup_G$site) - 5
)
idx <- 87:129
match_idx <- match(rownames(LGmat)[idx], lookup_G$site)
rownames(LGmat)[idx] <- lookup_G$genotype[match_idx]
colnames(LGmat)[idx] <- lookup_G$genotype[match_idx]

## load Becca's current matrix
CUmat <- read.csv("Matrices/K_all_genotypes_filtered_common_garden_plus_satellites.csv", header = TRUE)[, -1]

## identify mismatches
keep_ids <- colnames(CUmat)
missing_in_LG <- setdiff(keep_ids, colnames(LGmat))
extra_in_LG <- setdiff(colnames(LGmat), keep_ids)

##
## subset to match
##

LGmat <- as.matrix(LGmat)
CUmat <- as.matrix(CUmat)
rownames(CUmat)=colnames(CUmat)
fix_names <- function(x) trimws(as.character(x))
colnames(LGmat) <- fix_names(colnames(LGmat)); colnames(LGmat) <- fix_names(colnames(LGmat))
colnames(CUmat) <- fix_names(colnames(CUmat)); colnames(CUmat) <- fix_names(colnames(CUmat))
common_ids <- intersect(colnames(CUmat), colnames(LGmat))
common_ids <- colnames(CUmat)[colnames(CUmat) %in% common_ids]
LGmat_common <- LGmat[common_ids, common_ids, drop = FALSE]
CUmat_common <- CUmat[common_ids, common_ids, drop = FALSE]
colnames(LGmat_common) <- rownames(LGmat_common)
colnames(CUmat_common) <- rownames(CUmat_common)

##
## measures of difference
##

frobenius_dist <- norm(LGmat_common - CUmat_common, type = "F")
frobenius_dist

mean_abs_diff <- mean(abs(LGmat_common - CUmat_common))
mean_abs_diff

matrix_cor <- cor(
  as.vector(LGmat_common),
  as.vector(CUmat_common)
)
matrix_cor

rmsd <- sqrt(mean((LGmat_common - CUmat_common)^2))
rmsd

##
## just looking at satellite sites
##

G_ids <- grep("^G", rownames(LGmat_common), value = TRUE)
LG_G <- LGmat_common[G_ids, G_ids, drop = FALSE]
CU_G <- CUmat_common[G_ids, G_ids, drop = FALSE]

rmsd_G <- sqrt(mean((LG_G - CU_G)^2))
rmsd_G

mad_G <- mean(abs(LG_G - CU_G))
mad_G

