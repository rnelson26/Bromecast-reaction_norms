#mkdir -p ~/projects/Bromecast
#cd ~/projects/Bromecast

# ==========================
# setup_hpc.R
# Install R packages, CmdStan, and clone GitHub repo
# ==========================

# Use a personal library
lib <- "~/R/4.2.2"
dir.create(lib, showWarnings = FALSE, recursive = TRUE)
.libPaths(lib)

# Install CRAN packages
install.packages(
  c("tidyverse","bayesplot","posterior","loo","rstan","remotes","cmdstanr"),
  lib = lib,
  repos = "https://cloud.r-project.org",
  dependencies = TRUE
)

# Install CmdStan
library(cmdstanr)
cmdstanr::install_cmdstan(dir="~/cmdstan", cores = 4, overwrite = TRUE)

# ==========================
# Clone GitHub repository (non-interactive)
# ==========================
proj_dir <- "~/projects/Bromecast/Bromecast-reaction_norms"

if (!dir.exists(proj_dir)) {
  # Use HTTPS if SSH keys are not set up
  system("git clone https://github.com/rnelson26/Bromecast-reaction_norms.git ~/projects/Bromecast/Bromecast-reaction_norms")
} else {
  message("Repository already exists: ", proj_dir)
}

message("Setup complete!")
