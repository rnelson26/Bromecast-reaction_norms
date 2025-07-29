#!/bin/bash
#SBATCH --job-name=fit_models
#SBATCH --output=logs/fit_models_%j.out
#SBATCH --error=logs/fit_models_%j.err
#SBATCH --time=48:00:00          # max time hh:mm:ss - adjust as needed
#SBATCH --cpus-per-task=4        # adjust based on your CPU needs
#SBATCH --mem=16G                # adjust memory as needed
#SBATCH --partition=standard     # change if your HPC uses partitions

# Load R module (change based on your HPC)
module load R/4.2.2

# Create logs directory if it doesn't exist
mkdir -p logs

# Run your R script
Rscript scripts/05_Fit_Models.R
