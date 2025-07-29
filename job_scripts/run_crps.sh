#!/bin/bash
#SBATCH --job-name=crps_calc
#SBATCH --output=logs/crps_%j.out
#SBATCH --error=logs/crps_%j.err
#SBATCH --time=24:00:00          # adjust based on expected runtime
#SBATCH --cpus-per-task=2        # adjust based on your script's parallelism
#SBATCH --mem=32G                # higher memory due to graphics and computations
#SBATCH --partition=standard

module load R/4.2.2

mkdir -p logs

Rscript scripts/06_CRPS.R
