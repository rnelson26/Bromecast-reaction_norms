
#!/bin/bash
#SBATCH --job-name=fit_models
#SBATCH --output=logs/fit_models_%A_%a.out
#SBATCH --error=logs/fit_models_%A_%a.err
#SBATCH --time=24:00:00
#SBATCH --cpus-per-task=4
#SBATCH --mem=16G
#SBATCH --partition=standard
#SBATCH --array=1-18   # one per model

module load R/4.2.2
mkdir -p logs results/draws

echo "SLURM_ARRAY_TASK_ID: $SLURM_ARRAY_TASK_ID"
Rscript scripts/09.1_run_single_model.R $SLURM_ARRAY_TASK_ID

## submit: sbatch job_scripts/run_fit_models.sh
