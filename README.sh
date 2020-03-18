# module load R
sbatch --mem=2G --cpus-per-task=20 --array=1-50%5 prox_noC_repstate1_recoded_glmmtmb.R 

sbatch --mem=2G --cpus-per-task=20 --array=1-50%5 prox_noC_repstate2_recoded_glmmtmb.R 
