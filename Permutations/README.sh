# module load R
sbatch --mem=2G --cpus-per-task=20 --array=1-50%25 groom_perm.R
sbatch --mem=2G --cpus-per-task=20 --array=1-50%25 prox_perm.R 

cat GLMMTMB_permuted_results_prox1* >> all.GLMMTMB_permuted_results_prox1_noC_repstate_recoded.txt
cat GLMMTMB_permuted_results_prox2* >> all.GLMMTMB_permuted_results_prox2_noC_repstate_recoded.txt
cat GLMMTMB_permuted_results_groom1* >> all.GLMMTMB_permuted_results_groom1_noC_repstate_recoded.txt
cat GLMMTMB_permuted_results_groom2* >> all.GLMMTMB_permuted_results_groom2_noC_repstate_recoded.txt
