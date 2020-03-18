# module load R
sbatch --mem=2G --cpus-per-task=20 --array=1-50%5 prox_noC_repstate1_recoded_glmmtmb.R 

sbatch --mem=2G --cpus-per-task=20 --array=1-50%5 prox_noC_repstate2_recoded_glmmtmb.R 

sbatch --mem=2G --cpus-per-task=20 --array=1-50%5 groom_noC_repstate1_recoded_glmmtmb.R 

sbatch --mem=2G --cpus-per-task=20 --array=1-50%5 groom_noC_repstate2_recoded_glmmtmb.R 

cat GLMMTMB_permuted_results_prox1* >> all.GLMMTMB_permuted_results_prox1_noC_repstate.txt
cat GLMMTMB_permuted_results_prox2* >> all.GLMMTMB_permuted_results_prox2_noC_repstate.txt
cat GLMMTMB_permuted_results_groom1* >> all.GLMMTMB_permuted_results_groom1_noC_repstate.txt
cat GLMMTMB_permuted_results_groom2* >> all.GLMMTMB_permuted_results_groom2_noC_repstate.txt
