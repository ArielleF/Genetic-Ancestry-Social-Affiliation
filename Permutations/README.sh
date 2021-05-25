# module load R before running script since we're set up the groom_perm.R and prox_perm.R to load our current environment
sbatch --mem=100 --cpus-per-task=20 --array=1-50%25 groom_perm.R
sbatch --mem=100 --cpus-per-task=20 --array=1-50%25 prox_perm.R 

# concatenate the 50 jobs per array (20 permutations per job) into a single file for each model
cat groom_permuted_results_* >> all.groom_permuted_results.txt
cat prox_permuted_results_* >> all.prox_permuted_results.txt

# delete individual files for each job in the array as these results are now stored in the all.xxx_permute_results.txt files above
rm groom_permuted_results_*
rm prox_permuted_results_*

# check that each data frame with all of the permutaiton results has 1,000 lines corresponding to the 1,000 permutations run
wc -l all.*
#1000 all.groom_permuted_results.txt
#1000 all.prox_permuted_results.txt
#2000 total
