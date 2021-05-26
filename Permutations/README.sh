# In R, run 1prepare_for_permutations.R which computes, for each female-two-month interval combination, the proportion of dyads where an event (grooming or proximity) occurred (these values are estimates of the probability of grooming or proximity with any male, per female-interval combination). This script generates two R data files (.Rd) which can then be uploaded to a computing cluster for parallelization of permutations.

# In a directory on a computing cluster containing both R data files, run the scripts 2agroom_perm.R and 2bprox_perm.R as arrays (see below) which will randomly permute the grooming/proximity probabilities computed in 1prepare_for_permutations.R across all female two-month intervals), randomly assign 0/1 by drawing from a binomial distribution with probability equal to the permuted grooming/proximity probability for each female two-month interval, and then fit the same grooming/proximity model used to analyze the real data but using the new, permutation-based outcome variable.
# Run 50 jobs per array since each job is currently set to run 20 permutations (can be changed by setting nperm to a value other than 20 in groom_perm.R and prox_perm.R) and we want to run 1,000 permutations per model in total (50 jobs * 20 permutations/job = 1,000 permutations total)
# module load R before running script since we're set up the groom_perm.R and prox_perm.R to load our current environment
sbatch --mem=100 --cpus-per-task=20 --array=1-50%5 2agroom_perm.R
sbatch --mem=100 --cpus-per-task=20 --array=1-50%5 2bprox_perm.R 

# Concatenate the output of the 50 jobs per array (20 permutations per job) into a single file for each model
cat groom_permuted_results_*.txt >> all.groom_permuted_results.txt
cat prox_permuted_results_*.txt >> all.prox_permuted_results.txt

# Delete individual files for each job in the array as these results are now stored in either the all.groom_permuted_results.txt or all.prox_permuted_results.txt files above
rm groom_permuted_results_*
rm prox_permuted_results_*

# Check that each dataframe with all of the permutation results has 1,000 lines corresponding to the 1,000 permutations run
wc -l all.*
#1000 all.groom_permuted_results.txt
#1000 all.prox_permuted_results.txt
#2000 total

# Finally, in R, run 3permuted_pvalue_calculation.R which calculates a permutation based p-value for each predictor variable based on the number of times that the absolute value of the effect size estimated from the permuted data sets was greater than the absolute value of the effect size estimated from the observed data set, across 1,000 permutations. 
