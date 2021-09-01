This directory contains multiple scripts for using permutation tests to assess statistical significance in the multivariate logistic regression models.
Note that because the p-values reported in the manuscript were based on permutations, re-running these scripts will replicate the results but with p-values that may differ slightly from what we reported.

In R, run the script `1prepare_for_permutations.R` which computes, for each female-two-month interval combination, the proportion of dyads where an event (grooming or proximity) occurred (these values are estimates of the probability of grooming or proximity with any male, per female-interval combination). This script generates two R data files (`groom_perm.Rd` and `prox_perm.Rd`) which can then be uploaded to a computing cluster for parallelization of permutations.

In a directory on a computing cluster containing both R data files, run the scripts `2agroom_perm.R` and `2bprox_perm.R` as arrays (see below) which will:
1. randomly permute the grooming/proximity probabilities computed in `1prepare_for_permutations.R` across all female two-month intervals,
2. randomly assign 0/1 by drawing from a binomial distribution with probability equal to the permuted grooming/proximity probability for each female two-month interval and,
3. fit the same grooming/proximity model used to analyze the real data but using the new, permutation-based outcome variable.

Run 50 jobs per array (since each job is currently set to run 20 permutations which can be changed by setting _nperm_ to a value other than 20 in `groom_perm.R` and `prox_perm.R`) because we want to run 1,000 permutations per model in total (50 jobs * 20 permutations/job = 1,000 permutations total).
`module load R` before running these scripts since `groom_perm.R` and `prox_perm.R` are set up to load the current environment where R needs to already be loaded.
```console
sbatch --mem=100 --cpus-per-task=20 --array=1-50%5 2agroom_perm.R
sbatch --mem=100 --cpus-per-task=20 --array=1-50%5 2bprox_perm.R
```

Concatenate the output of the 50 jobs per array (20 permutations per job) into a single file for each model.
```console
cat groom_permuted_results_*.txt >> all.groom_permuted_results.txt
cat prox_permuted_results_*.txt >> all.prox_permuted_results.txt
```

Delete individual files for each job in the array as these results are now stored in either the `all.groom_permuted_results.txt` or `all.prox_permuted_results.txt` files generated above.
```console
rm groom_permuted_results_*
rm prox_permuted_results_*
```

Check that each model's file, which includes all of the permutation results per model, has 1,000 lines corresponding to the 1,000 permutations.
```console
wc -l all.*
#1000 all.groom_permuted_results.txt
#1000 all.prox_permuted_results.txt
#2000 total
```

Finally, in R, run `3permuted_pvalue_calculation.R` which calculates a permutation based p-value for each predictor variable based on the number of times that the absolute value of the effect size estimated from the permuted data sets was greater than the absolute value of the effect size estimated from the observed data set, across 1,000 permutations.
