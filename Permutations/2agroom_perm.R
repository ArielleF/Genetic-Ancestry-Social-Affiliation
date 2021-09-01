#!/bin/env Rscript
load("groom_perm.Rd") # generated in script 1prepare_for_permutations.R

# Load R libraries
library(parallel)
library(doParallel)

clus <- makeCluster(20)
registerDoParallel(cores=20)

clusterExport(clus,varlist=c("groom_null_unique_prob","groom_null_2"),envir=environment())

nperm=20 # run 20 permutations per job in the array (and we'll run 50 arrays so in total = 50 * 20 = 1000 permutations)

permuted_results=Reduce(rbind,parLapply(clus,1:nperm,function(perm){
  library(glmmTMB, lib.loc="/data/tunglab/asf40/programs/Rlibs/")
  groom_null_unique_prob$groom_prob_perm <- sample(groom_null_unique_prob$groom_prob,1866,replace=F) # randomly permute grooming probabilities across all female two-month intervals; note that 1866 is the number of unique female two-month intervals in the grooming data set
  groom_null_perm <- merge(groom_null_2,groom_null_unique_prob,by="female_interval_id")
  groom_null_perm$groom_two_month_perm=NA # this is where we will put the new, permutation-based outcome variable
  for (name in groom_null_unique_prob$female_interval_id){
    groom_null_perm[groom_null_perm$female_interval_id==name,"groom_two_month_perm"] <- rbinom(n=length(which(groom_null_perm$female_interval_id==name)),size=1,prob=as.numeric(unlist(groom_null_unique_prob[groom_null_unique_prob$female_interval_id == name,])[3])) # randomly assign 0/1 by drawing from a binomial distribution with probability equal to the permuted grooming probability for each female two-month interval 
    
  }
  
  # fit the same grooming model used to analyze the real data but use the new, permutation-based outcome variable
  groom_null_model <-
    glmmTMB(groom_two_month_perm ~ assortative_genetic_ancestry_index + heterozygosity_female + heterozygosity_male + genetic_relatedness + rank_female*rank_male + female_age + female_age_transformed + females_in_group + males_in_group + reproductive_state*genetic_ancestry_female + reproductive_state*genetic_ancestry_male + pair_coresidency + observer_effort + (1 | female_id) + (1 | male_id), data = groom_null_perm, family = "binomial")
  
  print(perm)
  return(unlist(fixef(groom_null_model))[1:19]) # output intercept and all predictor variables in the model
}))


write.table(permuted_results,file=paste0("groom_permuted_results_",as.numeric(Sys.getenv(c("SLURM_ARRAY_TASK_ID"))),".txt"),quote=F,col.names=F,row.names=F) # output results as text file with name that identifies the job number within the array (i.e., 1-50) so that this file doesn't overwrite other job outputs from the array


print("done")

q(save="no")
