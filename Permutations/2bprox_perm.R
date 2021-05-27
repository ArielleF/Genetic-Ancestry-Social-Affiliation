#!/bin/env Rscript
load("prox_perm.Rd")

# Load R libraries
library(parallel)
library(doParallel)

clus <- makeCluster(20)
registerDoParallel(cores=20)

clusterExport(clus,varlist=c("prox_null_unique_prob","prox_null_2"),envir=environment())

nperm=20 # run 20 permutations per job in the array (and we'll run 50 arrays so in total = 50 * 20 = 1000 permutations)

permuted_results=Reduce(rbind,parLapply(clus,1:nperm,function(perm){
  library(glmmTMB, lib.loc="/data/tunglab/asf40/programs/Rlibs/") # location of installed glmmTMB library
  prox_null_unique_prob$prox_prob_perm <- sample(prox_null_unique_prob$prox_prob,2338,replace=F) # randomly permute proximity probabilities across all female two-month intervals; note that 2338 is the number of female two-month intervals in the proximity data set
  prox_null_perm <- merge(prox_null_2,prox_null_unique_prob,by="female_interval_id")
  prox_null_perm$prox_two_month_perm=NA # this is where we will put the new, permutation-based outcome variable
  for (name in prox_null_unique_prob$female_interval_id){
    prox_null_perm[prox_null_perm$female_interval_id==name,"prox_two_month_perm"] <- rbinom(n=length(which(prox_null_perm$female_interval_id==name)),size=1,prob=as.numeric(unlist(prox_null_unique_prob[prox_null_unique_prob$female_interval_id == name,])[3])) # randomly assign 0/1 by drawing from a binomial distribution with probability equal to the permuted proximity probability for each female two-month interval 
    
  }
  
  # fit the same proximity model used to analyze the real data but use the new, permutation-based outcome variable
  prox_null_model <-
    glmmTMB(prox_two_month_perm ~ assortative_genetic_ancestry_index + heterozygosity_female + heterozygosity_male + genetic_relatedness + rank_female*rank_male + female_age + female_age_transformed + females_in_group + males_in_group + reproductive_state*genetic_ancestry_female + reproductive_state*genetic_ancestry_male + pair_coresidency + observer_effort + (1 | female_id) + (1 | male_id), data = prox_null_perm, family = "binomial")
  
  print(perm)
  return(unlist(fixef(prox_null_model))[1:19])
}))


write.table(permuted_results,file=paste0("prox_permuted_results_",seed,".txt"),quote=F,col.names=F,row.names=F)

print("done")

q(save="no")
