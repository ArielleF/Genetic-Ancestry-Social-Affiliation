#!/bin/env Rscript
load("prox_perm_18Mar2020.Rd")
## simulations
library(parallel)
library(doParallel)
seed=as.numeric(Sys.getenv(c("SLURM_ARRAY_TASK_ID")))
set.seed(seed)
clus <- makeCluster(20)
registerDoParallel(cores=20)

clusterExport(clus,varlist=c("fem_uniq_id_uniq_p","prox_null_2"),envir=environment())

nperm=20

permuted_results=Reduce(rbind,parLapply(clus,1:nperm,function(perm){
library(glmmTMB, lib.loc="/data/tunglab/asf40/programs/Rlibs/")
fem_uniq_id_uniq_p$prox_probperm<-sample(fem_uniq_id_uniq_p$prox_prob,2356,replace=F)
prox_null_perm<-merge(prox_null_2,fem_uniq_id_uniq_p,by="fem_uniq_id")
prox_null_perm$prox_two_month_perm=NA
for (name in fem_uniq_id_uniq_p$fem_uniq_id){
   prox_null_perm[prox_null_perm$fem_uniq_id==name,"prox_two_month_perm"]<-rbinom(n=length(which(prox_null_perm$fem_uniq_id==name)),size=1,prob=as.numeric(unlist(fem_uniq_id_uniq_p[fem_uniq_id_uniq_p$fem_uniq_id == name,])[3]))

}

prox_null_model <-
glmmTMB(prox_two_month_perm ~ male_hybridscore + female_hybridscore + females_in_grp_avg + males_in_grp_avg + female_age_avg + female_rank_avg + male_rank_avg + female_rank_avg*male_rank_avg + female_age_transform_with_avg + reproductive_state_binary + assortative_max + male_hybridscore*reproductive_state_binary + female_hybridscore*reproductive_state_binary + observer_effort_two_months + gen_diversity_male + gen_diversity_female + QG_final + (1 | female_sname) + (1 | male_sname), data = prox_null_perm, family = "binomial")

  print(perm)
  return(unlist(fixef(prox_null_model))[1:18])
  }))


write.table(permuted_results,file=paste0("GLMMTMB_permuted_results_prox1_noC_repstate_recoded",seed,".txt"),quote=F,col.names=F,row.names=F)
q(save="no")
