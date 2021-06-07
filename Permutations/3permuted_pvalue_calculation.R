#!/usr/bin/env Rscript

# Script for calculating a permutation based p-value for each predictor variable based on the number of times that the absolute value of the effect size estimated from the permuted data sets was greater than the absolute value of the effect size estimated from the observed data set, across 1,000 permutations. 

# Load results from all 1,000 permutations 
permuted_results_groom <- read.table("all.groom_permuted_results.txt", header=F)
permuted_results_prox <- read.table("all.prox_permuted_results.txt", header=F)

# Load R libraries
library(ggplot2)
library(glmmTMB)

# set the number of total permutations run
totalperm=1000
# this should equal the number of rows in permuted_results_groom and permuted_results_prox
nrow(permuted_results_groom)==totalperm # TRUE
nrow(permuted_results_prox)==totalperm # TRUE

# We will compare the effect sizes estimated from the permuted data sets with the effect size estimated from the observed data set so need to rerun the original model to get effect sizes from the empirical data sets
# Load the grooming and proximity data sets available at https://doi.org/10.7924/r4kp82d1z
groom <- read.csv("groom_anonymized.csv", header=T) 
prox <- read.csv("prox_anonymized.csv", header=T) 
# run grooming model (the same code is in the Models directory)
groom_model <- glmmTMB(groom_two_month ~ assortative_genetic_ancestry_index + heterozygosity_female + heterozygosity_male + genetic_relatedness + rank_female*rank_male + female_age + female_age_transformed + females_in_group + males_in_group + reproductive_state*genetic_ancestry_female + reproductive_state*genetic_ancestry_male + pair_coresidency + observer_effort + (1 | female_id) + (1 | male_id), data=groom, family="binomial")
groom_model_betas <- fixef(groom_model)$cond # get betas (effect estimates) for intercept and predictor variables 
# run proximity model (the same code is in the Models directory)
prox_model <- glmmTMB(prox_two_month ~ assortative_genetic_ancestry_index + heterozygosity_female + heterozygosity_male + genetic_relatedness + rank_female*rank_male + female_age + female_age_transformed + females_in_group + males_in_group + reproductive_state*genetic_ancestry_female + reproductive_state*genetic_ancestry_male + pair_coresidency + observer_effort + (1 | female_id) + (1 | male_id), data=prox, family="binomial")
prox_model_betas <- fixef(prox_model)$cond # get betas (effect estimates) for intercept and predictor variables 


#############################################################################################################################
# Permutation based p-values for grooming model.
#############################################################################################################################

pval_groom <- c() # where we will store the permutation based p-values

for (i in 1:ncol(permuted_results_groom)) { # for the model intercept and each predictor variable
  pval_groom[i] <- (sum(abs(unlist(groom_model_betas)[i]) < abs(permuted_results_groom[,i])))/totalperm # the number of times that the absolute value of the effect size estimated from the permuted data sets was greater than the absolute value of the effect size estimated from the observed data set divided by the number of permutations run (totalperm)
}

groom_results_final <- as.data.frame(cbind(names(groom_model_betas),unlist(groom_model_betas, use.names=F), pval_groom))
colnames(groom_results_final)[1] <- c("variable")
colnames(groom_results_final)[2] <- c("beta")

groom_results_final$beta <- as.numeric(as.character(groom_results_final$beta))
groom_results_final$pval_groom <- as.numeric(as.character(groom_results_final$pval_groom))

# let's plot both the effect estimates and the negative log10 permuted p-values for the proximity model
ggplot(data=groom_results_final) + geom_point(aes(variable, beta), size=3) + theme_classic() + theme(axis.text.x = element_text(angle=90)) # plot effect estimates
ggplot(data=groom_results_final) + geom_point(aes(variable, -log10(pval_groom)), size=3) + theme_classic() + theme(axis.text.x = element_text(angle=90)) # plot permuted p-values; -log10 of pvalues = 0 is undefined so predictor variables with these p-values will be shown cutoff at the very top of the plot


#############################################################################################################################
# Permutation based p-values for proximity model.
#############################################################################################################################

pval_prox <- c() # where we will store the permutation based p-values

for (i in 1:ncol(permuted_results_prox)) { # for the model intercept and each predictor variable
  pval_prox[i] <- (sum(abs(unlist(prox_model_betas)[i]) < abs(permuted_results_prox[,i])))/totalperm # the number of times that the absolute value of the effect size estimated from the permuted data sets was greater than the absolute value of the effect size estimated from the observed data set divided by the number of permutations run (totalperm)
}

prox_results_final <- as.data.frame(cbind(names(prox_model_betas), unlist(prox_model_betas, use.names=F), pval_prox))
colnames(prox_results_final)[1] <- c("variable")
colnames(prox_results_final)[2] <- c("beta")

prox_results_final$beta <- as.numeric(as.character(prox_results_final$beta))
prox_results_final$pval_prox <- as.numeric(as.character(prox_results_final$pval_prox))

# let's plot both the effect estimates and the negative log10 permuted p-values for the proximity model
ggplot(data=prox_results_final) + geom_point(aes(variable, beta), size=3) + theme_classic() + theme(axis.text.x = element_text(angle=90)) # plot effect estimates
ggplot(data=prox_results_final) + geom_point(aes(variable, -log10(pval_prox)), size=3) + theme_classic() + theme(axis.text.x = element_text(angle=90)) # plot permuted p-values; -log10 of pvalues = 0 is undefined so predictor variables with these p-values will be shown cutoff at the very top of the plot

# we can also plot the effect estimates of the predictor variables from both the grooming and proximity models in the same plot
ggplot() + geom_point(data=prox_results_final, aes(variable, beta), color="forestgreen", alpha=0.7, size=3) + geom_point(data=groom_results_final, aes(variable, beta), color="orange", alpha=0.8, size=3) + theme_classic() + theme(axis.text.x = element_text(angle=90))
# we can also plot the effect estimates of the predictor variables with permuted p-values less than 0.05 from both the grooming and proximity models in the same plot
ggplot() + theme_classic() + theme(axis.text.x = element_text(angle=90)) + geom_point(data=subset(groom_results_final, pval_groom<0.05), aes(variable, beta), color="orange", alpha=0.8, size=3) + geom_point(data=subset(prox_results_final, pval_prox<0.05), aes(variable, beta), color="forestgreen", alpha=0.7, size=3)
