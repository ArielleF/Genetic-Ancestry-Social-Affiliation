#!/usr/bin/env Rscript

# Scripts for running the multivariate logistic regression models predicting grooming and proximity behavior

# Load the grooming and proximity data sets available at https://doi.org/10.7924/r4kp82d1z
groom <- read.csv("groom_anonymized.csv", header=T) 
prox <- read.csv("prox_anonymized.csv", header=T) 

# Load R libraries
library(glmmTMB)

#############################################################################################################################
# Model 1. Multivariate logistic regression model predicting grooming behavior.
#############################################################################################################################

# run grooming model
groom_model <- glmmTMB(groom_two_month ~ assortative_genetic_ancestry_index + heterozygosity_female + heterozygosity_male + genetic_relatedness + rank_female*rank_male + female_age + female_age_transformed + females_in_group + males_in_group + reproductive_state*genetic_ancestry_female + reproductive_state*genetic_ancestry_male + pair_coresidency + observer_effort + (1 | female_id) + (1 | male_id), data=groom, family="binomial")

# look at model output (this includes non-permuted p-values)
summary(groom_model)
groom_model_betas <- fixef(groom_model)$cond # get betas (effect estimates) for intercept and predictor variables 
round(groom_model_betas, 3) # effect estimates reported in Table 1  


#############################################################################################################################
# Model 2. Multivariate logistic regression model predicting proximity behavior.
#############################################################################################################################

# run proximity model
prox_model <- glmmTMB(prox_two_month ~ assortative_genetic_ancestry_index + heterozygosity_female + heterozygosity_male + genetic_relatedness + rank_female*rank_male + female_age + female_age_transformed + females_in_group + males_in_group + reproductive_state*genetic_ancestry_female + reproductive_state*genetic_ancestry_male + pair_coresidency + observer_effort + (1 | female_id) + (1 | male_id), data=prox, family="binomial")

# look at model output (this includes non-permuted p-values)
summary(prox_model)
prox_model_betas <- fixef(prox_model)$cond # get betas (effect estimates) for intercept and predictor variables 
round(prox_model_betas, 3) # effect estimates reported in Table 2  
