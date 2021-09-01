#!/usr/bin/env Rscript

# Script for preparing for permutations to assess statistical significance of predictor variables in the multivariate logistic regression models predicting grooming and proximity behavior
# This script computes, for each female-two month interval combination, the proportion of dyads where an event (grooming or proximity) occurred. These values are estimates of the probability of grooming or proximity with any male, per female-interval combination.

# Load the grooming data set available at https://doi.org/10.7924/r4kp82d1z
groom <- read.csv("groom_anonymized.csv", header=T) 

# Load the proximity data set available at https://doi.org/10.7924/r4kp82d1z
prox <- read.csv("prox_anonymized.csv", header=T) 

# Load R libraries
library(dplyr)

# create a female-two month interval id by pasting a female's id with the two month interval id
groom$female_interval_id <- paste(groom$female_id, groom$interval_id, sep="_") 
prox$female_interval_id <- paste(prox$female_id, prox$interval_id, sep="_")

groom_null_1 <- groom %>% group_by(female_interval_id) %>% mutate(count=dplyr::n()) # count the total number of grooming opportunities in a given female_interval_id (i.e., all potential male grooming partners in a female's given social group in a given two month interval) and put in new column titled "count"
groom_null_2 <- groom_null_1 %>% group_by(female_interval_id) %>% mutate(groom_count=sum(groom_two_month)) # count the total number of grooming occurrences (i.e., 1 for groom_two_month) in a given female_interval_id and put in new column titled "groom_count"
groom_null_2$groom_prob <- groom_null_2$groom_count/groom_null_2$count # divide the total number of grooming occurrences by the total number of grooming opportunities to get the probability of grooming for a given female_interval_id and put in a new column titled "groom_prob"

# summary(groom_null_2$groom_prob)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0500  0.1000  0.1429  0.1780  0.2222  1.0000 

prox_null_1 <- prox %>% group_by(female_interval_id) %>% mutate(count=dplyr::n()) # count the total number of proximity opportunities in a given female_interval_id (i.e., all potential males that could be in proximity in a female's given social group in a given two month interval) and put in new column titled "count"
prox_null_2 <- prox_null_1 %>% group_by(female_interval_id) %>% mutate(prox_count=sum(prox_two_month)) # count the total number of proximity occurrences (i.e., 1 for prox_two_month) in a given female_interval_id and put in new column titled "prox_count"
prox_null_2$prox_prob <- prox_null_2$prox_count/prox_null_2$count # divide the total number of proximity occurrences by the total number of proximity opportunities to get the probability of being in proximity for a given female_interval_id and put in a new column titled "prox_prob"

# summary(prox_null_2$prox_prob)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.05263 0.12500 0.22222 0.27974 0.36842 1.00000 

# create dataframe with one row per female-interval combination and its associated grooming/proximity probability
groom_null_unique_prob <- unique(groom_null_2[c("female_interval_id","groom_prob")])
nrow(groom_null_unique_prob) # 1866 - number of female two-month intervals in the grooming data set
prox_null_unique_prob <- unique(prox_null_2[c("female_interval_id","prox_prob")])
nrow(prox_null_unique_prob) # 2338 - number of female two-month intervals in the proximity data set

# save dataframes for uploading to a computing cluster for parallelization of permutations
save(groom_null_unique_prob,groom_null_2,file="groom_perm.Rd")
save(prox_null_unique_prob,prox_null_2,file="prox_perm.Rd")
