#!/usr/bin/env Rscript

# Scripts for calculating the assortative genetic ancestry index, heterozygosity, relatedness and female transformed age included as predictor variables in the multivariate logistic regression models predicting grooming and proximity behavior

# Load the grooming data set available at XXX (calcuations are the same for the proximity data set so they are not included here)
groom <- read.csv("groom_anonymized.csv", header=T) 

# Load the microsatellite data set available at XXX 
microsat <- read.csv("microsat_anonymized.csv", header=T) 

#############################################################################################################################
# Assortative genetic ancestry index
#############################################################################################################################

# Calculate a pairwise assortative genetic ancestry index as a function of the genetic ancestry estimates of the female and male, paralleling the approach used in Tung et al. (2012)’s pairwise assortative mating index (Tung et al. 2012 - https://doi.org/10.1086/665993)
# See equation in methods

# multiply male genetic ancestry by female genetic ancestry and put results in new column called "assortative_1" which assigns a high value for assortatively affiliating male-female pairs for which both individuals have high genetic ancestry scores (i.e., both anubis-like individuals)
groom$assortative_1 <- (groom$genetic_ancestry_male*groom$genetic_ancestry_female)

# subtract both male genetic ancestry and female genetic ancestry from 1, multiply these values, and then put results in new column called "assortative_2" which assigns a high value for assortatively affiliating male-female pairs for which both individuals have low genetic ancestry scores (i.e., both yellow-like individuals)
groom$assortative_2 <- ((1-groom$genetic_ancestry_male)*(1-groom$genetic_ancestry_female))

# take the maximum value between assortative_1 and assortative_2 and assign that value as the assortative genetic ancestry index
groom$assortative_genetic_ancestry_index2 <- apply(groom[, (ncol(groom)-1):ncol(groom)], 1, max) 

# assortative_genetic_ancestry_index2 should equal the assortative_genetic_ancestry_index column in the groom dataframe
sum(groom$assortative_genetic_ancestry_index2==groom$assortative_genetic_ancestry_index)==nrow(groom) # TRUE - these columns match!

#############################################################################################################################
# Heterozygostiy
#############################################################################################################################


#############################################################################################################################
# Relatedness
#############################################################################################################################


#############################################################################################################################
# Female transformed age
#############################################################################################################################

# Calculate a transformed measure of female age (following Tung et al. 2012) that reflects the relationship between female age and conception probability in the Amboseli baboon population where the highest conception probability occurs at ~14 years of age (Beehner et al. 2006 - https://doi.org/10.1093/beheco/arl006)
# See equation in methods

# calculate female transformed age as a function of the untransformed female age (transformed female age is given 0 at an untransformed female age of 14; transformed female age becomes more negative with deviations from untransformed female age of 14) and put results in new column called female_transformed_age2
groom$female_age_transformed2 <- -1*(((groom$female_age - 14)/14)^2)

# female_age_transformed2 should equal the female_age_transformed2 column in the groom dataframe
sum(groom$female_age_transformed2==groom$female_age_transformed)==nrow(groom) # TRUE - these columns match!
