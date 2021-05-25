#!/usr/bin/env Rscript

# Scripts for calculating the assortative genetic ancestry index, heterozygosity, relatedness and female transformed age included as predictor variables in the multivariate logistic regression models predicting grooming and proximity behavior

# Load the grooming data set available at XXX (calcuations are the same for the proximity data set so they are not included here)
groom <- read.csv("groom_anonymized.csv", header=T) 

# Load the microsatellite genotype data set available at XXX 
geno <- read.csv("geno_anonymized.csv", header=T) 

# Load R libraries
library(dplyr)
library(related)

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

# Calculate each individual's genetic diversity by dividing the number of heterozygous loci by the number of genotyped loci for each individual (following Charpentier et al. 2008 - https://doi.org/10.1111/j.1365-294X.2008.03724.x).

ids <- as.data.frame(geno[c(1)]) # get the individual ids

# first, assign homozygous, heterozygous, or NA status for each individual at each locus 
for (i in 0:13) { # for each locus (n=14)
  
  geno2 <- geno[,((i*2)+2):((i*2)+3)] # grab both columns per locus
  geno2[,3] <- ifelse((geno2[,1]==geno2[,2] & !is.na(geno2[,1])), "HOM", ifelse((is.na(geno2[,1]) | is.na(geno2[,2])), NA, "HET")) # assign "HOM" (i.e., homozygous) if the same value in both columns (excluding NA values), assign "HET" (i.e., heterozygous) if different values in both columns (excluding NA values)
  
  colnames(geno2)[3] <- paste(colnames(geno2)[1], colnames(geno2)[2], sep="_") # assign locus as column name for column with HOM/HET/NA status
  
  if (i==0) {cbind(ids, geno2[c(3)]) -> gen_div} # if this is the first locus, combine the HOM/HET/NA status column with the ids and make new dataframe called gen_div
  if (i>0) {cbind(gen_div, geno2[c(3)]) -> gen_div} # if this not the first locus, combine the HOM/HET/NA status column with the gen_div dataframe
  
  print(i) # output locus number you are evaluating to track progress
  
}

# second, for each individual, count the number of loci that were successfully genotyped, the number of heterozygous loci, and the number of homozygous loci
for (i in 1:nrow(gen_div)) { # for each row (i.e., each individual)
  
  # count the number of loci that were successfully genotyped and put in new column called loci_genotyped                     
  gen_div$loci_genotyped[i] <- length(which(gen_div[i,2:15]=="HET")) + length(which(gen_div[i,2:15]=="HOM"))
  
  # count the number of heterozygous loci put in new column called HET_loci
  gen_div$HET_loci[i] <- length(which(gen_div[i,2:15]=="HET"))
  
  # count the number of heterozygous loci put in new column called HOM_loci
  gen_div$HOM_loci[i] <- length(which(gen_div[i,2:15]=="HOM")) 
  
  print(i) # output row number you are evaluating to track progress
}

# check that for alls rows (i.e., all individuals) in the datafrae, the number of heterozygous loci + the number of homozygous loci equal the number of loci genotyped (they should!)
sum(gen_div$HET_loci+gen_div$HOM_loci==gen_div$loci_genotyped)==nrow(gen_div) # TRUE!

# calculate the proportion of genotyped loci that were heterozygous (i.e. HET_loci divided by loci_genotyped) and put in new column called heterozygosity
gen_div$heterozygosity <- gen_div$HET_loci/gen_div$loci_genotyped

# merge with groom data set to check that what we calculated here is equal to the heterozygosity estimates in the groom dataframe
# first, merge the groom dataframe with the female heterozygosity estimates
tmp <- merge(groom, gen_div[c(1,19)], by.x=c("female_id"), by.y=c("id"), all.x=T)
colnames(tmp)[ncol(tmp)] <- c("heterozyosity_female2")
# second, merge the groom dataframe with the male heterozygosity estimates
tmp2 <- merge(tmp, gen_div[c(1,19)], by.x=c("male_id"), by.y=c("id"), all.x=T)
colnames(tmp2)[ncol(tmp2)] <- c("heterozyosity_male2")

# female_heterozygosity2 should equal the female_heterozygosity2
sum(tmp2$heterozyosity_female2==tmp2$heterozyosity_female)==nrow(tmp2) # TRUE - these columns match!

# male_heterozygosity2 should equal the female_heterozygosity2
sum(tmp2$heterozyosity_male2==tmp2$heterozyosity_male)==nrow(tmp2) # TRUE - these columns match!


#############################################################################################################################
# Relatedness
#############################################################################################################################

# Now calculate Queller-Goodnight Estimate of Relatedness


# subset geno data such that it only contains sname and genotype data for our study subjects (this is new as of 29-June-2020)
tmp <- merge(geno, females, by.x=c("sname"), by.y=c("female_sname"))
tmp2 <- merge(geno, males, by.x=c("sname"), by.y=c("male_sname"))
QG_data <- rbind(tmp, tmp2)

write.table(QG_data,"QG_data", sep="\t", row.names=FALSE, col.names = FALSE)
QG_data = read.delim("QG_data", header=F)
QG_data$V1 <- as.character(QG_data$V1)
QG_data[is.na(QG_data)] <- 0 # change all NAs to 0

summary(QG_data)

input <- readgenotypedata(QG_data)
input$nalleles

# point estimates of relatedness
pt_results_1 <- coancestry(input$gdata, quellergt = 1)
pt_results_1_est <- pt_results_1$relatedness

QG_1 <- pt_results_1_est[c(2:3,10)]
QG_1$pair1 <- paste(QG_1$ind1.id, QG_1$ind2.id, sep="_")
QG_1$pair2 <- paste(QG_1$ind2.id, QG_1$ind1.id, sep="_")

QG_final <- subset(QG_1, select=c("quellergt", "pair1"))

QG_final2 <- subset(QG_1, select=c("quellergt", "pair2"))
names(QG_final2)[names(QG_final2) == "pair2"] <- "pair1"

# get stats on loci genotyped across all subjects
tmp <- QG_data[c(1)] # get list of all study subjects
tmp2 <- merge(tmp, geno_gen_div, by.x=c("V1"), by.y=c("sname"))
mean(tmp2$loci_genotyped) #13.13176       
sd(tmp2$loci_genotyped) #1.215523

#############################################################################################################################
# Female transformed age
#############################################################################################################################

# Calculate a transformed measure of female age (following Tung et al. 2012) that reflects the relationship between female age and conception probability in the Amboseli baboon population where the highest conception probability occurs at ~14 years of age (Beehner et al. 2006 - https://doi.org/10.1093/beheco/arl006)
# See equation in methods

# calculate female transformed age as a function of the untransformed female age (transformed female age is given 0 at an untransformed female age of 14; transformed female age becomes more negative with deviations from untransformed female age of 14) and put results in new column called female_transformed_age2
groom$female_age_transformed2 <- -1*(((groom$female_age - 14)/14)^2)

# female_age_transformed2 should equal the female_age_transformed2 column in the groom dataframe
sum(groom$female_age_transformed2==groom$female_age_transformed)==nrow(groom) # TRUE - these columns match!
