#!/usr/bin/env Rscript

# Script for preparing for permutations to assess statistical significance of predictor variables in the multivariate logistic regression models predicting grooming and proximity behavior
# This script computes, for each female-two month interval combination, the proportion of dyads where an event (grooming or proximity) occurred. These values are estimates of the probability of grooming or proximity with any male, per female-interval combination.

# Load the grooming data set available at XXX
groom <- read.csv("groom_anonymized.csv", header=T) 

# Load the proximity data set available at XXX
prox <- read.csv("prox_anonymized.csv", header=T) 

# Calculate the proportion of dyads within each female-two month interval for which grooming/proximity occurred (i.e., an estimate of the grooming/proximity probability)
# Need to create a female-two month interval id
groom$female_interval_id <- paste(groom$female_id, groom$interval_id, sep="_") # paste female id with two month interval id to get a female-two month interval id

groom_null_1 <- groom %>% group_by(female_interval_id) %>% mutate(count=dplyr::n()) # count the potential set of grooming pairs in a given female_interval_id (i.e. all potential male grooming partners in a female's given social group in a given two month interval) and put in new column titled "count"
groom_null_2 <- groom_null_1 %>% group_by(female_interval_id) %>% mutate(groom_count=sum(groom_two_month)) # sum the total number of pairs that groomed in a given female_interval_id (put in new column titled "groom_count")
groom_null_2$groom_prob <- groom_null_2$groom_count/groom_null_2$count # and then calculate the proportion of female-male pairs grooming in a given female_interval_id (put in new column titled "groom_prob" <- this is the grooming probability)

summary(groom_null_2$groom_prob)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0500  0.1000  0.1429  0.1780  0.2222  1.0000 

# proximity is same as groom_null_1 and groom_null_2 above but with proximity
prox_null_1 <- prox %>% group_by(female_interval_id) %>% mutate(count=dplyr::n()) # count = number of rows in a female_interval_id i.e. number of female_male pairs for a female in a given two month interval
prox_null_2 <- prox_null_1 %>% group_by(female_interval_id) %>% mutate(prox_count=sum(prox_two_month))
prox_null_2$prox_prob <- prox_null_2$prox_count/prox_null_2$count

summary(prox_null_2$prox_prob)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.05263 0.12500 0.22222 0.27974 0.36842 1.00000 

#---SOME IMPORTANT CHECKS ON THE DATA---
# note that just grouping by female_interval_id should in theory be the same as grouping by female_interval_id, grp, and reproductive_state (i.e. none of these should be changing within a given female_interval_id) --> check this!

# first add grp to female_interval_id grouping
groom_null_1a <- groom %>% group_by(female_interval_id,grp) %>% mutate(count=dplyr::n()) # count = number of rows in a female_interval_id i.e. number of female_male pairs for a female in a given two month interval
groom_null_2a <- groom_null_1a %>% group_by(female_interval_id,grp) %>% mutate(groom_count=sum(groom_two_month))
groom_null_2a$groom_prob <- groom_null_2a$groom_count/groom_null_2a$count

summary(groom_null_2a$groom_prob)==summary(groom_null_2$groom_prob) # same as groom_null_2

# also check
all(groom_null_2$groom_prob==groom_null_2a$groom_prob) # True!

# now add reproductive_state to female_interval_id grouping
groom_null_1b <- groom %>% group_by(female_interval_id,reproductive_state) %>% mutate(count=dplyr::n()) # count = number of rows in a female_interval_id i.e. number of female_male pairs for a female in a given two month interval
groom_null_2b <- groom_null_1b %>% group_by(female_interval_id,reproductive_state) %>% mutate(groom_count=sum(groom_two_month))
groom_null_2b$groom_prob <- groom_null_2b$groom_count/groom_null_2b$count

summary(groom_null_2b$groom_prob)==summary(groom_null_2$groom_prob)  # same as groom_null_2 and 2a

# also check
all(groom_null_2$groom_prob==groom_null_2b$groom_prob) # True!
all(groom_null_2a$groom_prob==groom_null_2b$groom_prob) # True!

# now add grp and reproductive_state to female_interval_id grouping
groom_null_1c <- groom %>% group_by(female_interval_id,grp,reproductive_state) %>% mutate(count=dplyr::n()) # count = number of rows in a female_interval_id i.e. number of female_male pairs for a female in a given two month interval
groom_null_2c <- groom_null_1c %>% group_by(female_interval_id,grp,reproductive_state) %>% mutate(groom_count=sum(groom_two_month))
groom_null_2c$groom_prob <- groom_null_2c$groom_count/groom_null_2c$count

summary(groom_null_2c$groom_prob)==summary(groom_null_2$groom_prob)  # same as groom_null_2, 2a, 2b

all(groom_null_2c$groom_prob==groom_null_2$groom_prob) # True!

# Now do same check with proximity as done with grooming
# first add grp to female_interval_id grouping
prox_null_1a <- prox %>% group_by(female_interval_id,grp) %>% mutate(count=dplyr::n()) # count = number of rows in a female_interval_id i.e. number of female_male pairs for a female in a given two month interval
prox_null_2a <- prox_null_1a %>% group_by(female_interval_id,grp) %>% mutate(prox_count=sum(prox_two_month))
prox_null_2a$prox_prob <- prox_null_2a$prox_count/prox_null_2a$count

# check
all(prox_null_2$prox_prob==prox_null_2a$prox_prob) # True!

# now add reproductive_state to female_interval_id grouping
prox_null_1b <- prox %>% group_by(female_interval_id,reproductive_state) %>% mutate(count=dplyr::n()) # count = number of rows in a female_interval_id i.e. number of female_male pairs for a female in a given two month interval
prox_null_2b <- prox_null_1b %>% group_by(female_interval_id,reproductive_state) %>% mutate(prox_count=sum(prox_two_month))
prox_null_2b$prox_prob <- prox_null_2b$prox_count/prox_null_2b$count

# check
all(prox_null_2$prox_prob==prox_null_2b$prox_prob) # True!

# now add grp and reproductive_state to female_interval_id grouping
prox_null_1c <- prox %>% group_by(female_interval_id,grp,reproductive_state) %>% mutate(count=dplyr::n()) # count = number of rows in a female_interval_id i.e. number of female_male pairs for a female in a given two month interval
prox_null_2c <- prox_null_1c %>% group_by(female_interval_id,grp,reproductive_state) %>% mutate(prox_count=sum(prox_two_month))
prox_null_2c$prox_prob <- prox_null_2c$prox_count/prox_null_2c$count

all(prox_null_2$prox_prob==prox_null_2c$prox_prob) # True!

summary(prox_null_2$prox_prob)==summary(prox_null_2a$prox_prob) #TRUE
summary(prox_null_2$prox_prob)==summary(prox_null_2b$prox_prob) #TRUE
summary(prox_null_2$prox_prob)==summary(prox_null_2c$prox_prob) #TRUE

fem_uniq_id_uniq_p <- unique(prox_null_2[c("female_interval_id","prox_prob")])
nrow(fem_uniq_id_uniq_p) # 2338
fem_uniq_id_uniq_g <- unique(groom_null_2[c("female_interval_id","groom_prob")])
nrow(fem_uniq_id_uniq_g) # 1866

# also good to look at the distribution of probabilities
round(summary(fem_uniq_id_uniq_p$prox_prob)*100,1)
round(summary(fem_uniq_id_uniq_g$groom_prob)*100,1)

# get the files you to HARDAC
save(fem_uniq_id_uniq_p,prox_null_2,file="prox_perm_6Oct2020.Rd")
save(fem_uniq_id_uniq_g,groom_null_2,file="groom_perm_6Oct2020.Rd")

# also, save the full data frames if you need them later
save(prox,groom,file="groom_prox_final_6_6Oct2020.Rd")

#PERMUTATION RESULTS FROM HARDAC!


# grooming
# load results from all 1,000 permutations using glmmTMB
perm_groom1_repstate_recoded_glmmtmb <- read.table("all.6Oct2020.GLMMTMB_permuted_results_groom1_noC_repstate_recoded_newhs.txt", header=F) # pregnancy=-1
perm_groom2_repstate_recoded_glmmtmb <- read.table("all.6Oct2020.GLMMTMB_permuted_results_groom2_noC_repstate_recoded_newhs.txt", header=F) # lactation=-1

groom_results1_glmmtmb <-c()
groom_results2_glmmtmb <-c()

for (i in 1:ncol(perm_groom1_repstate_recoded_glmmtmb)) {
  groom_results1_glmmtmb[i] <- (sum(abs(unlist(estimates_groom_glmmtmb1)[i]) < abs(perm_groom1_repstate_recoded_glmmtmb[,i])))/1000
  groom_results2_glmmtmb[i] <- (sum(abs(unlist(estimates_groom_glmmtmb2)[i]) < abs(perm_groom2_repstate_recoded_glmmtmb[,i])))/1000
}

sig_results_final_groom_glmmtmb <- as.data.frame(cbind(names(estimates_groom_glmmtmb1$cond),unlist(estimates_groom_glmmtmb1, use.names=F), groom_results1_glmmtmb, groom_results2_glmmtmb))
colnames(sig_results_final_groom_glmmtmb)[1] <- c("variable")
colnames(sig_results_final_groom_glmmtmb)[2] <- c("beta")

sig_results_final_groom_glmmtmb$beta <- as.numeric(as.character(sig_results_final_groom_glmmtmb$beta))
sig_results_final_groom_glmmtmb$groom_results1_glmmtmb <- as.numeric(as.character(sig_results_final_groom_glmmtmb$groom_results1_glmmtmb))
sig_results_final_groom_glmmtmb$groom_results2_glmmtmb <- as.numeric(as.character(sig_results_final_groom_glmmtmb$groom_results2_glmmtmb))
sig_results_final_groom_glmmtmb$beta_round <- round(sig_results_final_groom_glmmtmb$beta,3)

# double checking for loop and specifically, results for female_hybridscore
(sum(abs(unlist(estimates_groom_glmmtmb1)[3]) < abs(perm_groom1_repstate_recoded_glmmtmb[3])))/1000
(sum(abs(unlist(estimates_groom_glmmtmb2)[3]) < abs(perm_groom2_repstate_recoded_glmmtmb[3])))/1000

ggplot(data=sig_results_final_groom_glmmtmb) + geom_point(aes(variable, beta), size=3) + theme_classic() + theme(axis.text.x = element_text(angle=90))

ggplot(data=sig_results_final_groom_glmmtmb) + geom_point(aes(variable, groom_results1_glmmtmb), color="blue", alpha=0.3, size=3) + geom_point(aes(variable, groom_results2_glmmtmb), color="red", alpha=0.3, size=3) + theme_classic() + theme(axis.text.x = element_text(angle=90))

# Proximity
# load results from all 1,000 permutations using glmmTMB
perm_prox1_repstate_recoded_glmmtmb <- read.table("all.6Oct2020.GLMMTMB_permuted_results_prox1_noC_repstate_recoded_newhs.txt", header=F) # pregnancy=-1
perm_prox2_repstate_recoded_glmmtmb <- read.table("all.6Oct2020.GLMMTMB_permuted_results_prox2_noC_repstate_recoded_newhs.txt", header=F) # lactation=-1

prox_results1_glmmtmb <-c()
prox_results2_glmmtmb <-c()

for (i in 1:ncol(perm_prox1_repstate_recoded_glmmtmb)) {
  prox_results1_glmmtmb[i] <- (sum(abs(unlist(estimates_prox_glmmtmb1)[i]) < abs(perm_prox1_repstate_recoded_glmmtmb[,i])))/1000
  prox_results2_glmmtmb[i] <- (sum(abs(unlist(estimates_prox_glmmtmb2)[i]) < abs(perm_prox2_repstate_recoded_glmmtmb[,i])))/1000
}

sig_results_final_prox_glmmtmb <- as.data.frame(cbind(names(estimates_prox_glmmtmb1$cond),
                                                      unlist(estimates_prox_glmmtmb1, use.names=F), prox_results1_glmmtmb, prox_results2_glmmtmb))
colnames(sig_results_final_prox_glmmtmb)[1] <- c("variable")
colnames(sig_results_final_prox_glmmtmb)[2] <- c("beta")

sig_results_final_prox_glmmtmb$beta <- as.numeric(as.character(sig_results_final_prox_glmmtmb$beta))
sig_results_final_prox_glmmtmb$prox_results1_glmmtmb <- as.numeric(as.character(sig_results_final_prox_glmmtmb$prox_results1_glmmtmb))
sig_results_final_prox_glmmtmb$prox_results2_glmmtmb <- as.numeric(as.character(sig_results_final_prox_glmmtmb$prox_results2_glmmtmb))
sig_results_final_prox_glmmtmb$beta_round <- round(sig_results_final_prox_glmmtmb$beta,3)

# double checking for loop and specifically, results for female_hybridscore
(sum(abs(unlist(estimates_prox_glmmtmb1)[3]) < abs(perm_prox1_repstate_recoded_glmmtmb[3])))/1000 
(sum(abs(unlist(estimates_prox_glmmtmb2)[3]) < abs(perm_prox2_repstate_recoded_glmmtmb[3])))/1000 


ggplot(data=sig_results_final_prox_glmmtmb) + geom_point(aes(variable, beta), size=3) + theme_classic() + theme(axis.text.x = element_text(angle=90))

ggplot(data=sig_results_final_prox_glmmtmb) + geom_point(aes(variable, prox_results1_glmmtmb), color="blue", alpha=0.3, size=3) + geom_point(aes(variable, prox_results2_glmmtmb), color="red", alpha=0.3, size=3) + theme_classic() + theme(axis.text.x = element_text(angle=90))

# compare both grooming and proximity on same plot
ggplot() + geom_point(data=sig_results_final_prox_glmmtmb, aes(variable, beta), color="forestgreen", alpha=0.7, size=3) + geom_point(data=sig_results_final_groom_glmmtmb, aes(variable, beta), color="orange", alpha=0.8, size=3) + theme_classic() + theme(axis.text.x = element_text(angle=90))

# plot betas that are sigificant in both analyses
ggplot() + theme_classic() + theme(axis.text.x = element_text(angle=90)) + geom_point(data=subset(sig_results_final_groom_glmmtmb, groom_results1_glmmtmb<0.05), aes(variable, beta), color="orange", alpha=0.8, size=3) + geom_point(data=subset(sig_results_final_prox_glmmtmb, prox_results1_glmmtmb<0.05), aes(variable, beta), color="forestgreen", alpha=0.7, size=3)

sig_results_final_groom_glmmtmb_newhs <- sig_results_final_groom_glmmtmb
sig_results_final_prox_glmmtmb_newhs <- sig_results_final_prox_glmmtmb

colnames(sig_results_final_groom_glmmtmb_newhs) <- c("variable_newhs", "beta_newhs_glmmtmb", "groom_results1_glmmtmb_newhs", "groom_results2_glmmtmb_newhs",  "beta_newhs_round_glmmtmb")

colnames(sig_results_final_prox_glmmtmb_newhs) <- c("variable_newhs", "beta_newhs_glmmtmb", "prox_results1_glmmtmb_newhs", "prox_results2_glmmtmb_newhs",  "beta_newhs_round_glmmtmb")

write.csv(sig_results_final_groom_glmmtmb_newhs, "6Oct2020.sig_results_final_groom_glmmtmb_newhs.csv", row.names=F)
write.csv(sig_results_final_prox_glmmtmb_newhs, "6Oct2020.sig_results_final_prox_glmmtmb_newhs.csv", row.names=F)

# join with original data results (redo from "Manuscript Code for Github" markdown file)
results_groom <- cbind(sig_results_final_groom_glmmtmb, sig_results_final_groom_glmmtmb_newhs)
results_prox <- cbind(sig_results_final_prox_glmmtmb, sig_results_final_prox_glmmtmb_newhs)

# plot original model results next to new model results using variables with new hybrid scores
# plot the betas
ggplot(data=results_groom) + geom_point(aes(variable, beta), size=3, color="blue", alpha=0.5) + geom_point(aes(variable, beta_newhs), size=3, color="green", alpha=0.5) + theme_classic() + theme(axis.text.x = element_text(angle=90))
ggplot(data=results_prox) + geom_point(aes(variable, beta), size=3, color="blue", alpha=0.5) + geom_point(aes(variable, beta_newhs), size=3, color="green", alpha=0.5) + theme_classic() + theme(axis.text.x = element_text(angle=90))

# plot the p-values
ggplot(data=results_groom) + geom_point(aes(variable, -log10(groom_results1_glmmtmb)), size=3, color="blue", alpha=0.5) + geom_point(aes(variable, -log10(groom_results2_glmmtmb)), size=3, color="steelblue3", alpha=0.5) + geom_point(aes(variable, -log10(groom_results1_glmmtmb_newhs)), size=3, color="green", alpha=0.5) + geom_point(aes(variable, -log10(groom_results2_glmmtmb_newhs)), size=3, color="seagreen3", alpha=0.5) + theme_classic() + theme(axis.text.x = element_text(angle=90)) + scale_y_continuous(name="-log10(p-value from permutations)", limits = c(0,3)) + geom_hline(yintercept=-log10(0.01), linetype="dashed") + geom_hline(yintercept=-log10(0.05), linetype="dotted")

ggplot(data=results_prox) + geom_point(aes(variable, -log10(prox_results1_glmmtmb)), size=3, color="blue", alpha=0.5) + geom_point(aes(variable, -log10(prox_results2_glmmtmb)), size=3, color="steelblue3", alpha=0.5) + geom_point(aes(variable, -log10(prox_results1_glmmtmb_newhs)), size=3, color="green", alpha=0.5) + geom_point(aes(variable, -log10(prox_results2_glmmtmb_newhs)), size=3, color="seagreen3", alpha=0.5) + theme_classic() + theme(axis.text.x = element_text(angle=90)) + scale_y_continuous(name="-log10(p-value from permutations)") + geom_hline(yintercept=-log10(0.01), linetype="dashed") + geom_hline(yintercept=-log10(0.05), linetype="dotted")
