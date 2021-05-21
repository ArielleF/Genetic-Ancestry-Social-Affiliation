#!/usr/bin/env Rscript

# Load the grooming and proximity data sets available at XXX
groom <- read.csv(, header=T)
prox <- read.csv(, header=T)

# Load R libraries
library(ggplot2)
library(glmmTMB)

# Need to run original grooming and proximity models for some of the figure making
# Original model
tmp <- 
tmp2 <-

set.seed(1234) # so figures with any jittering are reproducible

#############################################################################################################################
# Figure 1. Genetic ancestry and dominance rank predict the tendency to groom with an opposite-sex partner.
#############################################################################################################################

####################
# Fig. 1A
####################
# Calculate the probability of grooming among co-resident opposite-sex pairs, per two-month interval, for the most anubis-like males (above the 90th percentile for male genetic ancestry in the data set) and the most yellow-like males (below the 10th percentile for male genetic ancestry in the data set) without adjustment for other covariates
# Use the 90th and 10th percentiles across rows in the data sets
abovemed_1 <- subset(groom, male_hybridscore_new>quantile(groom$male_hybridscore_new, 0.9))
abovemed_1$set <- "above_90"
abovemed_2 <- abovemed_1 %>% group_by(male_sname) %>% mutate(count_behav=dplyr::n()) 
abovemed_3 <- abovemed_2 %>% group_by(male_sname, count_behav) %>% mutate(yes_behav=sum(groom_two_month))
abovemed_3$prop_behav <- abovemed_3$yes_behav/abovemed_3$count_behav
abovemed_3 <- abovemed_3[!duplicated(abovemed_3$male_sname),]
nrow(abovemed_3)

belowmed_1 <- subset(groom, male_hybridscore_new<quantile(groom$male_hybridscore_new, 0.1))
belowmed_1$set <- "below_10"
belowmed_2 <- belowmed_1 %>% group_by(male_sname) %>% mutate(count_behav=dplyr::n()) 
belowmed_3 <- belowmed_2 %>% group_by(male_sname, count_behav) %>% mutate(yes_behav=sum(groom_two_month))
belowmed_3$prop_behav <- belowmed_3$yes_behav/belowmed_3$count_behav
belowmed_3 <- belowmed_3[!duplicated(belowmed_3$male_sname),]
nrow(belowmed_3)

tmp2 <- rbind(abovemed_3, belowmed_3)

ggplot(data=tmp2) + geom_violin(aes(x = set, y = prop_behav, color=set, fill=set),  alpha = 0.4) +  geom_jitter(aes(x = set, y = prop_behav, color=set, size=5)) + geom_boxplot(aes(x = set, y = prop_behav),  size=1.25, width=0.1, color="black", fill="white", outlier.colour = NA) + scale_y_continuous(name="grooming probability") + scale_x_discrete(labels=c("above_90"= "most anubis-like", "below_10" = "most yellow-like")) + theme_classic() + theme(legend.position = "none", legend.title = element_blank(), axis.title.x = element_blank(),text=element_text(size=28), axis.text = element_text(color="black")) + scale_color_manual(values = c("above_90" = "turquoise4", "below_10" = "turquoise4")) + scale_fill_manual(values = c("above_90" = "turquoise4", "below_10" = "turquoise4"))


####################
# Fig. 1B
####################
# For the dashed line which shows the predicted relationship between male dominance rank and grooming based on model estimates, assuming average values for all other covariates
# Get the grooming probability for male rank given an average female social partner in an average demographic environment
# Note that we exclude male rank and female id and male id are coded as NA
groomm_r <- data.frame(females_in_grp_avg=mean(groom_final_6$females_in_grp_avg), males_in_grp_avg=mean(groom_final_6$males_in_grp_avg), female_rank_avg=mean(groom_final_6$female_rank_avg), female_hybridscore_new=mean(groom_final_6$female_hybridscore_new), gen_diversity_male=mean(groom_final_6$gen_diversity_male), gen_diversity_female=mean(groom_final_6$gen_diversity_female), QG_final=mean(groom_final_6$QG_final), observer_effort_two_months=mean(groom_final_6$observer_effort_two_months), female_age_avg_minus1=mean(groom_final_6$female_age_avg_minus1), female_age_transform_with_avg=mean(groom_final_6$female_age_transform_with_avg), reproductive_state_binary=1, sum_female_male_co_residency_count=mean(groom_final_6$sum_female_male_co_residency_count), male_hybridscore_new=mean(groom_final_6$male_hybridscore_new), assortative_max_new=mean(groom_final_6$assortative_max_new), female_sname=NA, male_sname=NA)
# Get the range of empirical values for male rank
m_rank <- as.data.frame(seq(min(groom_final_6$male_rank_avg),max(groom_final_6$male_rank_avg), by=1))
colnames(m_rank) <- c("male_rank_avg")

tmp5 <- merge(groomm_r,m_rank)

# Get the grooming probability for the range of male rank values assuming average values for all other covaries
tmp5$probs <- predict(tmp, tmp5, type="response", re.form=NA)

# For the colored dots which show probabilities based on counts of grooming occurrences
raw <- groom %>% group_by(male_rank_avg) %>% mutate(count_groom=dplyr::n()) 
raw2 <- raw %>% group_by(male_rank_avg, count_groom) %>% mutate(yes_groom=sum(groom_two_month))
raw3 <- distinct(raw2, male_rank_avg, count_groom, yes_groom)
raw3$groom_prox <- raw3$yes_groom/raw3$count_groom

# Plot figure 1A
ggplot() + geom_line(data=tmp5, aes(male_rank_avg, probs), color="black", size=1, linetype="dashed") + geom_jitter(data=groom, aes(male_rank_avg, groom_two_month), alpha=0.05, height=0.05, color="grey70") + scale_x_continuous(name="male rank",breaks=c(1,5,10,15,20,25,30)) +  theme_classic() + theme(text=element_text(size=20), axis.text = element_text(color="black")) + geom_point(data=raw3, aes(male_rank_avg, groom_prox), size=3, color="turquoise4") + scale_y_continuous(name="grooming probability") 


####################
# Fig. 1C
####################
# Calculate the probability of grooming among co-resident opposite-sex pairs, per two-month interval, for the most anubis-like females (above the 90th percentile for female genetic ancestry in the data set) and the most yellow-like females (below the 10th percentile for female genetic ancestry in the data set) without adjustment for other covariates
# Use the 90th and 10th percentiles across rows in the data sets
abovemed_1 <- subset(groom, female_hybridscore_new>quantile(groom_final_6$female_hybridscore_new, 0.9))
abovemed_1$set <- "above_90"
abovemed_2 <- abovemed_1 %>% group_by(female_sname) %>% mutate(count_behav=dplyr::n()) 
abovemed_3 <- abovemed_2 %>% group_by(female_sname, count_behav) %>% mutate(yes_behav=sum(groom_two_month))
abovemed_3$prop_behav <- abovemed_3$yes_behav/abovemed_3$count_behav
abovemed_3 <- abovemed_3[!duplicated(abovemed_3$female_sname),]
nrow(abovemed_3)

belowmed_1 <- subset(groom, female_hybridscore_new<quantile(groom_final_6$female_hybridscore_new, 0.1))
belowmed_1$set <- "below_10"
belowmed_2 <- belowmed_1 %>% group_by(female_sname) %>% mutate(count_behav=dplyr::n()) 
belowmed_3 <- belowmed_2 %>% group_by(female_sname, count_behav) %>% mutate(yes_behav=sum(groom_two_month))
belowmed_3$prop_behav <- belowmed_3$yes_behav/belowmed_3$count_behav
belowmed_3 <- belowmed_3[!duplicated(belowmed_3$female_sname),]
nrow(belowmed_3)

tmp2 <- rbind(abovemed_3, belowmed_3)

ggplot(data=tmp2) + geom_violin(aes(x = set, y = prop_behav, color=set, fill=set),  alpha = 0.4) + geom_jitter(aes(x = set, y = prop_behav, color=set, size=5)) + geom_boxplot(aes(x = set, y = prop_behav), size=1.25, color="black", width=0.1, fill="white", outlier.colour = NA) + scale_y_continuous(name="grooming probability") + scale_x_discrete(labels=c("above_90"= "most anubis-like", "below_10" = "most yellow-like")) + theme_classic() + theme(legend.position = "none", legend.title = element_blank(), axis.title.x = element_blank(),text=element_text(size=28), axis.text = element_text(color="black")) + scale_color_manual(values = c("above_90" = "royalblue3", "below_10" = "royalblue3")) + scale_fill_manual(values = c("above_90" = "royalblue3", "below_10" = "royalblue3"))


####################
# Fig. 1D
####################
# For the dashed line which shows the predicted relationship between female dominance rank and grooming based on model estimates, assuming average values for all other covariates
# Get the grooming probability for female rank given an average male social partner in an average demographic environment
# Note that we exclude female rank and female id and male id are coded as NA
groomf_r <- data.frame(females_in_grp_avg=mean(groom_final_6$females_in_grp_avg), males_in_grp_avg=mean(groom_final_6$males_in_grp_avg), male_rank_avg=mean(groom_final_6$male_rank_avg), female_hybridscore_new=mean(groom_final_6$female_hybridscore_new), gen_diversity_male=mean(groom_final_6$gen_diversity_male), gen_diversity_female=mean(groom_final_6$gen_diversity_female), QG_final=mean(groom_final_6$QG_final), observer_effort_two_months=mean(groom_final_6$observer_effort_two_months), female_age_avg_minus1=mean(groom_final_6$female_age_avg_minus1), female_age_transform_with_avg=mean(groom_final_6$female_age_transform_with_avg), reproductive_state_binary=1, sum_female_male_co_residency_count=mean(groom_final_6$sum_female_male_co_residency_count), male_hybridscore_new=mean(groom_final_6$male_hybridscore_new), assortative_max_new=mean(groom_final_6$assortative_max_new), female_sname=NA, male_sname=NA)
# Get the range of empirical values for female rank
f_rank <- as.data.frame(seq(min(groom_final_6$female_rank_avg),max(groom_final_6$female_rank_avg), by=1))
colnames(f_rank) <- c("female_rank_avg")

tmp5 <- merge(groomf_r,f_rank)

# Get the grooming probability for the range of female rank values assuming average values for all other covaries
tmp5$probs <- predict(tmp, tmp5, type="response", re.form=NA)

# For the colored dots which show probabilities based on counts of grooming occurrences
raw <- groom %>% group_by(female_rank_avg) %>% mutate(count_groom=dplyr::n()) 
raw2 <- raw %>% group_by(female_rank_avg, count_groom) %>% mutate(yes_groom=sum(groom_two_month))
raw3 <- distinct(raw2, female_rank_avg, count_groom, yes_groom)
raw3$groom_prox <- raw3$yes_groom/raw3$count_groom

# Plot figure 1A
ggplot() + geom_jitter(data=groom, aes(female_rank_avg, groom_two_month), alpha=0.05, height=0.05, color="grey70") + scale_x_continuous(name="female rank",breaks=c(1,5,10,15,20,25,30)) +  theme_classic() + theme(text=element_text(size=20), axis.text = element_text(color="black")) + geom_point(data=raw3, aes(female_rank_avg, groom_prox), size=3, colour="royalblue3") + scale_y_continuous(name="grooming probability") + geom_line(data=tmp5, aes(female_rank_avg, probs), color="black", size=1, linetype="dashed") 


#############################################################################################################################
# Figure 2. Combined genetic ancestry characteristics of females and males affect the probability of grooming. 
#############################################################################################################################

####################
# Fig. 2 central heatmap
####################

####################
# Fig. 2 line graps surrounding heatmap
####################

#############################################################################################################################
# Figure 3. Combined rank characteristics of females and males affect the probability of grooming.
#############################################################################################################################

####################
# Fig. 3 central heatmap
####################

####################
# Fig. 4 line graps surrounding heatmap
####################

