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

# Figure 1. Genetic ancestry and dominance rank predict the tendency to groom with an opposite-sex partner.
## Fig. 1A

## Fig. 1B
## For the dashed line which shows the predicted relationship between male dominance rank and grooming based on model estimates, assuming average values for all other covariates
## Get the grooming probability for male_rank_avg given an average female social partner in an average demographic environment
## Note that we exclude m_rank_avg and female_sname and male_sname are coded as NA
groomm_r <- data.frame(females_in_grp_avg=mean(groom_final_6$females_in_grp_avg), males_in_grp_avg=mean(groom_final_6$males_in_grp_avg), female_rank_avg=mean(groom_final_6$female_rank_avg), female_hybridscore_new=mean(groom_final_6$female_hybridscore_new), gen_diversity_male=mean(groom_final_6$gen_diversity_male), gen_diversity_female=mean(groom_final_6$gen_diversity_female), QG_final=mean(groom_final_6$QG_final), observer_effort_two_months=mean(groom_final_6$observer_effort_two_months), female_age_avg_minus1=mean(groom_final_6$female_age_avg_minus1), female_age_transform_with_avg=mean(groom_final_6$female_age_transform_with_avg), reproductive_state_binary=1, sum_female_male_co_residency_count=mean(groom_final_6$sum_female_male_co_residency_count), male_hybridscore_new=mean(groom_final_6$male_hybridscore_new), assortative_max_new=mean(groom_final_6$assortative_max_new), female_sname=NA, male_sname=NA)
## Get the range of empirical values for male rank
m_rank <- as.data.frame(seq(min(groom_final_6$male_rank_avg),max(groom_final_6$male_rank_avg), by=1))
colnames(m_rank) <- c("male_rank_avg")

tmp5 <- merge(groomm_r,m_rank)

## Get the grooming probability for the range of male rank values assuming average values for all other covaries
tmp5$probs <- predict(tmp, tmp5, type="response", re.form=NA)

## For the colored dots which show probabilities based on counts of grooming occurrences
raw <- groom %>% group_by(male_rank_avg) %>% mutate(count_groom=dplyr::n()) 
raw2 <- raw %>% group_by(male_rank_avg, count_groom) %>% mutate(yes_groom=sum(groom_two_month))
raw3 <- distinct(raw2, male_rank_avg, count_groom, yes_groom)
raw3$groom_prox <- raw3$yes_groom/raw3$count_groom

## Plot figure 1A
ggplot() + geom_line(data=tmp5, aes(male_rank_avg, probs), color="black", size=1, linetype="dashed") + geom_jitter(data=groom, aes(male_rank_avg, groom_two_month), alpha=0.05, height=0.05, color="grey70") + scale_x_continuous(name="male rank",breaks=c(1,5,10,15,20,25,30)) +  theme_classic() + theme(text=element_text(size=20), axis.text = element_text(color="black")) + geom_point(data=raw3, aes(male_rank_avg, groom_prox), size=3, color="turquoise4") + scale_y_continuous(name="grooming probability") 


## Fig. 1C

## Fig. 1D
