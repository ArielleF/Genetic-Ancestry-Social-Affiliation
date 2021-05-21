#!/usr/bin/env Rscript

# Scripts for producing Figures 1-3 and S5-6
# The scripts for Figures S2-S4, S5c-d, S6c-d are not included here as they are identical to the scripts for Figures 1-3, S5, S6 except the dataset (groom) is replaced by the proximity data set (also availble at XXX) and the model used for predictions is the proximity logistic regression model
# Load the grooming data set available at XXX
groom <- read.csv(, header=T) # for Figures S2-4, load the proximity data set instead

# Load R libraries
library(ggplot2)
library(glmmTMB)

# Need to run original grooming logistic regression model for some of the figure making
# Original model (see scripts in Models directory)
tmp <- glmmTMB(groom_two_month ~ male_hybridscore_new + female_hybridscore_new + females_in_grp_avg + males_in_grp_avg + female_age_avg_minus1 + female_rank_avg + male_rank_avg + female_rank_avg*male_rank_avg + female_age_transform_with_avg + reproductive_state_binary + assortative_max_new + male_hybridscore_new*reproductive_state_binary + female_hybridscore_new*reproductive_state_binary + observer_effort_two_months + gen_diversity_male + gen_diversity_female + QG_final + sum_female_male_co_residency_count + (1 | female_sname) + (1 | male_sname), data = groom, family = "binomial") # for Figures S2-4, run the proximity logistic regression model instead (see scripts in Models directory)

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

# Plot figure 1B
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

# Plot figure 1C
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

# Plot figure 1D
ggplot() + geom_jitter(data=groom, aes(female_rank_avg, groom_two_month), alpha=0.05, height=0.05, color="grey70") + scale_x_continuous(name="female rank",breaks=c(1,5,10,15,20,25,30)) +  theme_classic() + theme(text=element_text(size=20), axis.text = element_text(color="black")) + geom_point(data=raw3, aes(female_rank_avg, groom_prox), size=3, colour="royalblue3") + scale_y_continuous(name="grooming probability") + geom_line(data=tmp5, aes(female_rank_avg, probs), color="black", size=1, linetype="dashed") 


#############################################################################################################################
# Figure 2. Combined genetic ancestry characteristics of females and males affect the probability of grooming. 
#############################################################################################################################

####################
# Fig. 2 central heatmap
####################
# Get the probability of grooming behavior as a function of female genetic ancestry and male genetic ancestry, based on model estimates assuming average values for all other covariates
tmp5 <- data.frame(females_in_grp_avg=mean(groom_final_6$females_in_grp_avg), males_in_grp_avg=mean(groom_final_6$males_in_grp_avg), female_rank_avg=mean(groom_final_6$female_rank_avg), male_rank_avg=mean(groom_final_6$male_rank_avg),  gen_diversity_male=mean(groom_final_6$gen_diversity_male), gen_diversity_female=mean(groom_final_6$gen_diversity_female), QG_final=mean(groom_final_6$QG_final), observer_effort_two_months=mean(groom_final_6$observer_effort_two_months), female_age_avg_minus1=mean(groom_final_6$female_age_avg_minus1), female_age_transform_with_avg=mean(groom_final_6$female_age_transform_with_avg), reproductive_state_binary=1, sum_female_male_co_residency_count=mean(groom_final_6$sum_female_male_co_residency_count))

# Get all possible female genetic ancestry and male genetic ancestry values (these values can range continuously from 0 to 1 where where 0 corresponds to unadmixed yellow baboon ancestry and 1 corresponds to unadmixed anubis baboon ancestry)
f_genetic <- as.vector(seq(0,1, by=0.001))
m_genetic <- as.vector(seq(0,1, by=0.001))
# Get all pairwise combinations of female genetic ancestry and male genetic ancestry
tmp7 <- expand.grid(f_genetic, m_genetic)
colnames(tmp7) <- c("male_hybridscore_new", "female_hybridscore_new")
# Recalculate the assortative genetic ancestry index for all of these combinations
tmp7$assortative_1 <- (tmp7$male_hybridscore_new*tmp7$female_hybridscore_new)
tmp7$assortative_2 <- ((1-tmp7$male_hybridscore_new)*(1-tmp7$female_hybridscore_new))
tmp7$assortative_max_new <- apply(tmp7[, 3:4], 1, max)

tmp8 <- merge(tmp7, tmp5)
tmp8$female_sname <- NA
tmp8$male_sname <- NA

# Get the grooming probability for female genetic ancestry and male genetic ancestry based on model estimates assuming average values for all other covariates
tmp8$probs <- predict(tmp, tmp8, type="response", re.form=NA) 

# Plot figure 2 central heatmap
ggplot(tmp8, aes(x=female_hybridscore_new, y=male_hybridscore_new, fill=probs)) + geom_raster() + theme_classic() + scale_fill_gradientn(colours=c("blue","cyan","green", "yellow","orange", "red", "indianred4")) + scale_x_continuous(name="female genetic ancestry") + scale_y_continuous(name="male genetic ancestry") + theme(legend.position = "none", text=element_text(size=24), axis.text = element_text(color="black")) + guides(fill=guide_colorbar(ticks.colour = "black", ticks.linewidth = 1.5))

# For just the color legend - replot but use size 16 for color bar ticks
ggplot(tmp8, aes(x=female_hybridscore_new, y=male_hybridscore_new, fill=probs)) + geom_raster() + theme_classic() + scale_fill_gradientn(colours=c("blue","cyan","green", "yellow","orange", "red", "indianred4")) + scale_x_continuous(name="female genetic ancestry") + scale_y_continuous(name="male genetic ancestry") + theme(legend.position = "right", text=element_text(size=16), axis.text = element_text(color="black")) + guides(fill=guide_colorbar(ticks.colour = "black", ticks.linewidth = 1.5))

####################
# Fig. 2 line graps surrounding heatmap
####################
# Plot the probability of grooming behavior for yellow males as a function of the genetic ancestry of potential opposite-sex social partners (left top panel)
test <- subset(tmp8, male_hybridscore_new==0)
ggplot(test) + geom_line(aes(female_hybridscore_new, probs), size=2) + theme_classic()+ theme(legend.position = "none", text=element_text(size=24), axis.text = element_text(color="black")) + scale_x_continuous(name="female genetic ancestry") + scale_y_continuous(limits=c(min(tmp8$probs), max(tmp8$probs)), name="grooming probability")
#*****ARIELLE CHECK TO MAKE SURE THE MIN, MAX WORKS FOR THE LIMITS
# Plot the probability of grooming behavior for anubis males as a function of the genetic ancestry of potential opposite-sex social partners (left bottom panel)
test <- subset(tmp8, male_hybridscore_new==1)
ggplot(test) + geom_line(aes(female_hybridscore_new, probs), size=2) + theme_classic()+ theme(legend.position = "none", text=element_text(size=24), axis.text = element_text(color="black")) + scale_x_continuous(name="female genetic ancestry") + scale_y_continuous(limits=c(min(tmp8$probs), max(tmp8$probs)), name="grooming probability")
#*****ARIELLE CHECK TO MAKE SURE THE MIN, MAX WORKS FOR THE LIMITS

# Plot the probability of grooming behavior for yellow females as a function of the genetic ancestry of potential opposite-sex social partners (bottom left panel)
test <- subset(tmp8, female_hybridscore_new==1)
ggplot(test) + geom_line(aes(male_hybridscore_new, probs), size=2) + theme_classic()+ theme(legend.position = "none", text=element_text(size=24), axis.text = element_text(color="black")) + scale_x_continuous(name="male genetic ancestry") + scale_y_continuous(limits=c(min(tmp8$probs), max(tmp8$probs), name="grooming probability")
# Plot the probability of grooming behavior for anubis females as a function of the genetic ancestry of potential opposite-sex social partners (bottom right panel)
test <- subset(tmp8, female_hybridscore_new==0)
ggplot(test) + geom_line(aes(male_hybridscore_new, probs), size=2) + theme_classic()+ theme(legend.position = "none", text=element_text(size=24), axis.text = element_text(color="black")) + scale_x_continuous(name="male genetic ancestry") + scale_y_continuous(limits=c(min(tmp8$probs), max(tmp8$probs), name="grooming probability")


#############################################################################################################################
# Figure 3. Combined rank characteristics of females and males affect the probability of grooming.
#############################################################################################################################

####################
# Fig. 3 central heatmap
####################
# Get the probability of grooming behavior as a function of female dominance rank and male dominance rank, based on model estimates assuming average values for all other covariates
tmp9 <- data.frame(females_in_grp_avg=mean(groom_final_6$females_in_grp_avg), males_in_grp_avg=mean(groom_final_6$males_in_grp_avg), female_hybridscore_new=mean(groom_final_6$female_hybridscore_new), male_hybridscore_new=mean(groom_final_6$male_hybridscore_new),  gen_diversity_male=mean(groom_final_6$gen_diversity_male), gen_diversity_female=mean(groom_final_6$gen_diversity_female), QG_final=mean(groom_final_6$QG_final), observer_effort_two_months=mean(groom_final_6$observer_effort_two_months), female_age_avg_minus1=mean(groom_final_6$female_age_avg_minus1), female_age_transform_with_avg=mean(groom_final_6$female_age_transform_with_avg), reproductive_state_binary=1, assortative_max_new=mean(groom_final_6$assortative_max_new), sum_female_male_co_residency_count=mean(groom_final_6$sum_female_male_co_residency_count))

# Get all the empirical range of female dominance rank and male dominance rank values
f_rank <- as.vector(seq(min(groom_final_6$female_rank_avg),max(groom_final_6$female_rank_avg), by=1))
m_rank <- as.vector(seq(min(groom_final_6$male_rank_avg),max(groom_final_6$male_rank_avg), by=1))

# Get all pairwise combinations of female dominance rank and male dominance rank
tmp10 <- expand.grid(f_rank, m_rank)
colnames(tmp10) <- c("female_rank_avg", "male_rank_avg")

tmp11 <- merge(tmp10, tmp9)
tmp11$female_sname <- NA
tmp11$male_sname <- NA

# Get the grooming probability for female dominance rank and male dominance rank based on model estimates assuming average values for all other covariates
tmp11$probs <- predict(tmp, tmp11, type="response", re.form=NA)

# Plot figure 3 central heatmap
ggplot(tmp11, aes(x=female_rank_avg, y=male_rank_avg, fill=probs)) + geom_raster() + theme_classic() + scale_fill_gradientn(colours=c("blue","cyan","green", "yellow","orange", "red", "indianred4")) + scale_x_continuous(name="female rank",  breaks=c(1,5,10,15,20,25,30)) + scale_y_continuous(name="male rank", breaks=c(1,5,10,15,20,25,30)) + theme(legend.position = "none", text=element_text(size=24), axis.text = element_text(color="black")) + guides(fill=guide_colorbar(ticks.colour = "black", ticks.linewidth = 1.5))

# For just the color legend - replot but use size 16 for color bar ticks
ggplot(tmp11, aes(x=female_rank_avg, y=male_rank_avg, fill=probs)) + geom_raster() + theme_classic() + scale_fill_gradientn(colours=c("blue","cyan","green", "yellow","orange", "red", "indianred4")) + scale_x_continuous(name="female rank",  breaks=c(1,5,10,15,20,25,30)) + scale_y_continuous(name="male rank", breaks=c(1,5,10,15,20,25,30)) + theme(legend.position = "right", text=element_text(size=16), axis.text = element_text(color="black")) + guides(fill=guide_colorbar(ticks.colour = "black", ticks.linewidth = 1.5))


####################
# Fig. 3 line graps surrounding heatmap
####################
# Note: we represented rank using an ordinal approach, where the highest-ranking individual holds rank 1 and lower-ranking individuals occupy ranks of successively higher numbers
# Plot the probability of grooming behavior for the lowest ranking males as a function of the dominance rank of potential opposite-sex social partners (left top panel)
test <- subset(tmp11, max(male_rank_avg)==male_rank_avg) 
ggplot(test) + geom_line(aes(female_rank_avg, probs), size=2) + theme_classic()+ theme(legend.position = "none", text=element_text(size=24), axis.text = element_text(color="black")) + scale_x_continuous(name="female rank", breaks=c(1,5,10,15,20,25,30)) + scale_y_continuous(limits=c(0.17, 0.34), name="grooming probability")
# Plot the probability of grooming behavior for the highest ranking males as a function of the dominance rank of potential opposite-sex social partners (left bottom panel)
test <- subset(tmp11, min(male_rank_avg)==male_rank_avg) 
ggplot(test) + geom_line(aes(female_rank_avg, probs), size=2) + theme_classic()+ theme(legend.position = "none", text=element_text(size=24), axis.text = element_text(color="black")) + scale_x_continuous(name="female rank", breaks=c(1,5,10,15,20,25,30)) + scale_y_continuous(limits=c(0.17, 0.34), name="grooming probability")

# Plot the probability of grooming behavior for the lowest ranking females as a function of the dominance rank of potential opposite-sex social partners (bottom left panel)
test <- subset(tmp11, max(female_rank_avg)==female_rank_avg)
ggplot(test) + geom_line(aes(male_rank_avg, probs), size=2) + theme_classic()+ theme(legend.position = "none", text=element_text(size=24), axis.text = element_text(color="black")) + scale_x_continuous(name="male rank", breaks=c(1,5,10,15,20,25,30)) + scale_y_continuous(limits=c(0.17, 0.34), name="grooming probability")
# Plot the probability of grooming behavior for the highest ranking females as a function of the dominance rank of potential opposite-sex social partners (bottom right panel)
test <- subset(tmp11, min(female_rank_avg)==female_rank_avg)
ggplot(test) + geom_line(aes(male_rank_avg, probs), size=2) + theme_classic()+ theme(legend.position = "none", text=element_text(size=24), axis.text = element_text(color="black")) + scale_x_continuous(name="male rank", breaks=c(1,5,10,15,20,25,30)) + scale_y_continuous(limits=c(0.17, 0.34), name="grooming probability")


##***ARIELLE START ANNOTATION HERE
#############################################################################################################################
# Figure S5. The number of adult males present in a social group influences grooming and proximity behavior.
#############################################################################################################################

####################
# Fig. S5A
####################
abovemed_1 <- subset(groom, males_in_grp_avg>quantile(groom_final_6$males_in_grp_avg, 0.5))
abovemed_1$set <- "above_median"
abovemed_2 <- abovemed_1 %>% group_by(male_sname) %>% mutate(count_behav=dplyr::n()) 
abovemed_3 <- abovemed_2 %>% group_by(male_sname, count_behav) %>% mutate(yes_behav=sum(groom_two_month))
abovemed_3$prop_behav <- abovemed_3$yes_behav/abovemed_3$count_behav
abovemed_3 <- abovemed_3[!duplicated(abovemed_3$male_sname),]

belowmed_1 <- subset(groom, males_in_grp_avg<quantile(groom_final_6$males_in_grp_avg, 0.5))
belowmed_1$set <- "below_median"
belowmed_2 <- belowmed_1 %>% group_by(male_sname) %>% mutate(count_behav=dplyr::n()) 
belowmed_3 <- belowmed_2 %>% group_by(male_sname, count_behav) %>% mutate(yes_behav=sum(groom_two_month))
belowmed_3$prop_behav <- belowmed_3$yes_behav/belowmed_3$count_behav
belowmed_3 <- belowmed_3[!duplicated(belowmed_3$male_sname),]

tmp2 <- rbind(abovemed_3, belowmed_3)

ggplot(data=tmp2) + geom_jitter(aes(x = set, y = prop_behav, color=set, size=5), alpha=0.8) + geom_violin(aes(x = set, y = prop_behav, color=set, fill=set),  alpha = 0.4) + geom_boxplot(aes(x = set, y = prop_behav), size=1.25, color="black", width=0.1, fill="white", outlier.colour = NA) + scale_y_continuous(name="proportion of grooming occurrences\nout of all potential grooming opportunities")  + scale_x_discrete(labels=c("above_median"= "above median", "below_median" = "below median")) + theme_classic() + theme(legend.position = "none", legend.title = element_blank(), axis.title.x = element_blank(),text=element_text(size=30), axis.text = element_text(color="black")) + scale_color_manual(values = c("above_median" = "turquoise4", "below_median" = "turquoise4")) + scale_fill_manual(values = c("above_median" = "turquoise4", "below_median" = "turquoise4"))


####################
# Fig. S5B
####################
# grooming - female perspective
abovemed_1 <- subset(groom, males_in_grp_avg>quantile(groom_final_6$males_in_grp_avg, 0.5))
abovemed_1$set <- "above_median"
abovemed_2 <- abovemed_1 %>% group_by(female_sname) %>% mutate(count_behav=dplyr::n()) 
abovemed_3 <- abovemed_2 %>% group_by(female_sname, count_behav) %>% mutate(yes_behav=sum(groom_two_month))
abovemed_3$prop_behav <- abovemed_3$yes_behav/abovemed_3$count_behav
abovemed_3 <- abovemed_3[!duplicated(abovemed_3$female_sname),]

belowmed_1 <- subset(groom, males_in_grp_avg<quantile(groom_final_6$males_in_grp_avg, 0.5))
belowmed_1$set <- "below_median"
belowmed_2 <- belowmed_1 %>% group_by(female_sname) %>% mutate(count_behav=dplyr::n()) 
belowmed_3 <- belowmed_2 %>% group_by(female_sname, count_behav) %>% mutate(yes_behav=sum(groom_two_month))
belowmed_3$prop_behav <- belowmed_3$yes_behav/belowmed_3$count_behav
belowmed_3 <- belowmed_3[!duplicated(belowmed_3$female_sname),]

tmp2 <- rbind(abovemed_3, belowmed_3)

ggplot(data=tmp2) + geom_jitter(aes(x = set, y = prop_behav, color=set, size=5), alpha=0.8) + geom_violin(aes(x = set, y = prop_behav, color=set, fill=set),  alpha = 0.4) + geom_boxplot(aes(x = set, y = prop_behav), size=1.25, color="black", width=0.1, fill="white", outlier.colour = NA) + scale_y_continuous(name="proportion of grooming occurrences\nout of all potential grooming opportunities")  + scale_x_discrete(labels=c("above_median"= "above median", "below_median" = "below median")) + theme_classic() + theme(legend.position = "none", legend.title = element_blank(), axis.title.x = element_blank(),text=element_text(size=30), axis.text = element_text(color="black")) + scale_color_manual(values = c("above_median" = "royalblue3", "below_median" = "royalblue3")) + scale_fill_manual(values = c("above_median" = "royalblue3", "below_median" = "royalblue3"))


#############################################################################################################################
# Figure S6. The number of adult females present in a social group influences grooming but not proximity behavior.
#############################################################################################################################

####################
# Fig. S6A
####################
abovemed_1 <- subset(groom, females_in_grp_avg>quantile(groom_final_6$females_in_grp_avg, 0.5))
abovemed_1$set <- "above_median"
abovemed_2 <- abovemed_1 %>% group_by(male_sname) %>% mutate(count_behav=dplyr::n()) 
abovemed_3 <- abovemed_2 %>% group_by(male_sname, count_behav) %>% mutate(yes_behav=sum(groom_two_month))
abovemed_3$prop_behav <- abovemed_3$yes_behav/abovemed_3$count_behav
abovemed_3 <- abovemed_3[!duplicated(abovemed_3$male_sname),]

belowmed_1 <- subset(groom, females_in_grp_avg<quantile(groom_final_6$females_in_grp_avg, 0.5))
belowmed_1$set <- "below_median"
belowmed_2 <- belowmed_1 %>% group_by(male_sname) %>% mutate(count_behav=dplyr::n()) 
belowmed_3 <- belowmed_2 %>% group_by(male_sname, count_behav) %>% mutate(yes_behav=sum(groom_two_month))
belowmed_3$prop_behav <- belowmed_3$yes_behav/belowmed_3$count_behav
belowmed_3 <- belowmed_3[!duplicated(belowmed_3$male_sname),]

tmp2 <- rbind(abovemed_3, belowmed_3)

ggplot(data=tmp2) + geom_jitter(aes(x = set, y = prop_behav, color=set, size=5), alpha=0.8) + geom_violin(aes(x = set, y = prop_behav, color=set, fill=set),  alpha = 0.4) + geom_boxplot(aes(x = set, y = prop_behav), size=1.25, color="black", width=0.1, fill="white", outlier.colour = NA) + scale_y_continuous(name="proportion of grooming occurrences\nout of all potential grooming opportunities")  + scale_x_discrete(labels=c("above_median"= "above median", "below_median" = "below median")) + theme_classic() + theme(legend.position = "none", legend.title = element_blank(), axis.title.x = element_blank(),text=element_text(size=30), axis.text = element_text(color="black")) + scale_color_manual(values = c("above_median" = "turquoise4", "below_median" = "turquoise4")) + scale_fill_manual(values = c("above_median" = "turquoise4", "below_median" = "turquoise4"))

####################
# Fig. S6B
####################
# grooming - female perspective
abovemed_1 <- subset(groom, females_in_grp_avg>quantile(groom_final_6$females_in_grp_avg, 0.5))
abovemed_1$set <- "above_median"
abovemed_2 <- abovemed_1 %>% group_by(female_sname) %>% mutate(count_behav=dplyr::n()) 
abovemed_3 <- abovemed_2 %>% group_by(female_sname, count_behav) %>% mutate(yes_behav=sum(groom_two_month))
abovemed_3$prop_behav <- abovemed_3$yes_behav/abovemed_3$count_behav
abovemed_3 <- abovemed_3[!duplicated(abovemed_3$female_sname),]

belowmed_1 <- subset(groom, females_in_grp_avg<quantile(groom_final_6$females_in_grp_avg, 0.5))
belowmed_1$set <- "below_median"
belowmed_2 <- belowmed_1 %>% group_by(female_sname) %>% mutate(count_behav=dplyr::n()) 
belowmed_3 <- belowmed_2 %>% group_by(female_sname, count_behav) %>% mutate(yes_behav=sum(groom_two_month))
belowmed_3$prop_behav <- belowmed_3$yes_behav/belowmed_3$count_behav
belowmed_3 <- belowmed_3[!duplicated(belowmed_3$female_sname),]

tmp2 <- rbind(abovemed_3, belowmed_3)

ggplot(data=tmp2) + geom_jitter(aes(x = set, y = prop_behav, color=set, size=5), alpha=0.8) + geom_violin(aes(x = set, y = prop_behav, color=set, fill=set),  alpha = 0.4) + geom_boxplot(aes(x = set, y = prop_behav), size=1.25, color="black", width=0.1, fill="white", outlier.colour = NA) + scale_y_continuous(name="proportion of grooming occurrences\nout of all potential grooming opportunities")  + scale_x_discrete(labels=c("above_median"= "above median", "below_median" = "below median")) + theme_classic() + theme(legend.position = "none", legend.title = element_blank(), axis.title.x = element_blank(),text=element_text(size=30), axis.text = element_text(color="black")) + scale_color_manual(values = c("above_median" = "royalblue3", "below_median" = "royalblue3")) + scale_fill_manual(values = c("above_median" = "royalblue3", "below_median" = "royalblue3"))

