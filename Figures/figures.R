#!/usr/bin/env Rscript

# Scripts for producing Figures 1-3, S1, and S5-6
# The scripts for Figures S2-S4, S5c-d, S6c-d are not included here as they are identical to the scripts for Figures 1-3 and S5-S6 except:
# (1) the dataset (groom) is replaced by the proximity data set (also availble at XXX) and
# (2) the model used for predictions is the proximity logistic regression model

# Load the grooming data set available at XXX
groom <- read.csv("groom_anonymized.csv", header=T) # for Figures S2-4, load the proximity data set (prox_anonymized.csv) instead

# Load the data set for producing Figure S1 available at XXX
# This data set is from the initial, monthly-based data set and contains all unique social group-month combinations
s1 <- read.csv("unique_social_group_months_FigS1.csv", header=T)

# Load R libraries
library(ggplot2)
library(glmmTMB)

# Need to run original grooming logistic regression model for some of the figure making
# Grooming model (see script in Models directory)
groom_model <- glmmTMB(groom_two_month ~ assortative_genetic_ancestry_index + heterozygosity_female + heterozygosity_male + genetic_relatedness + rank_female*rank_male + female_age + female_age_transformed + females_in_group + males_in_group + reproductive_state*genetic_ancestry_female + reproductive_state*genetic_ancestry_male + pair_coresidency + observer_effort + (1 | female_id) + (1 | male_id), data=groom, family="binomial") # for Figures S2-4, run the proximity logistic regression model instead (see script in Models directory)

set.seed(1234) # so figures with any jittering are reproducible - will not completely match the published figures because although this set.seed was used to generate the paper's figures, the figures were produced in a different order below (order below reflects order of figures in the paper)

#############################################################################################################################
# Figure 1. Genetic ancestry and dominance rank predict the tendency to groom with an opposite-sex partner.
#############################################################################################################################

####################
# Fig. 1A
####################
# Calculate the probability of grooming among co-resident opposite-sex pairs per two-month interval, for the most anubis-like males (above the 90th percentile for male genetic ancestry in the data set) and the most yellow-like males (below the 10th percentile for male genetic ancestry in the data set) without adjustment for other covariates
# Use the 90th and 10th percentiles across rows in the data sets
highperc_1 <- subset(groom, genetic_ancestry_male>quantile(groom$genetic_ancestry_male, 0.9)) # get all lines of data for the most anubis-like males (above the 90th percentile for male genetic ancestry)
highperc_1$set <- "above_90"
highperc_2 <- highperc_1 %>% group_by(male_id) %>% mutate(count_behav=dplyr::n()) # count the total number of grooming opportunities (i.e., the total number of co-resident pairings) for the most anubis-like males
highperc_3 <- highperc_2 %>% group_by(male_id, count_behav) %>% mutate(yes_behav=sum(groom_two_month)) # count the total number of grooming occurrences (i.e., 1 for groom_two_month) for the most anubis-like males
highperc_3$prop_behav <- highperc_3$yes_behav/highperc_3$count_behav # divide the total number of grooming occurrences by the total number of grooming opportunities to get the probability of grooming for the most anubis-like males (without adjustment for other covariates)
highperc_3 <- highperc_3[!duplicated(highperc_3$male_id),] # only grab one row per male

lowperc_1 <- subset(groom, genetic_ancestry_male<quantile(groom$genetic_ancestry_male, 0.1)) # get all lines of data for the most yellow-like males (below the 10th percentile for male genetic ancestry)
lowperc_1$set <- "below_10"
lowperc_2 <- lowperc_1 %>% group_by(male_id) %>% mutate(count_behav=dplyr::n()) # count the total number of grooming opportunities (i.e., the total number of co-resident pairings) for the most yellow-like males
lowperc_3 <- lowperc_2 %>% group_by(male_id, count_behav) %>% mutate(yes_behav=sum(groom_two_month)) # count the total number of grooming occurrences (i.e., 1 for groom_two_month) for the most yellow-like males
lowperc_3$prop_behav <- lowperc_3$yes_behav/lowperc_3$count_behav # divide the total number of grooming occurrences by the total number of grooming opportunities to get the probability of grooming for the most yellow-like males (without adjustment for other covariates)
lowperc_3 <- lowperc_3[!duplicated(lowperc_3$male_id),] # only grab one row per male

tmp <- rbind(highperc_3, lowperc_3) # combine most anubis-like and most yellow-like males into a single dataframe

# Plot figure 1A
ggplot(data=tmp) + geom_violin(aes(x = set, y = prop_behav, color=set, fill=set),  alpha = 0.4) +  geom_jitter(aes(x = set, y = prop_behav, color=set, size=5)) + geom_boxplot(aes(x = set, y = prop_behav),  size=1.25, width=0.1, color="black", fill="white", outlier.colour = NA) + scale_y_continuous(name="grooming probability") + scale_x_discrete(labels=c("above_90"= "most anubis-like", "below_10" = "most yellow-like")) + theme_classic() + theme(legend.position = "none", legend.title = element_blank(), axis.title.x = element_blank(),text=element_text(size=28), axis.text = element_text(color="black")) + scale_color_manual(values = c("above_90" = "turquoise4", "below_10" = "turquoise4")) + scale_fill_manual(values = c("above_90" = "turquoise4", "below_10" = "turquoise4"))


####################
# Fig. 1B
####################
# For the dashed line which shows the predicted relationship between male dominance rank and grooming based on model estimates, assuming average values for all other covariates
# Get the grooming probability for male rank given an average female social partner in an average demographic environment
# Note that we exclude male rank and female id and male id are coded as NA
groomm_r <- data.frame(females_in_group=mean(groom$females_in_group), males_in_group=mean(groom$males_in_group), rank_female=mean(groom$rank_female), genetic_ancestry_female=mean(groom$genetic_ancestry_female), heterozygosity_male=mean(groom$heterozygosity_male), heterozygosity_female=mean(groom$heterozygosity_female), genetic_relatedness=mean(groom$genetic_relatedness), observer_effort=mean(groom$observer_effort), female_age=mean(groom$female_age), female_age_transformed=mean(groom$female_age_transformed), reproductive_state=1, pair_coresidency=mean(groom$pair_coresidency), genetic_ancestry_male=mean(groom$genetic_ancestry_male), assortative_genetic_ancestry_index=mean(groom$assortative_genetic_ancestry_index), female_id=NA, male_id=NA)
# Get the range of empirical values for male rank
m_rank <- as.data.frame(seq(min(groom$rank_male),max(groom$rank_male), by=1))
colnames(m_rank) <- c("rank_male")

tmp <- merge(groomm_r,m_rank) # combine dataframe of averaged variables with the dataframe of the empirical range of male rank

# Get the grooming probability for the range of male rank values assuming average values for all other covariates
tmp$probs <- predict(groom_model, tmp, type="response", re.form=NA)

# For the colored dots which show probabilities based on counts of grooming occurrences (same calculation as in figure 1A but per rank)
raw <- groom %>% group_by(rank_male) %>% mutate(count_groom=dplyr::n()) # count the total number of grooming opportunities (i.e., the total number of co-resident pairings) for each male rank
raw2 <- raw %>% group_by(rank_male, count_groom) %>% mutate(yes_groom=sum(groom_two_month)) # count the total number of grooming occurrences (i.e., 1 for groom_two_month) for each male rank
raw3 <- distinct(raw2, rank_male, count_groom, yes_groom)  # only grab one row per rank
raw3$groom_prox <- raw3$yes_groom/raw3$count_groom # divide the total number of grooming occurrences by the total number of grooming opportunities to get the probability of grooming for each male rank (without adjustment for other covariates)

# Plot figure 1B
ggplot() + geom_line(data=tmp, aes(rank_male, probs), color="black", size=1, linetype="dashed") + geom_jitter(data=groom, aes(rank_male, groom_two_month), alpha=0.05, height=0.05, color="grey70") + scale_x_continuous(name="male rank",breaks=c(1,5,10,15,20,25,30)) +  theme_classic() + theme(text=element_text(size=20), axis.text = element_text(color="black")) + geom_point(data=raw3, aes(rank_male, groom_prox), size=3, color="turquoise4") + scale_y_continuous(name="grooming probability") 


####################
# Fig. 1C
####################
# Calculate the probability of grooming among co-resident opposite-sex pairs, per two-month interval, for the most anubis-like females (above the 90th percentile for female genetic ancestry in the data set) and the most yellow-like females (below the 10th percentile for female genetic ancestry in the data set) without adjustment for other covariates
# Use the 90th and 10th percentiles across rows in the data sets
highperc_1 <- subset(groom, genetic_ancestry_female>quantile(groom$genetic_ancestry_female, 0.9)) # get all lines of data for the most anubis-like females (above the 90th percentile for female genetic ancestry)
highperc_1$set <- "above_90"
highperc_2 <- highperc_1 %>% group_by(female_id) %>% mutate(count_behav=dplyr::n()) # count the total number of grooming opportunities (i.e., the total number of co-resident pairings) for the most anubis-like females
highperc_3 <- highperc_2 %>% group_by(female_id, count_behav) %>% mutate(yes_behav=sum(groom_two_month)) # count the total number of grooming occurrences (i.e., 1 for groom_two_month) for the most anubis-like females
highperc_3$prop_behav <- highperc_3$yes_behav/highperc_3$count_behav # divide the total number of grooming occurrences by the total number of grooming opportunities to get the probability of grooming for the most anubis-like females (without adjustment for other covariates)
highperc_3 <- highperc_3[!duplicated(highperc_3$female_id),] # only grab one row per female

lowperc_1 <- subset(groom, genetic_ancestry_female<quantile(groom$genetic_ancestry_female, 0.1)) # get all lines of data for the most yellow-like females (above the 90th percentile for female genetic ancestry)
lowperc_1$set <- "below_10"
lowperc_2 <- lowperc_1 %>% group_by(female_id) %>% mutate(count_behav=dplyr::n()) # count the total number of grooming opportunities (i.e., the total number of co-resident pairings) for the most yellow-like females
lowperc_3 <- lowperc_2 %>% group_by(female_id, count_behav) %>% mutate(yes_behav=sum(groom_two_month)) # count the total number of grooming occurrences  (i.e., 1 for groom_two_month)for the most yellow-like females
lowperc_3$prop_behav <- lowperc_3$yes_behav/lowperc_3$count_behav # divide the total number of grooming occurrences by the total number of grooming opportunities to get the probability of grooming for the most yellow-like females (without adjustment for other covariates)
lowperc_3 <- lowperc_3[!duplicated(lowperc_3$female_id),] # only grab one row per female

tmp <- rbind(highperc_3, lowperc_3)

# Plot figure 1C
ggplot(data=tmp) + geom_violin(aes(x = set, y = prop_behav, color=set, fill=set),  alpha = 0.4) + geom_jitter(aes(x = set, y = prop_behav, color=set, size=5)) + geom_boxplot(aes(x = set, y = prop_behav), size=1.25, color="black", width=0.1, fill="white", outlier.colour = NA) + scale_y_continuous(name="grooming probability") + scale_x_discrete(labels=c("above_90"= "most anubis-like", "below_10" = "most yellow-like")) + theme_classic() + theme(legend.position = "none", legend.title = element_blank(), axis.title.x = element_blank(),text=element_text(size=28), axis.text = element_text(color="black")) + scale_color_manual(values = c("above_90" = "royalblue3", "below_10" = "royalblue3")) + scale_fill_manual(values = c("above_90" = "royalblue3", "below_10" = "royalblue3"))


####################
# Fig. 1D
####################
# For the dashed line which shows the predicted relationship between female dominance rank and grooming based on model estimates, assuming average values for all other covariates
# Get the grooming probability for female rank given an average male social partner in an average demographic environment
# Note that we exclude female rank and female id and male id are coded as NA
groomf_r <- data.frame(females_in_group=mean(groom$females_in_group), males_in_group=mean(groom$males_in_group), rank_male=mean(groom$rank_male), genetic_ancestry_female=mean(groom$genetic_ancestry_female), heterozygosity_male=mean(groom$heterozygosity_male), heterozygosity_female=mean(groom$heterozygosity_female), genetic_relatedness=mean(groom$genetic_relatedness), observer_effort=mean(groom$observer_effort), female_age=mean(groom$female_age), female_age_transformed=mean(groom$female_age_transformed), reproductive_state=1, pair_coresidency=mean(groom$pair_coresidency), genetic_ancestry_male=mean(groom$genetic_ancestry_male), assortative_genetic_ancestry_index=mean(groom$assortative_genetic_ancestry_index), female_id=NA, male_id=NA)
# Get the range of empirical values for female rank
f_rank <- as.data.frame(seq(min(groom$rank_female),max(groom$rank_female), by=1))
colnames(f_rank) <- c("rank_female")

tmp <- merge(groomf_r,f_rank) # combine dataframe of averaged variables with the dataframe of the empirical range of female rank

# Get the grooming probability for the range of female rank values assuming average values for all other covariates
tmp$probs <- predict(groom_model, tmp, type="response", re.form=NA)

# For the colored dots which show probabilities based on counts of grooming occurrences (same calculation as in figure 1C but per rank)
raw <- groom %>% group_by(rank_female) %>% mutate(count_groom=dplyr::n()) # count the total number of grooming opportunities (i.e., the total number of co-resident pairings) for each female rank
raw2 <- raw %>% group_by(rank_female, count_groom) %>% mutate(yes_groom=sum(groom_two_month)) # count the total number of grooming occurrences (i.e., 1 for groom_two_month) for each female rank
raw3 <- distinct(raw2, rank_female, count_groom, yes_groom) # only grab one row per rank
raw3$groom_prox <- raw3$yes_groom/raw3$count_groom # divide the total number of grooming occurrences by the total number of grooming opportunities to get the probability of grooming for each female rank (without adjustment for other covariates)

# Plot figure 1D
ggplot() + geom_jitter(data=groom, aes(rank_female, groom_two_month), alpha=0.05, height=0.05, color="grey70") + scale_x_continuous(name="female rank",breaks=c(1,5,10,15,20,25,30)) +  theme_classic() + theme(text=element_text(size=20), axis.text = element_text(color="black")) + geom_point(data=raw3, aes(rank_female, groom_prox), size=3, colour="royalblue3") + scale_y_continuous(name="grooming probability") + geom_line(data=tmp, aes(rank_female, probs), color="black", size=1, linetype="dashed") 


#############################################################################################################################
# Figure 2. Combined genetic ancestry characteristics of females and males affect the probability of grooming. 
#############################################################################################################################

####################
# Fig. 2 central heatmap
####################
# Get the probability of grooming behavior as a function of female genetic ancestry and male genetic ancestry, based on model estimates assuming average values for all other covariates
# Note that we exclude female genetic ancestry, male genetic ancestry, and the assortative genetic ancestry index (and female id and male id are coded as NA below)
tmp <- data.frame(females_in_group=mean(groom$females_in_group), males_in_group=mean(groom$males_in_group), rank_female=mean(groom$rank_female), rank_male=mean(groom$rank_male),  heterozygosity_male=mean(groom$heterozygosity_male), heterozygosity_female=mean(groom$heterozygosity_female), genetic_relatedness=mean(groom$genetic_relatedness), observer_effort=mean(groom$observer_effort), female_age=mean(groom$female_age), female_age_transformed=mean(groom$female_age_transformed), reproductive_state=1, pair_coresidency=mean(groom$pair_coresidency))

# Get all possible female genetic ancestry and male genetic ancestry values (these values can range continuously from 0 to 1 where where 0 corresponds to unadmixed yellow baboon ancestry and 1 corresponds to unadmixed anubis baboon ancestry)
f_genetic <- as.vector(seq(0,1, by=0.001))
m_genetic <- as.vector(seq(0,1, by=0.001))

# Get all pairwise combinations of female genetic ancestry and male genetic ancestry
tmp2 <- expand.grid(f_genetic, m_genetic)
colnames(tmp2) <- c("genetic_ancestry_male", "genetic_ancestry_female")

# Recalculate the assortative genetic ancestry index for the above pairwise combinations of female genetic ancestry and male genetic ancestry
tmp2$assortative_1 <- (tmp2$genetic_ancestry_male*tmp2$genetic_ancestry_female)
tmp2$assortative_2 <- ((1-tmp2$genetic_ancestry_male)*(1-tmp2$genetic_ancestry_female))
tmp2$assortative_genetic_ancestry_index <- apply(tmp2[, 3:4], 1, max)

tmp3 <- merge(tmp2, tmp)  # combine dataframe of averaged variables with the dataframe of all pairwise combinations of female genetic ancestry and male genetic ancestry

# code female id and male id as NA
tmp3$female_id <- NA
tmp3$male_id <- NA

# Get the grooming probability for female genetic ancestry and male genetic ancestry based on model estimates assuming average values for all other covariates
tmp3$probs <- predict(groom_model, tmp3, type="response", re.form=NA) 

# Plot figure 2 central heatmap
ggplot(tmp3, aes(x=genetic_ancestry_female, y=genetic_ancestry_male, fill=probs)) + geom_raster() + theme_classic() + scale_fill_gradientn(colours=c("blue","cyan","green", "yellow","orange", "red", "indianred4")) + scale_x_continuous(name="female genetic ancestry") + scale_y_continuous(name="male genetic ancestry") + theme(legend.position = "right", text=element_text(size=16), axis.text = element_text(color="black")) + guides(fill=guide_colorbar(ticks.colour = "black", ticks.linewidth = 1.5))

####################
# Fig. 2 line graps surrounding heatmap
####################
# For these plots, it is helpful for comparisons if the y-axes - which plot the grooming probabilities - have the same ranges
min(tmp3$probs) # minimum grooming probability = 0.111543
max(tmp3$probs) # maximum grooming probability = 0.303592
# Set y-axes so the minimum y-value is 0.11 and the maximum y-value is 0.31

# Plot the probability of grooming behavior for yellow males (genetic ancestry = 0) as a function of the genetic ancestry of potential opposite-sex social partners (left top panel)
tmp4 <- subset(tmp3, genetic_ancestry_male==0)
ggplot(tmp4) + geom_line(aes(genetic_ancestry_female, probs), size=2) + theme_classic()+ theme(legend.position = "none", text=element_text(size=24), axis.text = element_text(color="black")) + scale_x_continuous(name="female genetic ancestry") + scale_y_continuous(limits=c(0.11, 0.31), name="grooming probability")
# Plot the probability of grooming behavior for anubis males (genetic ancestry = 1) as a function of the genetic ancestry of potential opposite-sex social partners (left bottom panel)
tmp4 <- subset(tmp3, genetic_ancestry_male==1)
ggplot(tmp4) + geom_line(aes(genetic_ancestry_female, probs), size=2) + theme_classic()+ theme(legend.position = "none", text=element_text(size=24), axis.text = element_text(color="black")) + scale_x_continuous(name="female genetic ancestry") + scale_y_continuous(limits=c(0.11, 0.31), name="grooming probability")

# Plot the probability of grooming behavior for yellow females (genetic ancestry = 0) as a function of the genetic ancestry of potential opposite-sex social partners (bottom left panel)
tmp4 <- subset(tmp3, genetic_ancestry_female==0)
ggplot(tmp4) + geom_line(aes(genetic_ancestry_male, probs), size=2) + theme_classic()+ theme(legend.position = "none", text=element_text(size=24), axis.text = element_text(color="black")) + scale_x_continuous(name="male genetic ancestry") + scale_y_continuous(limits=c(0.11, 0.31), name="grooming probability")
# Plot the probability of grooming behavior for anubis females (genetic ancestry = 1) as a function of the genetic ancestry of potential opposite-sex social partners (bottom right panel)
tmp4 <- subset(tmp3, genetic_ancestry_female==1)
ggplot(tmp4) + geom_line(aes(genetic_ancestry_male, probs), size=2) + theme_classic()+ theme(legend.position = "none", text=element_text(size=24), axis.text = element_text(color="black")) + scale_x_continuous(name="male genetic ancestry") + scale_y_continuous(limits=c(0.11, 0.31), name="grooming probability")


#############################################################################################################################
# Figure 3. Combined rank characteristics of females and males affect the probability of grooming.
#############################################################################################################################

####################
# Fig. 3 central heatmap
####################
# Get the probability of grooming behavior as a function of female dominance rank and male dominance rank, based on model estimates assuming average values for all other covariates
# Note that we exclude female rank and male rank (and female id and male id are coded as NA below)
tmp <- data.frame(females_in_group=mean(groom$females_in_group), males_in_group=mean(groom$males_in_group), genetic_ancestry_female=mean(groom$genetic_ancestry_female), genetic_ancestry_male=mean(groom$genetic_ancestry_male),  heterozygosity_male=mean(groom$heterozygosity_male), heterozygosity_female=mean(groom$heterozygosity_female), genetic_relatedness=mean(groom$genetic_relatedness), observer_effort=mean(groom$observer_effort), female_age=mean(groom$female_age), female_age_transformed=mean(groom$female_age_transformed), reproductive_state=1, assortative_genetic_ancestry_index=mean(groom$assortative_genetic_ancestry_index), pair_coresidency=mean(groom$pair_coresidency))

# Get all the empirical range of female dominance rank and male dominance rank values
f_rank <- as.vector(seq(min(groom$rank_female),max(groom$rank_female), by=1))
m_rank <- as.vector(seq(min(groom$rank_male),max(groom$rank_male), by=1))

# Get all pairwise combinations of female dominance rank and male dominance rank
tmp2 <- expand.grid(f_rank, m_rank)
colnames(tmp2) <- c("rank_female", "rank_male")

tmp3 <- merge(tmp2, tmp) # combine dataframe of averaged variables with the dataframe of all pairwise combinations of female dominance rank and male dominance rank

# code female id and male id as NA
tmp3$female_id <- NA
tmp3$male_id <- NA

# Get the grooming probability for female dominance rank and male dominance rank based on model estimates assuming average values for all other covariates
tmp3$probs <- predict(groom_model, tmp3, type="response", re.form=NA)

# Plot figure 3 central heatmap
ggplot(tmp3, aes(x=rank_female, y=rank_male, fill=probs)) + geom_raster() + theme_classic() + scale_fill_gradientn(colours=c("blue","cyan","green", "yellow","orange", "red", "indianred4")) + scale_x_continuous(name="female rank",  breaks=c(1,5,10,15,20,25,30)) + scale_y_continuous(name="male rank", breaks=c(1,5,10,15,20,25,30)) + theme(legend.position = "right", text=element_text(size=16), axis.text = element_text(color="black")) + guides(fill=guide_colorbar(ticks.colour = "black", ticks.linewidth = 1.5))


####################
# Fig. 3 line graps surrounding heatmap
####################
# For these plots, it is helpful for comparisons if the y-axes - which plot the grooming probabilities - have the same ranges
min(tmp3$probs) # minimum grooming probability = 0.04695171
max(tmp3$probs) # maximum grooming probability = 0.2242176
# Set y-axes so the minimum y-value is 0.04 and the maximum y-value is 0.23

# Note: we represented rank using an ordinal approach, where the highest-ranking individual holds rank 1 and lower-ranking individuals occupy ranks of successively higher numbers
# Plot the probability of grooming behavior for the lowest ranking males as a function of the dominance rank of potential opposite-sex social partners (left top panel)
tmp4 <- subset(tmp3, max(rank_male)==rank_male) 
ggplot(tmp4) + geom_line(aes(rank_female, probs), size=2) + theme_classic()+ theme(legend.position = "none", text=element_text(size=24), axis.text = element_text(color="black")) + scale_x_continuous(name="female rank", breaks=c(1,5,10,15,20,25,30)) + scale_y_continuous(limits=c(0.04, 0.23), name="grooming probability")
# Plot the probability of grooming behavior for the highest ranking males as a function of the dominance rank of potential opposite-sex social partners (left bottom panel)
tmp4 <- subset(tmp3, min(rank_male)==rank_male) 
ggplot(tmp4) + geom_line(aes(rank_female, probs), size=2) + theme_classic()+ theme(legend.position = "none", text=element_text(size=24), axis.text = element_text(color="black")) + scale_x_continuous(name="female rank", breaks=c(1,5,10,15,20,25,30)) + scale_y_continuous(limits=c(0.04, 0.23), name="grooming probability")

# Plot the probability of grooming behavior for the lowest ranking females as a function of the dominance rank of potential opposite-sex social partners (bottom left panel)
tmp4 <- subset(tmp3, max(rank_female)==rank_female)
ggplot(tmp4) + geom_line(aes(rank_male, probs), size=2) + theme_classic()+ theme(legend.position = "none", text=element_text(size=24), axis.text = element_text(color="black")) + scale_x_continuous(name="male rank", breaks=c(1,5,10,15,20)) + scale_y_continuous(limits=c(0.04, 0.23), name="grooming probability")
# Plot the probability of grooming behavior for the highest ranking females as a function of the dominance rank of potential opposite-sex social partners (bottom right panel)
tmp4 <- subset(tmp3, min(rank_female)==rank_female)
ggplot(tmp4) + geom_line(aes(rank_male, probs), size=2) + theme_classic()+ theme(legend.position = "none", text=element_text(size=24), axis.text = element_text(color="black")) + scale_x_continuous(name="male rank", breaks=c(1,5,10,15,20)) + scale_y_continuous(limits=c(0.04, 0.23), name="grooming probability")


#############################################################################################################################
# Figure S1. The total effort invested in behavioral observations is consistent across study groups of different sizes.
#############################################################################################################################
# Plot figure S1
ggplot(s1) + geom_point(alpha=0.5, size=2.5, aes(x=females_in_group, y=total_adult_female_point_samples_in_month, colour=as.factor(group_id))) + scale_x_continuous(name="adult females in social group") + scale_y_continuous(name="total adult female point samples\n per month per social group") + theme_classic() +  theme(axis.title=element_text(size=16), axis.text=element_text(size=16), legend.position = "none", text = element_text(color="black"))


#############################################################################################################################
# Figure S5. The number of adult males present in a social group influences grooming and proximity behavior.
#############################################################################################################################

####################
# Fig. S5A
####################
# Calculate the probability of grooming among co-resident opposite-sex pairs per two-month interval, for each male in social groups with greater or less than the median number of co-resident males in the sample without adjustment for other covariates
abovemed_1 <- subset(groom, males_in_group>quantile(groom$males_in_group, 0.5)) # get all lines of data with greater than the median number of co-resident males in the sample
abovemed_1$set <- "above_median"
abovemed_2 <- abovemed_1 %>% group_by(male_id) %>% mutate(count_behav=dplyr::n()) # count the total number of grooming opportunities (i.e., the total number of co-resident pairings) for males living in groups with with greater than the median number of co-resident males in the sample
abovemed_3 <- abovemed_2 %>% group_by(male_id, count_behav) %>% mutate(yes_behav=sum(groom_two_month)) # count the total number of grooming occurrences (i.e., 1 for groom_two_month) for males living in groups with with greater than the median number of co-resident males in the sample
abovemed_3$prop_behav <- abovemed_3$yes_behav/abovemed_3$count_behav # divide the total number of grooming occurrences by the total number of grooming opportunities to get the probability of grooming for males living in groups with greater than the median number of co-resident males in the sample (without adjustment for other covariates)
abovemed_3 <- abovemed_3[!duplicated(abovemed_3$male_id),] # only grab one row per male

belowmed_1 <- subset(groom, males_in_group<quantile(groom$males_in_group, 0.5)) # get all lines of data with less than the median number of co-resident males in the sample
belowmed_1$set <- "below_median"
belowmed_2 <- belowmed_1 %>% group_by(male_id) %>% mutate(count_behav=dplyr::n()) # count the total number of grooming opportunities (i.e., the total number of co-resident pairings) for males living in groups with with less than the median number of co-resident males in the sample
belowmed_3 <- belowmed_2 %>% group_by(male_id, count_behav) %>% mutate(yes_behav=sum(groom_two_month)) # count the total number of grooming occurrences (i.e., 1 for groom_two_month) for males living in groups with with less than the median number of co-resident males in the sample
belowmed_3$prop_behav <- belowmed_3$yes_behav/belowmed_3$count_behav # divide the total number of grooming occurrences by the total number of grooming opportunities to get the probability of grooming for males living in groups with less than the median number of co-resident males in the sample (without adjustment for other covariates)
belowmed_3 <- belowmed_3[!duplicated(belowmed_3$male_id),] # only grab one row per male

tmp <- rbind(abovemed_3, belowmed_3)  # combine males living in groups with greater than and less than the median number of co-resident males in the sample into a single dataframe

# Plot figure S5A
ggplot(data=tmp) + geom_jitter(aes(x = set, y = prop_behav, color=set, size=5), alpha=0.8) + geom_violin(aes(x = set, y = prop_behav, color=set, fill=set),  alpha = 0.4) + geom_boxplot(aes(x = set, y = prop_behav), size=1.25, color="black", width=0.1, fill="white", outlier.colour = NA) + scale_y_continuous(name="proportion of grooming occurrences\nout of all potential grooming opportunities")  + scale_x_discrete(labels=c("above_median"= "above median", "below_median" = "below median")) + theme_classic() + theme(legend.position = "none", legend.title = element_blank(), axis.title.x = element_blank(),text=element_text(size=30), axis.text = element_text(color="black")) + scale_color_manual(values = c("above_median" = "turquoise4", "below_median" = "turquoise4")) + scale_fill_manual(values = c("above_median" = "turquoise4", "below_median" = "turquoise4"))


####################
# Fig. S5B
####################
# Calculate the probability of grooming among co-resident opposite-sex pairs per two-month interval, for each female in social groups with greater or less than the median number of co-resident males in the sample without adjustment for other covariates
#abovemed_1 <- subset(groom, males_in_group>quantile(groom$males_in_group, 0.5)) # get all lines of data with greater than the median number of co-resident males in the sample - already done for Figure S5A so do not need to do here
#abovemed_1$set <- "above_median" # already done for Figure S5A so do not need to do here
abovemed_2 <- abovemed_1 %>% group_by(female_id) %>% mutate(count_behav=dplyr::n()) # count the total number of grooming opportunities (i.e., the total number of co-resident pairings) for females living in groups with with greater than the median number of co-resident males in the sample
abovemed_3 <- abovemed_2 %>% group_by(female_id, count_behav) %>% mutate(yes_behav=sum(groom_two_month)) # count the total number of grooming occurrences (i.e., 1 for groom_two_month) for females living in groups with with greater than the median number of co-resident males in the sample
abovemed_3$prop_behav <- abovemed_3$yes_behav/abovemed_3$count_behav # divide the total number of grooming occurrences by the total number of grooming opportunities to get the probability of grooming for females living in groups with greater than the median number of co-resident males in the sample (without adjustment for other covariates)
abovemed_3 <- abovemed_3[!duplicated(abovemed_3$female_id),] # only grab one row per female

#belowmed_1 <- subset(groom, males_in_group<quantile(groom$males_in_group, 0.5)) # get all lines of data with greater than the median number of co-resident males in the sample - already done for Figure S5A so do not need to do here
#belowmed_1$set <- "below_median"  # already done for Figure S5A so do not need to do here
belowmed_2 <- belowmed_1 %>% group_by(female_id) %>% mutate(count_behav=dplyr::n()) # count the total number of grooming opportunities (i.e., the total number of co-resident pairings) for females living in groups with with less than the median number of co-resident males in the sample
belowmed_3 <- belowmed_2 %>% group_by(female_id, count_behav) %>% mutate(yes_behav=sum(groom_two_month)) # count the total number of grooming occurrences (i.e., 1 for groom_two_month) for females living in groups with with less than the median number of co-resident males in the sample
belowmed_3$prop_behav <- belowmed_3$yes_behav/belowmed_3$count_behav # divide the total number of grooming occurrences by the total number of grooming opportunities to get the probability of grooming for females living in groups with less than the median number of co-resident males in the sample (without adjustment for other covariates)
belowmed_3 <- belowmed_3[!duplicated(belowmed_3$female_id),] # only grab one row per female

tmp <- rbind(abovemed_3, belowmed_3) # combine females living in groups with greater than and less than the median number of co-resident males in the sample into a single dataframe

# Plot figure S5B
ggplot(data=tmp) + geom_jitter(aes(x = set, y = prop_behav, color=set, size=5), alpha=0.8) + geom_violin(aes(x = set, y = prop_behav, color=set, fill=set),  alpha = 0.4) + geom_boxplot(aes(x = set, y = prop_behav), size=1.25, color="black", width=0.1, fill="white", outlier.colour = NA) + scale_y_continuous(name="proportion of grooming occurrences\nout of all potential grooming opportunities")  + scale_x_discrete(labels=c("above_median"= "above median", "below_median" = "below median")) + theme_classic() + theme(legend.position = "none", legend.title = element_blank(), axis.title.x = element_blank(),text=element_text(size=30), axis.text = element_text(color="black")) + scale_color_manual(values = c("above_median" = "royalblue3", "below_median" = "royalblue3")) + scale_fill_manual(values = c("above_median" = "royalblue3", "below_median" = "royalblue3"))


#############################################################################################################################
# Figure S6. The number of adult females present in a social group influences grooming but not proximity behavior.
#############################################################################################################################

####################
# Fig. S6A
####################
# Calculate the probability of grooming among co-resident opposite-sex pairs per two-month interval, for each male in social groups with greater or less than the median number of co-resident females in the sample without adjustment for other covariates
abovemed_1 <- subset(groom, females_in_group>quantile(groom$females_in_group, 0.5))
abovemed_1$set <- "above_median"
abovemed_2 <- abovemed_1 %>% group_by(male_id) %>% mutate(count_behav=dplyr::n()) 
abovemed_3 <- abovemed_2 %>% group_by(male_id, count_behav) %>% mutate(yes_behav=sum(groom_two_month))
abovemed_3$prop_behav <- abovemed_3$yes_behav/abovemed_3$count_behav
abovemed_3 <- abovemed_3[!duplicated(abovemed_3$male_id),]

belowmed_1 <- subset(groom, females_in_group<quantile(groom$females_in_group, 0.5))
belowmed_1$set <- "below_median"
belowmed_2 <- belowmed_1 %>% group_by(male_id) %>% mutate(count_behav=dplyr::n()) 
belowmed_3 <- belowmed_2 %>% group_by(male_id, count_behav) %>% mutate(yes_behav=sum(groom_two_month))
belowmed_3$prop_behav <- belowmed_3$yes_behav/belowmed_3$count_behav
belowmed_3 <- belowmed_3[!duplicated(belowmed_3$male_id),]

tmp <- rbind(abovemed_3, belowmed_3)

# Plot figure S6A
ggplot(data=tmp) + geom_jitter(aes(x = set, y = prop_behav, color=set, size=5), alpha=0.8) + geom_violin(aes(x = set, y = prop_behav, color=set, fill=set),  alpha = 0.4) + geom_boxplot(aes(x = set, y = prop_behav), size=1.25, color="black", width=0.1, fill="white", outlier.colour = NA) + scale_y_continuous(name="proportion of grooming occurrences\nout of all potential grooming opportunities")  + scale_x_discrete(labels=c("above_median"= "above median", "below_median" = "below median")) + theme_classic() + theme(legend.position = "none", legend.title = element_blank(), axis.title.x = element_blank(),text=element_text(size=30), axis.text = element_text(color="black")) + scale_color_manual(values = c("above_median" = "turquoise4", "below_median" = "turquoise4")) + scale_fill_manual(values = c("above_median" = "turquoise4", "below_median" = "turquoise4"))

####################
# Fig. S6B
####################
# Calculate the probability of grooming among co-resident opposite-sex pairs per two-month interval, for each female in social groups with greater or less than the median number of co-resident females in the sample without adjustment for other covariates
abovemed_1 <- subset(groom, females_in_group>quantile(groom$females_in_group, 0.5))
abovemed_1$set <- "above_median"
abovemed_2 <- abovemed_1 %>% group_by(female_id) %>% mutate(count_behav=dplyr::n()) 
abovemed_3 <- abovemed_2 %>% group_by(female_id, count_behav) %>% mutate(yes_behav=sum(groom_two_month))
abovemed_3$prop_behav <- abovemed_3$yes_behav/abovemed_3$count_behav
abovemed_3 <- abovemed_3[!duplicated(abovemed_3$female_id),]

belowmed_1 <- subset(groom, females_in_group<quantile(groom$females_in_group, 0.5))
belowmed_1$set <- "below_median"
belowmed_2 <- belowmed_1 %>% group_by(female_id) %>% mutate(count_behav=dplyr::n()) 
belowmed_3 <- belowmed_2 %>% group_by(female_id, count_behav) %>% mutate(yes_behav=sum(groom_two_month))
belowmed_3$prop_behav <- belowmed_3$yes_behav/belowmed_3$count_behav
belowmed_3 <- belowmed_3[!duplicated(belowmed_3$female_id),]

tmp <- rbind(abovemed_3, belowmed_3)

# Plot figure S6B
ggplot(data=tmp) + geom_jitter(aes(x = set, y = prop_behav, color=set, size=5), alpha=0.8) + geom_violin(aes(x = set, y = prop_behav, color=set, fill=set),  alpha = 0.4) + geom_boxplot(aes(x = set, y = prop_behav), size=1.25, color="black", width=0.1, fill="white", outlier.colour = NA) + scale_y_continuous(name="proportion of grooming occurrences\nout of all potential grooming opportunities")  + scale_x_discrete(labels=c("above_median"= "above median", "below_median" = "below median")) + theme_classic() + theme(legend.position = "none", legend.title = element_blank(), axis.title.x = element_blank(),text=element_text(size=30), axis.text = element_text(color="black")) + scale_color_manual(values = c("above_median" = "royalblue3", "below_median" = "royalblue3")) + scale_fill_manual(values = c("above_median" = "royalblue3", "below_median" = "royalblue3"))

