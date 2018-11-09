## LOAD PACKAGES ####
library(dplyr)
library(ggthemes)
library(broom)
library(MASS)

## READ IN DATA ####
source("~/Desktop/Impacts Systematic Review/scripts/impacts_systematic_review/clean_raw_data.R") # This tells R to run our entire cleaning script so that we have

## CLEAN DATA ####
head(temporal_raw)
# General chi-square for just strayer figure.  
counts_of_strayer <- count(temporal_raw$studylengthbinned)
counts_of_strayer
full_chi <- chisq.test(counts_of_strayer$freq)
full_chi
full_chi$expected

# # loglinear modeling Using the GLM function
temporal_and_impact <- dplyr::select(temporal_raw, impacttype, studylengthbinned)
temporal_and_impact_count <- plyr::count(temporal_and_impact)
head(temporal_and_impact_count)
# 
# # This is from tutorial using loglm()
# temporal_table = xtabs(freq ~ studylengthbinned + impacttype,
#               data=temporal_and_impact_count)
# temporal_table
# 
# # Here's the test of mutual independence of the two varaibles of concern
# loglm_for_temp_table <- loglm( ~ studylengthbinned*impacttype,
#        temporal_table)
# loglm_for_temp_table
# 
# # This equation is for a saturated loglinear model
# temporal_and_impact_count
# glm1 <- glm(freq ~ studylengthbinned*impacttype, data = temporal_and_impact_count, family = poisson())
# summary(glm1)
# tidy_glm_impact_time <- tidy(glm1)
# tidy_glm_impact_time
# write.csv(tidy_glm_impact_time, "/Users/rpecchia/Desktop/Impacts Systematic Review/output/coefficients_for_impact_and_time.csv")
# 
# glm1$xlevels
# # This lets us see if impact type is significant, if study length is signficant and 
# # if the interaction of impact type and study length are significant
# # is this model a good fit?
# # First, look at residual deviance in the summary above (really low! that's good)
# # then, the p-value in following should be ABOVE .05
# 
# # Should we get rid of interaction
# drop1(glm1, test = "Chisq")
# # second line of results "impacttpye:studylnegthbinned is the model without the interaction
# # Since p-value is below .05, we should drop the interaction
# 
# # so let's drop that interaction and just look at main effects.
# # all main effects are signficant
# # This model assumes that study length and impact type are independent of each other
# # This is called the independence model
# # seems great! all 
# glm2 <- glm(freq ~ studylengthbinned + impacttype, data = temporal_and_impact_count, family = poisson())
# summary(glm2) # but the residual deviance is high, so lets look at the residuals.
# # high residuals might mean poor model fit
# # Rule of thumb is that residual deviance should be close to degrees of freedom
# 
# # we can used chisq test to see if expected freuencies satisfy the simpler model
# # a low p-value here indicates that they do not!
# pchisq(deviance(glm2), df = df.residual(glm2), lower.tail = F)
# # Taken together, we should feel confident going with the more complex model
# 
# # glm1 is fully saturated and really complex. glm2 is really simple, maybe overly simple
# # check an anova to see if one model is better than the other
# # if p is above .05 then both models fit equally...go with the simple
# anova(glm1, glm2)
# # then take the deviance and put it into chi square
# 
########################################################
############### Chi-Squared w/ proportions
##########################################

t_temporal_and_impact <- tbl_df(temporal_and_impact_count)
t_temporal_and_impact
unique(t_temporal_and_impact$impacttype)

# Proportions of each taxa using UNIQUE SPECIES
head(temporal_raw)
subset_impact_and_time <- dplyr::select(temporal_raw, impacttype, studylengthbinned)
counted_studylength <- as.data.frame(dplyr::count(subset_impact_and_time, studylengthbinned))
head(counted_studylength)

setDT(counted_studylength)[, Prop := n/sum(n)]
expected_studylength <- counted_studylength$Prop
expected_studylength
# Proportions of taxa WITHIN fitness
# First, make sure all combinations of impact and taxa are complete, so we can get real proportions
t_temporal_and_impact
complete_t_impact_and_length <- complete(t_temporal_and_impact, studylengthbinned, impacttype, fill = list(freq=0))
unique(complete_t_impact_and_length$studylengthbinned)
# Get observe freq for all categories
subset_of_abundance_t <- filter(complete_t_impact_and_length, impacttype == "abundance")
observed_freq_abundance_t <- subset_of_abundance_t$freq

subset_of_behavior_t <- filter(complete_t_impact_and_length, impacttype == "behavior")
observed_freq_behavior_t <- subset_of_behavior_t$freq

subset_of_diversity_t <- filter(complete_t_impact_and_length, impacttype == "diversity")
observed_freq_diversity_t <- subset_of_diversity_t$freq

subset_of_fitness_t <- filter(complete_t_impact_and_length, impacttype == "fitness")
observed_freq_fitness_t <- subset_of_fitness_t$freq

subset_of_growth_t <- filter(complete_t_impact_and_length, impacttype == "growth")
observed_freq_growth_t <- subset_of_growth_t$freq

subset_of_habitatchange_t <- filter(complete_t_impact_and_length, impacttype == "habitat change")
observed_freq_habitatchange_t <- subset_of_habitatchange_t$freq

subset_of_hybrid_t <- filter(complete_t_impact_and_length, impacttype == "hybridization")
observed_freq_hybrid_t <- subset_of_hybrid_t$freq

subset_of_indirect_t <- filter(complete_t_impact_and_length, impacttype == "indirect")
observed_freq_indirect_t <- subset_of_indirect_t$freq

subset_of_nutrient_t <- filter(complete_t_impact_and_length, impacttype == "nutrient availability")
observed_freq_nutrient_t <- subset_of_nutrient_t$freq

subset_of_other_t <- filter(complete_t_impact_and_length, impacttype == "other")
observed_freq_other_t <- subset_of_other_t$freq

subset_of_production_t <- filter(complete_t_impact_and_length, impacttype == "production")
observed_freq_production_t <- subset_of_production_t$freq


# chi-squared for all impact types
abundance_chi <- chisq.test(x = observed_freq_abundance_t, p = expected_studylength)
abundance_chi
abundance_chi$observed
abundance_chi$expected
t_temporal_and_impact$studylengthbinned
behavior_chi <- chisq.test(x = observed_freq_behavior_t, p = expected_studylength)
behavior_chi
sum(behavior_chi$observed)
behavior_chi$expected
diversity_chi<-chisq.test(x = observed_freq_diversity_t, p = expected_studylength)
diversity_chi
diversity_chi$observed
diversity_chi$expected

chisq.test(x = observed_freq_fitness_t, p = expected_studylength)
chisq.test(x = observed_freq_growth_t, p = expected_studylength)
chisq.test(x = observed_freq_habitatchange_t, p = expected_studylength)
hybrid_chi <- chisq.test(x = observed_freq_hybrid_t, p = expected_studylength)
hybrid_chi
sum(hybrid_chi$observed)

chisq.test(x = observed_freq_indirect_t, p = expected_studylength)
nutrient_chi <- chisq.test(x = observed_freq_nutrient_t, p = expected_studylength)
nutrient_chi
sum(nutrient_chi$observed)
nutrient_chi$observed
nutrient_chi$expected

chisq.test(x = observed_freq_other_t, p = expected_studylength)
chisq.test(x = observed_freq_production_t, p = expected_studylength)
