## LOAD PACKAGES ####
library(plyr)
library(dplyr)
library(tidyr)
library(ggthemes)
library(devtools)
library(broom)
library(data.table)

## READ IN DATA ####
source("~/Desktop/Impacts Systematic Review/scripts/impacts_systematic_review/clean_raw_data.R") # This tells R to run our entire cleaning script so that we have

## CLEAN DATA ####
impact_and_taxa <- dplyr::select(raw_data, impacttype, invasivespeciestaxa)
head(impact_and_taxa)

class(impact_and_taxa$impacttype)
counts_of_impact <- plyr::count(impact_and_taxa$impacttype)
counts_of_impact
full_chi <- chisq.test(counts_of_impact$freq)
full_chi
full_chi$expected

# loglinear test
# This equation is for a saturated loglinear model
head(impact_and_taxa)
counts_of_impact_and_taxa <- plyr::count(impact_and_taxa)
counts_of_impact_and_taxa
glm1 <- glm(freq ~ impacttype*invasivespeciestaxa, data = counts_of_impact_and_taxa, family = poisson())
summary(glm1)

# Export coefficients into csv
tidy_lmfit <- tidy(glm1)
tidy_lmfit
write.csv(tidy_lmfit, "/Users/rpecchia/Desktop/Impacts Systematic Review/output/coefficients_for_impact_and_taxa.csv")
# This lets us see if impact type is significant, if study length is signficant and 
# if the interaction of impact type and study length are significant
# is this model a good fit?
# First, look at residual deviance in the summary above (really low! that's good)
# then, the p-value in following should be ABOVE .05

# Should we get rid of interaction
drop1(glm1, test = "Chisq")
# second line of results "impacttpye:studylnegthbinned is the model without the interaction
# Since p-value is below .05, we should drop the interaction

# so let's drop that interaction and just look at main effects.
# all main effects are signficant
# This model assumes that study length and impact type are independent of each other
# This is called the independence model
# seems great! all 
glm2 <- glm(freq ~ impacttype + invasivespeciestaxa, data = counts_of_impact_and_taxa, family = poisson())
summary(glm2) # but the residual deviance is high, so lets look at the residuals.
# high residuals might mean poor model fit
# Rule of thumb is that residual deviance should be close to degrees of freedom

# we can used chisq test to see if expected freuencies satisfy the simpler model
# a low p-value here indicates that they do not!
pchisq(deviance(glm2), df = df.residual(glm2), lower.tail = F)
# Taken together, we should feel confident going with the more complex model

# glm1 is fully saturated and really complex. glm2 is really simple, maybe overly simple
# check an anova to see if one model is better than the other
# if p is above .05 then both models fit equally...go with the simple
anova(glm1, glm2)
# then take the deviance and put it into chi square

pchisq(99.4, df = 1, lower.tail = F) # actually the two models are really different!

########################################################
############### Chi-Squared w/ proportions
##########################################
table_impacts_and_taxa <- tbl_df(counts_of_impact_and_taxa)
table_impacts_and_taxa
unique(table_impacts_and_taxa$impacttype)

# Proportions of each impact type
table_impacts_and_taxa %>%
  group_by(impacttype) %>%
  dplyr::summarise (n = n()) %>%
  mutate(freq = n / sum(n))

# Proportions of each taxa using UNIQUE SPECIES
subset_species_and_taxa <- dplyr::select(raw_data, invasivespecies, invasivespeciestaxa)
unique_species_and_taxa <- unique(subset_species_and_taxa)
head(unique_species_and_taxa)
dim(unique_species_and_taxa)
counted_unique_species <- as.data.frame(dplyr::count(unique_species_and_taxa, invasivespeciestaxa))
dim(counted_unique_species)
head(counted_unique_species)

setDT(counted_unique_species)[, Prop := n/sum(n)]
expected_taxa_prop <- counted_unique_species$Prop
expected_taxa_prop
expected_taxa_count <- counted_unique_species$n

# Proportions of taxa WITHIN fitness
# First, make sure all combinations of impact and taxa are complete, so we can get real proportions
table_impacts_and_taxa
complete_table_impacts_and_taxa <- tidyr::complete(table_impacts_and_taxa, impacttype, invasivespeciestaxa, fill = list(freq=0))
unique(table_impacts_and_taxa$invasivespeciestaxa)
complete_table_impacts_and_taxa

# Get observe freq for all categories
subset_of_abundance <- filter(complete_table_impacts_and_taxa, impacttype == "abundance")
observed_freq_abundance <- subset_of_abundance$freq

subset_of_behavior <- filter(complete_table_impacts_and_taxa, impacttype == "behavior")
observed_freq_behavior <- subset_of_behavior$freq

subset_of_diversity <- filter(complete_table_impacts_and_taxa, impacttype == "diversity")
observed_freq_diversity <- subset_of_diversity$freq

subset_of_fitness <- filter(complete_table_impacts_and_taxa, impacttype == "fitness")
observed_freq_fitness <- subset_of_fitness$freq

subset_of_growth <- filter(complete_table_impacts_and_taxa, impacttype == "growth")
observed_freq_growth <- subset_of_growth$freq

subset_of_habitatchange <- filter(complete_table_impacts_and_taxa, impacttype == "habitat change")
observed_freq_habitatchange <- subset_of_habitatchange$freq

subset_of_hybrid <- filter(complete_table_impacts_and_taxa, impacttype == "hybridization")
observed_freq_hybrid <- subset_of_hybrid$freq

subset_of_indirect <- filter(complete_table_impacts_and_taxa, impacttype == "indirect")
observed_freq_indirect <- subset_of_indirect$freq

subset_of_nutrient <- filter(complete_table_impacts_and_taxa, impacttype == "nutrient availability")
observed_freq_nutrient <- subset_of_nutrient$freq

subset_of_other <- filter(complete_table_impacts_and_taxa, impacttype == "other")
observed_freq_other <- subset_of_other$freq

subset_of_production <- filter(complete_table_impacts_and_taxa, impacttype == "production")
observed_freq_production <- subset_of_production$freq

# chi-squared for all impact types
chisq.test(x = observed_freq_abundance, p = expected_taxa_prop)
chi_behavior <- chisq.test(x = observed_freq_behavior, p = expected_taxa_prop)
chi_behavior
chi_diversity <- chisq.test(x = observed_freq_diversity, p = expected_taxa_prop)
chi_diversity
chi_diversity$observed
chi_diversity$expected
chisq.test(x = observed_freq_fitness, p = expected_taxa_prop)
chisq.test(x = observed_freq_growth, p = expected_taxa_prop)
chisq.test(x = observed_freq_habitatchange, p = expected_taxa_prop)
chisq.test(x = observed_freq_hybrid, p = expected_taxa_prop)
chisq.test(x = observed_freq_indirect, p = expected_taxa_prop)
chisq.test(x = observed_freq_nutrient, p = expected_taxa_prop)
chisq.test(x = observed_freq_other, p = expected_taxa_prop)
chisq.test(x = observed_freq_production, p = expected_taxa_prop)

#######
## and then reverse. get pattern of impacts, and see if they match across taxonomic groups
#####
# There are the proportions of impacts
counts_of_impact <- plyr::count(impact_and_taxa$impacttype)
head(counts_of_impact)
setDT(counts_of_impact)[, Prop := freq/sum(freq)]
prop_of_impact_type <- counts_of_impact$Prop
prop_of_impact_type

head(complete_table_of_impacts)
subset_of_algae <- filter(complete_table_impacts_and_taxa, invasivespeciestaxa == "algae and seaweed")
subset_of_algae
observed_freq_algae <- subset_of_algae$freq
observed_freq_algae

subset_of_amphib <- filter(complete_table_impacts_and_taxa, invasivespeciestaxa == "amphibians and reptiles")
observed_freq_ampihb <- subset_of_amphib$freq

subset_of_aquaticplant <- filter(complete_table_impacts_and_taxa, invasivespeciestaxa == "aquatic plant")
observed_freq_aquaticplant <- subset_of_aquaticplant$freq

subset_of_bird <- filter(complete_table_impacts_and_taxa, invasivespeciestaxa == "bird")
observed_freq_bird <- subset_of_bird$freq

subset_of_crust <- filter(complete_table_impacts_and_taxa, invasivespeciestaxa == "crustacean")
observed_freq_crust <- subset_of_crust$freq

subset_of_fish <- filter(complete_table_impacts_and_taxa, invasivespeciestaxa == "fish")
observed_freq_fish <- subset_of_fish$freq

subset_of_fungi <- filter(complete_table_impacts_and_taxa, invasivespeciestaxa == "fungi")
observed_freq_fungi <- subset_of_fungi$freq

subset_of_grass <- filter(complete_table_impacts_and_taxa, invasivespeciestaxa == "grasses")
observed_freq_grass <- subset_of_grass$freq

subset_of_herb <- filter(complete_table_impacts_and_taxa, invasivespeciestaxa == "herbaceous plant")
observed_freq_herb <- subset_of_herb$freq

subset_of_insect <- filter(complete_table_impacts_and_taxa, invasivespeciestaxa == "insect")
observed_freq_insect <- subset_of_insect$freq

subset_of_mammal <- filter(complete_table_impacts_and_taxa, invasivespeciestaxa == "mammal")
observed_freq_mammal <- subset_of_mammal$freq

subset_of_marineinvert <- filter(complete_table_impacts_and_taxa, invasivespeciestaxa == "marine invert")
observed_freq_marineinvert <- subset_of_marineinvert$freq

subset_of_moll <- filter(complete_table_impacts_and_taxa, invasivespeciestaxa == "molluscs")
observed_freq_moll <- subset_of_moll$freq

subset_of_terrestrialinvert <- filter(complete_table_impacts_and_taxa, invasivespeciestaxa == "terrestrial invert")
observed_freq_terrestrialinvert <- subset_of_terrestrialinvert$freq

subset_of_tree <- filter(complete_table_impacts_and_taxa, invasivespeciestaxa == "tree")
observed_freq_tree <- subset_of_tree$freq

subset_of_forest <- filter(complete_table_impacts_and_taxa, invasivespeciestaxa == "forest pathogen")
observed_freq_forest <- subset_of_forest$freq

subset_of_algae
algae_chi <- chisq.test(x = observed_freq_algae, p = prop_of_impact_type)
algae_chi
algae_chi$observed
algae_chi$expected
chi_amphib<-chisq.test(x = observed_freq_ampihb, p = prop_of_impact_type)
chi_amphib
chi_amphib$observed
chi_amphib$expected
chisq.test(x = observed_freq_aquaticplant, p = prop_of_impact_type)
chisq.test(x = observed_freq_bird, p = prop_of_impact_type)
chisq.test(x = observed_freq_crust, p = prop_of_impact_type)
chisq.test(x = observed_freq_fish, p = prop_of_impact_type)
fungi_chi <- chisq.test(x = observed_freq_fungi, p = prop_of_impact_type)
fungi_chi
fungi_chi$observed
fungi_chi$expected
chisq.test(x = observed_freq_grass, p = prop_of_impact_type)
chisq.test(x = observed_freq_herb, p = prop_of_impact_type)
chisq.test(x = observed_freq_insect, p = prop_of_impact_type)
chisq.test(x = observed_freq_mammal, p = prop_of_impact_type)
chisq.test(x = observed_freq_marineinvert, p = prop_of_impact_type)
moll_chi <- chisq.test(x = observed_freq_moll, p = prop_of_impact_type)
moll_chi
moll_chi$observed
moll_chi$expected
chisq.test(x = observed_freq_terrestrialinvert, p = prop_of_impact_type)
chisq.test(x = observed_freq_tree, p = prop_of_impact_type)
chisq.test(x = observed_freq_forest, p = prop_of_impact_type)

