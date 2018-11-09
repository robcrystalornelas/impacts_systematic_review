## LOAD PACKAGES ####
library(plyr)
library(tidyr)
library(dplyr)
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

counts_of_impact_and_taxa <- plyr::count(impact_and_taxa)

########################################################
############### Chi-Squared w/ proportions
##########################################
table_impacts_and_taxa <- dplyr::tbl_df(counts_of_impact_and_taxa)
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
# # Proportions of taxa WITHIN fitness
# # First, make sure all combinations of impact and taxa are complete, so we can get real proportions
# table_impacts_and_taxa
complete_table_impacts_and_taxa <- tidyr::complete(table_impacts_and_taxa, impacttype, invasivespeciestaxa, fill = list(freq=0))


#######
## and then reverse. get pattern of impacts, and see if they match across taxonomic groups
#####
# There are the proportions of impacts
counts_of_impact <- plyr::count(impact_and_taxa$impacttype)
head(counts_of_impact)
counts_of_impact
setDT(counts_of_impact)[, Prop := freq/sum(freq)]
prop_of_impact_type <- counts_of_impact$Prop
prop_of_impact_type
counts_of_taxa <- plyr::count(impact_and_taxa$invasivespeciestaxa)

counts_of_taxa
# exclude any less than 50, aquatic plant, bacteria, bird, forest pathogen, fungi, terrestrial invter, moss
subset_of_algae <- filter(complete_table_impacts_and_taxa, invasivespeciestaxa == "algae and seaweed")
subset_of_algae
observed_freq_algae <- subset_of_algae$freq
observed_freq_algae

subset_of_bac <- filter(complete_table_impacts_and_taxa, invasivespeciestaxa == "bacteria")
subset_of_bac
observed_freq_bac <- subset_of_bac$freq
observed_freq_bac

subset_of_moss <- filter(complete_table_impacts_and_taxa, invasivespeciestaxa == "moss")
subset_of_moss
observed_freq_moss <- subset_of_moss$freq
observed_freq_moss

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
algae_chi <- chisq.test(x = observed_freq_algae, p = prop_of_impact_type, simulate.p.value = TRUE)
algae_chi
algae_chi$observed
algae_chi$expected

chi_amphib<-chisq.test(x = observed_freq_ampihb, p = prop_of_impact_type, simulate.p.value = TRUE)
chi_amphib
chi_amphib$observed
chi_amphib$expected

chi_aquatic <- chisq.test(x = observed_freq_aquaticplant, p = prop_of_impact_type, simulate.p.value = TRUE)
chi_aquatic
chi_aquatic$observed
chi_aquatic$expected
chisq.test(x = observed_freq_bird, p = prop_of_impact_type, simulate.p.value = TRUE)
chisq.test(x = observed_freq_bac, p = prop_of_impact_type, simulate.p.value = TRUE)
chisq.test(x = observed_freq_crust, p = prop_of_impact_type, simulate.p.value = TRUE)
chisq.test(x = observed_freq_fish, p = prop_of_impact_type, simulate.p.value = TRUE)
fungi_chi <- chisq.test(x = observed_freq_fungi, p = prop_of_impact_type, simulate.p.value = TRUE)
fungi_chi
fungi_chi$observed
fungi_chi$expected
chisq.test(x = observed_freq_grass, p = prop_of_impact_type, simulate.p.value = TRUE)
chisq.test(x = observed_freq_herb, p = prop_of_impact_type, simulate.p.value = FALSE)
chi_herb <- chisq.test(x = observed_freq_herb, p = prop_of_impact_type, simulate.p.value = FALSE)
chi_herb$observed

chisq.test(x = observed_freq_insect, p = prop_of_impact_type, simulate.p.value = TRUE)
chisq.test(x = observed_freq_mammal, p = prop_of_impact_type, simulate.p.value = TRUE)
marine_chi <- chisq.test(x = observed_freq_marineinvert, p = prop_of_impact_type, simulate.p.value = TRUE)
marine_chi
marine_chi$observed
marine_chi$expected

moll_chi <- chisq.test(x = observed_freq_moll, p = prop_of_impact_type, simulate.p.value = TRUE)
moll_chi
moll_chi$observed
moll_chi$expected

chisq.test(x = observed_freq_moss, p = prop_of_impact_type, simulate.p.value = TRUE)
chisq.test(x = observed_freq_terrestrialinvert, p = prop_of_impact_type, simulate.p.value = TRUE)
chisq.test(x = observed_freq_tree, p = prop_of_impact_type, simulate.p.value = TRUE)
chisq.test(x = observed_freq_forest, p = prop_of_impact_type, simulate.p.value = TRUE)

## Subset of 10
algae_chi <- chisq.test(x = observed_freq_algae, p = prop_of_impact_type)
algae_chi
algae_chi$observed
algae_chi$expected

chi_amphib<-chisq.test(x = observed_freq_ampihb, p = prop_of_impact_type)
chi_amphib
chi_amphib$observed
chi_amphib$expected
chisq.test(x = observed_freq_crust, p = prop_of_impact_type)
chisq.test(x = observed_freq_fish, p = prop_of_impact_type)
chisq.test(x = observed_freq_grass, p = prop_of_impact_type)
herb_chi <- chisq.test(x = observed_freq_herb, p = prop_of_impact_type)
herb_chi
herb_chi$observed
herb_chi$expected

chisq.test(x = observed_freq_insect, p = prop_of_impact_type)
chisq.test(x = observed_freq_mammal, p = prop_of_impact_type)
moll_chi <- chisq.test(x = observed_freq_moll, p = prop_of_impact_type)
moll_chi
moll_chi$observed
moll_chi$expected
chisq.test(x = observed_freq_tree, p = prop_of_impact_type)


