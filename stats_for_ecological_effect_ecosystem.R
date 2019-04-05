## LOAD PACKAGES ####
library(plyr)
library(tidyr)
library(dplyr)
library(ggthemes)
library(devtools)
library(broom)
library(data.table)

## READ IN DATA ####
source("~/Desktop/ch2_impacts_systematic_review/scripts/impacts_systematic_review/clean_raw_data.R") # This tells R to run our entire cleaning script so that we have

## CLEAN DATA ####
impact_and_ecosystem <- dplyr::select(raw_data, impacttype, ecosystem)
head(impact_and_ecosystem)

class(impact_and_ecosystem$impacttype)
counts_of_impact <- plyr::count(impact_and_ecosystem$impacttype)
counts_of_impact
full_chi <- chisq.test(counts_of_impact$freq)
full_chi
full_chi$expected
counts_of_impact_and_ecosystem <- plyr::count(impact_and_ecosystem)

########################################################
############### Chi-Squared w/ proportions
##########################################
table_impacts_and_ecosystem <- dplyr::tbl_df(counts_of_impact_and_ecosystem)
table_impacts_and_ecosystem
unique(table_impacts_and_ecosystem$impacttype)

# Proportions of each impact type
table_impacts_and_ecosystem %>%
  group_by(impacttype) %>%
  dplyr::summarise (n = n()) %>%
  mutate(freq = n / sum(n))

# # Proportions of taxa WITHIN fitness
# # First, make sure all combinations of impact and taxa are complete, so we can get real proportions
# table_impacts_and_taxa
complete_table_impacts_and_ecosystem <- tidyr::complete(table_impacts_and_ecosystem, impacttype, ecosystem, fill = list(freq=0))
counts_of_impact <- plyr::count(impact_and_ecosystem$impacttype)
head(counts_of_impact)
counts_of_impact
setDT(counts_of_impact)[, Prop := freq/sum(freq)]
prop_of_impact_type <- counts_of_impact$Prop
prop_of_impact_type
counts_of_ecosystem <- plyr::count(impact_and_ecosystem$ecosystem)
counts_of_ecosystem

# Get groups of different ecosystems
subset_of_coastal <- filter(complete_table_impacts_and_ecosystem, ecosystem == "coastal")
observed_freq_coastal <- subset_of_coastal$freq

subset_of_desert <- filter(complete_table_impacts_and_ecosystem, ecosystem == "desert")
observed_freq_desert <- subset_of_desert$freq

subset_of_estuarine <- filter(complete_table_impacts_and_ecosystem, ecosystem == "estuarine")
observed_freq_estuarine <- subset_of_estuarine$freq

subset_of_forest <- filter(complete_table_impacts_and_ecosystem, ecosystem == "forest")
observed_freq_forest <- subset_of_forest$freq

subset_of_grassland <- filter(complete_table_impacts_and_ecosystem, ecosystem == "grassland")
observed_freq_grassland <- subset_of_grassland$freq

subset_of_intertidal <- filter(complete_table_impacts_and_ecosystem, ecosystem == "intertidal")
observed_freq_intertidal <- subset_of_intertidal$freq

subset_of_island <- filter(complete_table_impacts_and_ecosystem, ecosystem == "island")
observed_freq_island <- subset_of_island$freq

subset_of_lentic <- filter(complete_table_impacts_and_ecosystem, ecosystem == "lentic")
observed_freq_lentic <- subset_of_lentic$freq

subset_of_lotic <- filter(complete_table_impacts_and_ecosystem, ecosystem == "lotic")
observed_freq_lotic <- subset_of_lotic$freq

subset_of_mountain<- filter(complete_table_impacts_and_ecosystem, ecosystem == "mountain")
observed_freq_mountain <- subset_of_mountain$freq

subset_of_oceanic <- filter(complete_table_impacts_and_ecosystem, ecosystem == "oceanic")
observed_freq_oceanic <- subset_of_oceanic$freq

subset_of_shrubland <- filter(complete_table_impacts_and_ecosystem, ecosystem == "shrubland")
observed_freq_shrubland <- subset_of_shrubland$freq

subset_of_urban <- filter(complete_table_impacts_and_ecosystem, ecosystem == "urban")
observed_freq_urban <- subset_of_urban$freq


# Run chi-sqaured tests
coastal_chi <- chisq.test(x = observed_freq_coastal, p = prop_of_impact_type, simulate.p.value = TRUE)
coastal_chi
coastal_chi$observed
coastal_chi$expected

desert_chi <- chisq.test(x = observed_freq_desert, p = prop_of_impact_type, simulate.p.value = TRUE)
desert_chi
desert_chi$observed
desert_chi$expected

estuarine_chi <- chisq.test(x = observed_freq_estuarine, p = prop_of_impact_type, simulate.p.value = TRUE)
estuarine_chi
estuarine_chi$observed
estuarine_chi$expected

forest_chi <- chisq.test(x = observed_freq_forest, p = prop_of_impact_type, simulate.p.value = TRUE)
forest_chi
forest_chi$observed
forest_chi$expected

grassland_chi <- chisq.test(x = observed_freq_grassland, p = prop_of_impact_type, simulate.p.value = TRUE)
grassland_chi
grassland_chi$observed
grassland_chi$expected

intertidal_chi <- chisq.test(x = observed_freq_intertidal, p = prop_of_impact_type, simulate.p.value = FALSE)
intertidal_chi
intertidal_chi$observed
intertidal_chi$expected

island_chi <- chisq.test(x = observed_freq_island, p = prop_of_impact_type, simulate.p.value = TRUE)
island_chi
island_chi$observed
island_chi$expected

lentic_chi <- chisq.test(x = observed_freq_lentic, p = prop_of_impact_type, simulate.p.value = TRUE)
lentic_chi
lentic_chi$observed
lentic_chi$expected

lotic_chi <- chisq.test(x = observed_freq_lotic, p = prop_of_impact_type, simulate.p.value = TRUE)
lotic_chi
lotic_chi$observed
lotic_chi$expected

mountain_chi <- chisq.test(x = observed_freq_mountain, p = prop_of_impact_type, simulate.p.value = TRUE)
mountain_chi
mountain_chi$observed
mountain_chi$expected

oceanic_chi <- chisq.test(x = observed_freq_oceanic, p = prop_of_impact_type, simulate.p.value = TRUE)
oceanic_chi
oceanic_chi$observed
oceanic_chi$expected

shrubland_chi <- chisq.test(x = observed_freq_shrubland, p = prop_of_impact_type, simulate.p.value = TRUE)
shrubland_chi
shrubland_chi$observed
shrubland_chi$expected

urban_chi <- chisq.test(x = observed_freq_urban, p = prop_of_impact_type, simulate.p.value = TRUE)
urban_chi
urban_chi$observed
urban_chi$expected

