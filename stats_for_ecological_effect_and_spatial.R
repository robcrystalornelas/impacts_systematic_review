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
impact_and_spatial <- dplyr::select(raw_data, impacttype, spatialscale)
head(impact_and_spatial)

class(impact_and_spatial$impacttype)
counts_of_impact <- plyr::count(impact_and_spatial$impacttype)
counts_of_impact
full_chi <- chisq.test(counts_of_impact$freq)
full_chi
full_chi$expected
counts_of_impact_and_spatial <- plyr::count(impact_and_spatial)


table_impacts_and_spatial <- dplyr::tbl_df(counts_of_impact_and_spatial)
complete_table_impacts_and_spatial <- tidyr::complete(table_impacts_and_spatial, impacttype, spatialscale, fill = list(freq=0))
counts_of_impact <- plyr::count(impact_and_spatial$impacttype)
head(counts_of_impact)
counts_of_impact
setDT(counts_of_impact)[, Prop := freq/sum(freq)]
prop_of_impact_type <- counts_of_impact$Prop
prop_of_impact_type
counts_of_spatial <- plyr::count(impact_and_spatial$spatialscale)
counts_of_spatial

# Get groups of different spatial scales
subset_of_local <- filter(complete_table_impacts_and_spatial, spatialscale == "local")
observed_freq_local <- subset_of_local$freq

subset_of_lab <- filter(complete_table_impacts_and_spatial, spatialscale == "lab")
observed_freq_lab <- subset_of_local$freq

subset_of_regional <- filter(complete_table_impacts_and_spatial, spatialscale == "regional")
observed_freq_regional <- subset_of_regional$freq

subset_of_country <- filter(complete_table_impacts_and_spatial, spatialscale == "country")
observed_freq_country <- subset_of_country$freq

subset_of_global <- filter(complete_table_impacts_and_spatial, spatialscale == "global")
observed_freq_global <- subset_of_global$freq

local_chi <- chisq.test(x = observed_freq_local, p = prop_of_impact_type, simulate.p.value = FALSE)
local_chi
local_chi$observed)
local_chi$expected

lab_chi <- chisq.test(x = observed_freq_lab, p = prop_of_impact_type, simulate.p.value = TRUE)
lab_chi
lab_chi$observed
lab_chi$expected

regional_chi <- chisq.test(x = observed_freq_regional, p = prop_of_impact_type, simulate.p.value = TRUE)
prop_of_impact_type
regional_chi
regional_chi$observed
regional_chi$expected

country_chi <- chisq.test(x = observed_freq_country, p = prop_of_impact_type, simulate.p.value = TRUE)
country_chi
country_chi$observed
country_chi$expected

global_chi <- chisq.test(x = observed_freq_global, p = prop_of_impact_type, simulate.p.value = TRUE)
global_chi
sum(global_chi$observed)
global_chi$expected


