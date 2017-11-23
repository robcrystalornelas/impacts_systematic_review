# Script to check that there is one common name matches one species name

library(dplyr)

raw_data <- read.csv("~/Desktop/Impacts Systematic Review/Crystal-Ornelas_et_al_SR_v12.csv", header=TRUE)

common_and_species <- select(raw_data, invasivespecies, latinname)

distinct_combinations_of_names <- distinct(common_and_species)

arrange(distinct_combinations_of_names, latinname)

