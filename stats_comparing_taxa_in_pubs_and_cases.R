## Read IN DATA ####
source("~/Desktop/Impacts Systematic Review/scripts/impacts_systematic_review/clean_raw_data.R")

## LOAD PACKAGES ####
library(ggplot2)
library(dplyr)
library(ggthemes)
library(grid)


# Expected proportions for UNIQUE species
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

# counts of taxa in all case studies
subset_species_and_case <- dplyr::select(raw_data, code, invasivespecies, invasivespeciestaxa)
counted_case_studies_taxa <- as.data.frame(dplyr::count(subset_species_and_case, invasivespeciestaxa))
head(counted_case_studies_taxa) # this shows all 2,000 + case studies

# counts of unique taxa across publications
subset_species_and_publications <- dplyr::select(raw_data, code, invasivespecies, invasivespeciestaxa)
head(subset_species_and_publications)
unique_species_and_publications <- unique(subset_species_and_publications)
head(unique_species_and_publications)
counted_species_and_publications <- as.data.frame(dplyr::count(unique_species_and_publications, invasivespeciestaxa))
head(counted_species_and_publications) # this shows all 2,000 + case studies

# comparing expected based on unique species to case studies
counted_case_studies_taxa_n <- counted_case_studies_taxa$n
chisq.test(x = counted_case_studies_taxa_n, p = expected_taxa_prop)

# comparing expected based on unique species to publications
counted_species_and_publications_n <- counted_species_and_publications$n
publications_chi <- chisq.test(x = counted_species_and_publications_n, p = expected_taxa_prop)
publications_chi$observed
publications_chi$expected

# comparing taxonomic trends in publications and case studies
# First get case study proporitions
counted_case_studies_taxa
setDT(counted_case_studies_taxa)[, Prop := n/sum(n)]
counted_case_studies_taxa_prop <- counted_case_studies_taxa$Prop
counted_case_studies_taxa_prop
expected_taxa_prop
expected_taxa_count <- counted_unique_species$n

