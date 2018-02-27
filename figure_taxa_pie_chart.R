## Read IN DATA ####
source("~/Desktop/Impacts Systematic Review/scripts/impacts_systematic_review/clean_raw_data.R")

## LOAD PACKAGES ####
library(ggplot2)
library(dplyr)
library(ggthemes)
library(grid)

## ORGANIZE DATA ####
head(raw_data)
subset_species_and_taxa <- dplyr::select(raw_data, invasivespecies, invasivespeciestaxa)
unique_species_and_taxa <- unique(subset_species_and_taxa)
head(unique_species_and_taxa)
dim(unique_species_and_taxa)
counted_unique_species <- as.data.frame(dplyr::count(unique_species_and_taxa, invasivespeciestaxa))
labels_unique_species <- as.character(counted_unique_species$invasivespeciestaxa)
dim(counted_unique_species)
counted_unique_species
sum(counted_unique_species$n)
# Organize table of broad taxonomic groups
counted_broad_taxa <- as.data.frame(dplyr::count(raw_data, invasivespeciestaxa)) 
order(counted_broad_taxa$n)
labels_new <- as.character(counted_broad_taxa$invasivespeciestaxa)
dim(counted_broad_taxa)
counted_broad_taxa

## MAKE FIGURES ####
lbls <- counted_unique_species$invasivespeciestaxa
pie <- pie(counted_unique_species$n,labels = lbls, col=rainbow(length(lbls)), init.angle=-30)

# Save figure
pdf(file="~/Desktop/Impacts Systematic Review/figures/figure_taxaonomy_pie_chart.pdf")
pie(counted_unique_species$n,labels = lbls, col=rainbow(length(lbls)), init.angle=-30)
dev.off()
dev.off()
