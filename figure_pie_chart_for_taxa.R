#### Prepare data for stacked barplots ####

library(dplyr)
impacts_raw_data <- read.csv("~/Desktop/Impacts Systematic Review/Pecchia_et_al_Impacts_Worksheet_v8.csv", header=TRUE)

head(iris)
head(impacts_raw_data)

subset_species_and_taxa <- select(impacts_raw_data, invasivespecies, invasivespeciestaxa)
unique_species_and_taxa <- unique(subset_species_and_taxa)
head(unique_species_and_taxa)
dim(unique_species_and_taxa)
count(unique_species_and_taxa, invasivespeciestaxa)

# Now, focus on fish, and diversity within the fish group
subset_species_and_taxa <- select(impacts_raw_data, invasivespecies, invasivespeciestaxa)
subset_fish_species <- dplyr::filter(subset_species_and_taxa, invasivespeciestaxa=="fish")
dim(subset_fish_species)
counted_fish_species <- as.data.frame(count(subset_fish_species, invasivespecies))
counted_fish_species

# Now, focus on mammals, and diversity within the mammals group
subset_species_and_taxa <- select(impacts_raw_data, invasivespecies, invasivespeciestaxa)
subset_mammal_species <- dplyr::filter(subset_species_and_taxa, invasivespeciestaxa=="mammal")
dim(subset_mammal_species)
counted_mammal_species <- as.data.frame(count(subset_mammal_species, invasivespecies))
counted_mammal_species

# Now, focus on plants, and diversity within the plants group
subset_species_and_taxa <- select(impacts_raw_data, invasivespecies, invasivespeciestaxa)
subset_herbs <- dplyr::filter(subset_species_and_taxa, invasivespeciestaxa=="herbaceous plant")
subset_tree <- dplyr::filter(subset_species_and_taxa, invasivespeciestaxa=="tree")
subset_grass <- dplyr::filter(subset_species_and_taxa, invasivespeciestaxa=="grasses")
subset_plant_species<-rbind(subset_herbs,subset_tree,subset_grass)
dim(subset_plant_species)
counted_plant_species <- as.data.frame(count(subset_plant_species, invasivespecies))
counted_plant_species
pie(counted_plant_species$n,counted_plant_species$invasivespecies)
