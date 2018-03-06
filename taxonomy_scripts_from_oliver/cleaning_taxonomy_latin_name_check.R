library(taxize)
library(dplyr)

raw_data <- read.csv("~/Desktop/Impacts Systematic Review/Crystal-Ornelas_et_al_SR_v12.csv", header=TRUE)

# Select just the latinname row
head(raw_data)
latin_names_for_all_species <- select(raw_data, latinname)
dim(latin_names_for_all_species)
dim(table(latin_names_for_all_species)) # 653 unique species names when we don't correct using COL

# Pick data source as catalogue of life
sources <- gnr_datasources()
sources
col <- sources$id[sources$title == 'Catalogue of Life'] # pick the name data source we want

# Return latin names
latin_name_check <- gnr_resrolve(names = as.character(latin_names_for_all_species$latinname), data_source_ids=col)
latin_name_check
