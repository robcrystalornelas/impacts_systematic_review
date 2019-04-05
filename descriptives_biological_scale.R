## READ IN DATA ####
source("~/Desktop/ch2_impacts_systematic_review/scripts/impacts_systematic_review/clean_raw_data.R")

## LOAD PACKAGES ####
library(dplyr)

# Count up the biological scales
head(raw_data)
biological_scale <- dplyr::select(raw_data, impact_category_lockwood)
head(biological_scale)
dim(biological_scale)
counted_biological_scale <- as.data.frame(dplyr::count(raw_data, impact_category_lockwood)) 
counted_biological_scale
