## READ IN DATA ####
source("~/Desktop/Impacts Systematic Review/scripts/impacts_systematic_review/clean_raw_data.R")

## LOAD PACKAGES ####
library(dplyr)

## ORGANIZE DATA ####
head(raw_data)
spatial_raw <- dplyr::select(raw_data,spatialscale)
head(spatial_raw)
count_of_spatial_scale <- plyr::count(spatial_raw$spatialscale)
count_of_spatial_scale
prop.table(plyr::count(count_of_spatial_scale$freq))
