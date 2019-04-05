## READ IN DATA ####
source("~/Desktop/ch2_impacts_systematic_review/scripts/impacts_systematic_review/clean_raw_data.R")

## LOAD PACKAGES ####
library(dplyr)

## ORGANIZE DATA ####
head(raw_data)
ecosystem_raw <- dplyr::select(raw_data,ecosystem)
head(ecosystem_raw)
count_of_ecosystem <- plyr::count(ecosystem_raw$ecosystem)
count_of_ecosystem

