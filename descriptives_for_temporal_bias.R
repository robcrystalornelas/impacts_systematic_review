## READ IN DATA ####
source("~/Desktop/ch2_impacts_systematic_review/scripts/impacts_systematic_review/clean_raw_data.R")

## LOAD PACKAGES ####
library(dplyr)

## ORGANIZE DATA ####
head(raw_data)
temporal_raw <- dplyr::select(raw_data,publicationyear,firstyeardetected,firstyearatsite,yearbegins,yearends,studylength,samplingfrequency)
head(temporal_raw)

count(temporal_raw$firstyeardetected)
1191/2293
# So 52% of case studies did not report the year that invasive species was introduced

count(temporal_raw$samplingfrequency)

# Percentage for single measurements
1152/2293

# Percentage for multiple measurments in one year
691/2293

#percentage for measurements spaced out over more than one year
178/2293

#study design info
head(raw_data)
study_design_raw <- dplyr::select(raw_data, studydesignnew)
dplyr::count(study_design_raw,studydesignnew)

