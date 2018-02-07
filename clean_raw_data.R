## LOAD PACKAGES ####
library(dplyr)
## READ IN DATA ####
raw_data <- read.csv("~/Desktop/Impacts Systematic Review/Crystal-Ornelas_et_al_SR_v16.csv", header=TRUE)

#creating new column with years binned
raw_data$yearbinned <- cut(raw_data$publicationyear, breaks = c(1998,2004,2010,2017), labels = c("1999-2004","2005-2010","2011-2016"))

# Creating new column with study length binned
temporal_raw <- dplyr::select(raw_data,impacttype,publicationyear,firstyeardetected,firstyearatsite,yearbegins,yearends,studylength)

# R doesn't know what to do with <1 factor, so replace with numerical placeholder
temporal_raw$studylength <- as.character(temporal_raw$studylength) # first make it a character vector
temporal_raw$studylength[temporal_raw$studylength == "<1"] <- "0.5" # then replace <1 with a placeholder of .5
head(temporal_raw)
temporal_raw$studylength <- as.numeric(temporal_raw$studylength) # now conver the study length column back to numeric
temporal_raw # check to make sure all numbers remained

# Make bins so that they align with strayer categories
temporal_raw$studylengthbinned <- cut(temporal_raw$studylength, breaks = c(0,1,3,10,250), labels = c("0-1","1.1-3","3.1-10",">10"))
head(temporal_raw)

