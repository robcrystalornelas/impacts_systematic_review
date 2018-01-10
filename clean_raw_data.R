## LOAD PACKAGES ####
library(dplyr)

## READ IN DATA ####
raw_data <- read.csv("~/Desktop/Impacts Systematic Review/Crystal-Ornelas_et_al_SR_v15.csv", header=TRUE)

#creating new column with years binned
raw_data$yearbinned <- cut(raw_data$publicationyear, breaks = c(1998,2004,2010,2017), labels = c("1999-2004","2005-2010","2011-2016"))

