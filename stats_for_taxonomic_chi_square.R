## LOAD PACKAGES ####

## READ IN DATA ####
source("~/Desktop/ch2_impacts_systematic_review/scripts/impacts_systematic_review/clean_raw_data.R") # This tells R to run our entire cleaning script so that we have

## Prepare data for chi-squared
head(raw_data)

taxonomic_data <- select(raw_data, latinname,invasivespeciestaxapysek) %>% # select only the column we need
unique() %>% # We get a list of all the unique species in our databases, counted only once
  count(invasivespeciestaxapysek) # then we count the number of taxa these species represent
taxonomic_data

taxonomic_data
chisq.test(taxonomic_data$n)
