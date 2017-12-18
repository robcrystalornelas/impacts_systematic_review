##########################
##### Figure showing count of publications by year 
##########################
library(ggplot2)
library(dplyr)
raw_data <- read.csv("~/Desktop/Impacts Systematic Review/Crystal-Ornelas_et_al_SR_v14.csv", header=TRUE)
head(raw_data)

levels(raw_data$ecosystem)
levels(raw_data$impacttype)
dim(raw_data)

# Barplot for number of publications by year for CASE STUDIES
hist(raw_data$publicationyear)

# Barplot for number of publications by year for ARTICLES
# Fist, get the two rows that will show use unique publications
names(raw_data)
code_and_publication_year <- dplyr::select(raw_data, code, publicationyear)
distinct_code_and_publication_year <- distinct(code_and_publication_year)
dim(distinct_code_and_publication_year)

# Test out the histogram
hist(distinct_code_and_publication_year$publicationyear)

# Make the histogram with ggplot for UNIQUE PUBLICATIONS
binsize <- diff(range(distinct_code_and_publication_year$publicationyear))/17 #set to a total of 17 bins, one for each year
ggplot(distinct_code_and_publication_year, aes(publicationyear)) + 
  geom_histogram(binwidth = binsize, fill = "deepskyblue3", colour = "white")

# Make the histogram with ggplot for ALL CASE STUDIES
code_and_case_studies <- dplyr::select(raw_data,code,publicationyear)
binsize_case_studies <- diff(range(code_and_case_studies$publicationyear))/17 #set to a total of 17 bins, one for each year
gg_case_studies <- ggplot(code_and_case_studies, aes(publicationyear)) + 
  geom_histogram(binwidth = binsize_case_studies, fill = "lightgreen", colour = "white")
gg_case_studies <- gg_case_studies + theme(axis.text.x = element_text(size = 15))
gg_case_studies
