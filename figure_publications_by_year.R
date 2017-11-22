##########################
##### Figure showing count of publications by year 
##########################
library(ggplot2)
library(dplyr)
raw_data <- read.csv("~/Desktop/Impacts Systematic Review/Crystal-Ornelas_et_al_SR_v12.csv", header=TRUE)
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

# Make the histogram with ggplot
binsize <- diff(range(distinct_code_and_publication_year$publicationyear))/17 #set to a total of 17 bins, one for each year
ggplot(distinct_code_and_publication_year, aes(publicationyear)) + 
  geom_histogram(binwidth = binsize, fill = "deepskyblue3", colour = "white")
