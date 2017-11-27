# Script to check that there is one common name matches one species name

library(dplyr)

raw_data <- read.csv("~/Desktop/Impacts Systematic Review/Crystal-Ornelas_et_al_SR_v13.csv", header=TRUE)

common_names <- select(raw_data, invasivespecies)
latin_names <- select(raw_data, latinname)


distinct_common_names <- distinct(common_names)
distinct_common_names <- arrange(distinct_common_names, invasivespecies)
write.csv(distinct_common_names, file = "distinct_common_names.csv")

distinct_latin_names <- distinct(latin_names)
distinct_latin_names <- arrange(distinct_latin_names, latinname)
write.csv(distinct_latin_names, file = "distinct_latin_names.csv")
