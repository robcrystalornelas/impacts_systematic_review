# Script to check that there is one common name matches one species name

library(dplyr)

raw_data <- read.csv("~/Desktop/Impacts Systematic Review/Crystal-Ornelas_et_al_SR_v14.csv", header=TRUE)

common_names <- select(raw_data, invasivespecies)
latin_names <- select(raw_data, latinname)


distinct_common_names <- distinct(common_names)
distinct_common_names <- arrange(distinct_common_names, invasivespecies)
write.csv(distinct_common_names, file = "distinct_common_names.csv")

distinct_latin_names <- distinct(latin_names)
distinct_latin_names <- arrange(distinct_latin_names, latinname)
write.csv(distinct_latin_names, file = "distinct_latin_names.csv")

common_names_then_latin <- raw_data %>% 
  select(invasivespecies, latinname) %>%  # pick just two rows with common and latic
  distinct() %>% # unique combo of species names
  arrange(invasivespecies) # arrange them in alphabetical order by common name
write.csv(common_names_then_latin, file = "unique_common_and_latin_combined.csv")
dim(common_names_then_latin)
head(common_names_then_latin)

latin_names_then_common <- raw_data %>%
  select(latinname, invasivespecies) %>%
  distinct() %>%
  arrange(latinname)
write.csv(latin_names_then_common, file = "unique_latin_and_common_combined.csv")
