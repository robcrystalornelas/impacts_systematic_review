library(countrycode)
library(dplyr)

# Add a column w/ country code
raw_data <- read.csv("~/Desktop/Impacts Systematic Review/Crystal-Ornelas_et_al_SR_v19.csv", header=TRUE)

head(raw_data)
code_and_country <- select(raw_data, code, country)
head(code_and_country)
country_code_df <- countrycode(as.character(code_and_country$country), origin = "country.name", destination = "iso3n", warn = TRUE)
class(country_code_df)

# join the country code vector to original data.frame
head(country_code_df)
# country_code_df <- dplyr::rename(country_code_df, isocode = .) # columns need to have same name, new name first

raw_data$isocountrycode <- country_code_df
head(raw_data)

country_code_df_2 <- countrycode(as.character(code_and_country$country), origin = "country.name", destination = "country.name", warn = TRUE)

write.csv(raw_data, file = "/Users/rpecchia/Desktop/Impacts Systematic Review/output/raw_data_with_ISO_3.csv")

