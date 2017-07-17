library(dplyr)

# Import CSV file of systematic review
# Include argument na.strings = ""
impacts_raw_data <- read.csv("~/Desktop/Impacts Systematic Review/Pecchia_et_al_Impacts_Worksheet_v8.csv", header=TRUE, na.strings = "")
head(impacts_raw_data)

levels(impacts_raw_data$studylength)
length_less_than_1 <- filter(impacts_raw_data, studylength == "<1")

length_one_to_five <- filter(impacts_raw_data, studylength == c("1","1.5","2","3","4","5"))
