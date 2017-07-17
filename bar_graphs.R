library(ggplot2)
library(dplyr)
impacts_raw_data <- read.csv("~/Desktop/Impacts Systematic Review/Pecchia_et_al_Impacts_Worksheet_v8.csv", header=TRUE)
head(impacts_raw_data)

levels(impacts_raw_data$ecosystem)
levels(impacts_raw_data$impacttype)
dim(impacts_raw_data)
impacts_raw_data<-impacts_raw_data[-715,]
dim(impacts_raw_data)

hist(impacts_raw_data$publicationyear)
impacts_first_two_columns <- impacts_raw_data[,1:2]
head(impacts_first_two_columns)
impacts_2016 <- filter(impacts_first_two_columns, publicationyear == 2016)
dim(unique(impacts_2016))
impacts_2015 <- filter(impacts_first_two_columns, publicationyear == 2015)
dim(unique(impacts_2015))
impacts_2014 <- filter(impacts_first_two_columns, publicationyear == 2014)
dim(unique(impacts_2014))
impacts_2013 <- filter(impacts_first_two_columns, publicationyear == 2013)
dim(unique(impacts_2013))
impacts_2012 <- filter(impacts_first_two_columns, publicationyear == 2012)
dim(unique(impacts_2012))
