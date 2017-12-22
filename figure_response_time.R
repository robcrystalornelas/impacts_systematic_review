## READ IN DATA ####
source("~/Desktop/Impacts Systematic Review/scripts/impacts_systematic_review/clean_raw_data.R")

## LOAD PACKAGES ####
library(dplyr)
library(ggplot2)
library(ggthemes)

## ORGANIZE DATA ####
response_time_data <- select(raw_data, publicationyear, ecosystem,impacttype,firstyeardetected,firstyearatsite,yearbegins,yearends,yearbinned)
head(response_time_data)

# Create new column to show response time (yearbegins - firstyeardetected)
# first, make all columns numerical
response_time_data$firstyeardetected <- as.character(response_time_data$firstyeardetected)
response_time_data$firstyeardetected <- as.numeric(response_time_data$firstyeardetected)

response_time_data$yearbegins <- as.character(response_time_data$yearbegins)
response_time_data$yearbegins <- as.numeric(response_time_data$yearbegins)

# now add new column with response time variable
response_time_data <- mutate(response_time_data, responsetime = yearbegins - firstyeardetected)
head(response_time_data)
response_time_data

# bin response time data
response_time_data$responsetimebinned <- cut(response_time_data$responsetime, breaks = c(0,1,5,10,500), labels = c("Rapid (0-1 years)","1.1-5 years","5.1-10 years","Slow (10.1+)"))

# Over 2000 NAs, so remove those
response_time_data_cc <- select(response_time_data, publicationyear,responsetimebinned)
head(response_time_data_cc)
response_time_data_cc <- response_time_data_cc[complete.cases(response_time_data_cc$responsetimebinned), ]
head(response_time_data_cc)
dim(response_time_data_cc)

## MAKE FIGURES ####
gg <- ggplot(response_time_data_cc, aes(x = responsetimebinned))
gg <- gg + geom_bar(stat="count", fill = "#fc8d62")
gg
gg <- gg + theme_tufte()
gg
gg <- gg + ylab("Frequency")
gg <- gg + xlab("Response Time")
gg <- gg + theme(axis.text=element_text(size=12), # Change tick mark label size
                 axis.title=element_text(size=14,face="bold"))
gg
pdf(file="~/Desktop/Impacts Systematic Review/figures/responsetime_barplot.pdf")
gg
dev.off()
dev.off()



