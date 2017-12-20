## READ IN DATA ####
source("~/Desktop/Impacts Systematic Review/scripts/impacts_systematic_review/clean_raw_data.R")

## LOAD PACKAGES ####
library(dplyr)
library(ggplot2)

## ORGANIZE DATA ####
response_time_data <- select(raw_data, publicationyear, ecosystem,invasivespeciestaxa,firstyeardetected,firstyearatsite,yearbegins,yearends,yearbinned)
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

# bin response time data
response_time_data$responsetimebinned <- cut(response_time_data$responsetime, breaks = c(0,1,5,10,500), labels = c("Rapid (0-1 years)","1.1-5 years","5.1-10 years","Slow (10.1+)"))
response_time_data

# Over 2000 NAs, so remove those
response_time_data_cc <- select(response_time_data, ecosystem,publicationyear,responsetimebinned)
head(response_time_data_cc)
response_time_data_cc <- response_time_data_cc[complete.cases(response_time_data_cc$responsetimebinned), ]
head(response_time_data_cc)
dim(response_time_data_cc)
## MAKE FIGURES ####
ggplot(impact_and_taxa) +
  geom_bar(aes(x= impacttype, stat="bin", fill = invasivespeciestaxa)) + 
  facet_wrap(~invasivespeciestaxa) +
  scale_fill_manual(values = colorRampPalette(solarized_pal()(8))(16)) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

gg <- ggplot(response_time_data_cc) + 
  geom_bar(aes(x = responsetimebinned, stat = "bin", fill = ecosystem)) +
  facet_wrap(~ecosystem)
gg
gg <- gg + scale_fill_manual(values = colorRampPalette(solarized_pal()(8))(16))
gg <- gg + theme_tufte()
gg
gg <- gg + ylab("Frequency")
gg <- gg + xlab("Response Time (year species detected - year study begins)")
gg <- gg + ggtitle("Response time for invasive species research by ecosystem")
gg <- gg + theme(axis.text.x = element_text(angle = 60, hjust = 1))
gg <- gg + theme(legend.position="none")
gg

pdf(file="~/Desktop/Impacts Systematic Review/figures/responsetime_barplot_fascet_by_ecosystem.pdf")
gg
dev.off()
dev.off()
