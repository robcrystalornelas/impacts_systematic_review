## READ IN DATA ####
source("~/Desktop/Impacts Systematic Review/scripts/impacts_systematic_review/clean_raw_data.R")

## LOAD PACKAGES ####
library(ggplot2)
library(dplyr)
library(ggthemes)

## ORGANIZE DATA ####
head(raw_data)

unique(raw_data$yearends)

year_and_year_end <- select(raw_data, code, publicationyear, yearends) %>%
  group_by(publicationyear) %>%
  na.omit() %>%
  mutate(lag = publicationyear - yearends) %>%
  group_by(publicationyear) %>%
  summarize(avg_lag = mean(lag))
year_and_year_end

## MAKE FIGURES ####
gg <- ggplot(year_and_year_end, aes(x=publicationyear, y = avg_lag))
gg <- gg + geom_bar(stat="identity", fill = "#bebada")
gg
gg <- gg + theme_tufte()
gg <- gg + coord_flip()
gg <- gg + scale_x_reverse()
gg
gg <- gg + ylab("# of Years")
gg <- gg + xlab("Publication Year")
gg <- gg + ggtitle("Time lag between research end and publication")
gg

pdf(file="~/Desktop/Impacts Systematic Review/figures/publicationlag_barplot.pdf")
gg
dev.off()
dev.off()

