## READ IN DATA ####
source("~/Desktop/Impacts Systematic Review/scripts/impacts_systematic_review/clean_raw_data.R")

## LOAD PACKAGES ####
library(ggplot2)
library(dplyr)
library(ggthemes)

## ORGANIZE DATA ####
head(raw_data)

unique(raw_data$yearends)

# This is all code for CASE STUDIES
# year_and_year_end <- select(raw_data, code, publicationyear, yearends) %>%
#   group_by(publicationyear) %>%
#   na.omit() %>%
#   mutate(lag = publicationyear - yearends) %>%
#   group_by(publicationyear) %>%
#   summarize(avg_lag = mean(lag))
# year_and_year_end

# Use this code to determine how many articles included
year_and_year_end_publications <- select(raw_data, code, publicationyear, yearends) %>%
  distinct() %>%
  group_by(publicationyear) %>%
  na.omit() %>%
  mutate(lag = publicationyear - yearends)
dim(year_and_year_end_publications)
  
# This is all code for unique PUBLICATIONS
year_and_year_end_publications <- select(raw_data, code, publicationyear, yearends) %>%
  distinct() %>%
  group_by(publicationyear) %>%
  na.omit() %>%
  mutate(lag = publicationyear - yearends) %>%
  group_by(publicationyear) %>%
  summarize(avg_lag = mean(lag))

  ## MAKE FIGURES ####
gg <- ggplot(year_and_year_end_publications, aes(x=publicationyear, y = avg_lag))
gg <- gg + geom_bar(stat="identity", fill = "#bebada")
gg
gg <- gg + theme_tufte()
gg <- gg + coord_flip()
gg <- gg + scale_x_reverse()
gg
gg <- gg + ylab("# of Years")
gg <- gg + xlab("Publication Year")
gg <- gg + theme(axis.text=element_text(size=12), # Change tick mark label size
                 axis.title=element_text(size=14,face="bold"))
gg

pdf(file="~/Desktop/Impacts Systematic Review/figures/publication_lag_barplot.pdf")
gg
dev.off()
dev.off()
 Howe
