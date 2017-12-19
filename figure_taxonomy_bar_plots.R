## READ IN DATA ####
source("~/Desktop/Impacts Systematic Review/scripts/impacts_systematic_review/clean_raw_data.R")

## LOAD PACKAGES ####
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(ggpubr)
## ORGANIZE DATA ####

# Count of top 10 species
species_counted <- as.data.frame(count(raw_data, invasivespecies))
species_ordered <- arrange(species_counted, desc(n))
head(species_ordered)

top_ten_species <- slice(species_ordered, 1:10)
top_ten_species <-as.data.frame(top_ten_species)
top_ten_species <- rename(top_ten_species, count = n)
top_ten_species$invasivespecies <- factor(top_ten_species$invasivespecies, levels = top_ten_species$invasivespecies[order(-top_ten_species$count)])

# Invasive species taxa for all years
gg<- ggplot(data=top_ten_species, aes(x=invasivespecies, y=count))
gg <- gg + geom_bar(stat="identity", fill = "#FF6666")
gg
gg <- gg + theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 12))
gg

pdf(file="~/Desktop/Impacts Systematic Review/figures/taxonomy_top_ten_species.pdf")
plot(gg)
dev.off()
dev.off()

# Now created same figure, but fasceted in 5 year increments

## ORGANIZE DATA ####
species_counted_fascet <- as.data.frame(count(raw_data, invasivespecies,yearbinned))
head(species_counted_fascet)

# 1999-2004
species_counted_first_six <- filter(species_counted_fascet, yearbinned == "1999-2004")
species_counted_first_six <- arrange(species_counted_first_six, desc(n))
head(species_counted_first_six)
species_counted_first_six_top_ten <- slice(species_counted_first_six, 1:10)
species_counted_first_six_top_ten <-as.data.frame(species_counted_first_six_top_ten)
species_counted_first_six_top_ten <- rename(species_counted_first_six_top_ten, count = n)
species_counted_first_six_top_ten$invasivespecies <- factor(species_counted_first_six_top_ten$invasivespecies, levels = species_counted_first_six_top_ten$invasivespecies[order(-species_counted_first_six_top_ten$count)])
head(species_counted_first_six_top_ten)

#2005-2010
species_counted_second_six <-filter(species_counted_fascet, yearbinned == "2005-2010")
species_counted_second_six <- arrange(species_counted_second_six, desc(n))
head(species_counted_second_six)
species_counted_second_six_top_ten <- slice(species_counted_second_six, 1:10)
species_counted_second_six_top_ten <-as.data.frame(species_counted_second_six_top_ten)
species_counted_second_six_top_ten <- rename(species_counted_second_six_top_ten, count = n)
species_counted_second_six_top_ten$invasivespecies <- factor(species_counted_second_six_top_ten$invasivespecies, levels = species_counted_second_six_top_ten$invasivespecies[order(-species_counted_second_six_top_ten$count)])
head(species_counted_second_six_top_ten)

#2011-2016
species_counted_third_six <- filter(species_counted_fascet, yearbinned == "2011-2016")
species_counted_third_six <- arrange(species_counted_third_six, desc(n))
head(species_counted_third_six)
species_counted_third_six_top_ten <- slice(species_counted_third_six, 1:10)
species_counted_third_six_top_ten <-as.data.frame(species_counted_third_six_top_ten)
species_counted_third_six_top_ten <- rename(species_counted_third_six_top_ten, count = n)
species_counted_third_six_top_ten$invasivespecies <- factor(species_counted_third_six_top_ten$invasivespecies, levels = species_counted_third_six_top_ten$invasivespecies[order(-species_counted_third_six_top_ten$count)])
head(species_counted_third_six_top_ten)

## MAKE THREE FIGURES ####
gg_first <- ggplot(data=species_counted_first_six_top_ten, aes(x=invasivespecies, y=count))
gg_first <- gg_first + geom_bar(stat="identity", fill = "#fc8d62")
gg_first
gg_first <- gg_first + theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 12))
gg_first <- gg_first + ggtitle("1999-2004")
gg_first <- gg_first + ylim(0,30)
gg_first

gg_second <- ggplot(data=species_counted_second_six_top_ten, aes(x=invasivespecies, y=count))
gg_second <- gg_second + geom_bar(stat="identity", fill = "#66c2a5")
gg_second
gg_second <- gg_second + theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 12))
gg_second <- gg_second + ggtitle("2005-2010")
gg_second <- gg_second + ylim(0,30)
gg_second

gg_third <- ggplot(data=species_counted_third_six_top_ten, aes(x=invasivespecies, y=count))
gg_third <- gg_third + geom_bar(stat="identity", fill = "#8da0cb")
gg_third
gg_third <- gg_third + theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 12))
gg_third <- gg_third + ggtitle("2011-2016")
gg_third <- gg_third + ylim(0,30)
gg_third

# Arrange on one page
ggarrange(gg_first,gg_second,gg_third)
pdf(file="~/Desktop/Impacts Systematic Review/figures/taxonomy_top_ten_species_years_binned.pdf")
ggarrange(gg_first,gg_second,gg_third)
dev.off()
dev.off()
