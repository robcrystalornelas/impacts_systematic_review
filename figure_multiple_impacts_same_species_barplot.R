## READ IN DATA ####
source("~/Desktop/Impacts Systematic Review/scripts/impacts_systematic_review/clean_raw_data.R")

## LOAD PACKAGES ####
library(dplyr)
library(ggplot2)

## ORGANIZE DATA ####
head(raw_data)
impacts_model_species_and_bio <- select(raw_data, invasivespecies, impacttype, impact_category_lockwood)
head(impacts_model_species_and_bio)

# Count of top 10 species
species_counted <- as.data.frame(count(raw_data, invasivespecies))
species_ordered <- arrange(species_counted, desc(n))
top_ten_species <- slice(species_ordered, 1:10)
top_ten_species <-as.data.frame(top_ten_species)
list_of_top_ten <- as.character(top_ten_species$invasivespecies)

# Select only rows with our top 10 invasive species
subset_of_top_ten <- subset(impacts_model_species_and_bio, invasivespecies %in% c(list_of_top_ten))
subset_of_top_ten

# Reorder biological levels so that ecosystem is on top
subset_of_top_ten$impact_category_lockwood <- factor(subset_of_top_ten$impact_category_lockwood, levels = c("ecosystem","community","population","individual","genetic"))
subset_of_top_ten

## MAKE FIGURES ####
# Make stacked barplot
gg <- ggplot(data=subset_of_top_ten, aes(x=invasivespecies, fill = factor(impact_category_lockwood)))
gg <- gg + geom_bar(stat="count")
gg
gg <- gg + scale_fill_brewer(palette="Set2")
gg <- gg + theme_tufte()
gg
gg <- gg + ylab("Frequency")
gg <- gg + xlab("Invasive Species")
gg <- gg + ggtitle("Biological levels studied for top 10 invasive species")
gg <- gg + theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 12))
gg <- gg + theme(legend.title=element_blank()) # Remove legen title
gg


pdf(file="~/Desktop/Impacts Systematic Review/figures/taxonomy_top_ten_and_bio_level_stacked_barplot.pdf")
gg
dev.off()
dev.off()

# Fasceted by impact type
