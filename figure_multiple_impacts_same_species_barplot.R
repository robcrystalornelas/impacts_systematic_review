## READ IN DATA ####
source("~/Desktop/ch2_impacts_systematic_review/scripts/impacts_systematic_review/clean_raw_data.R")

## LOAD PACKAGES ####
library(dplyr)
library(ggplot2)
library(ggthemes)
library(viridis)
library(cowplot)

## ORGANIZE DATA ####
head(raw_data)
impacts_model_species_and_bio <- dplyr::select(raw_data, invasivespecies, impacttype, impact_category_lockwood)
head(impacts_model_species_and_bio)

# Count of top 10 species
species_counted <- as.data.frame(dplyr::count(raw_data, invasivespecies))
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
gg <- ggplot(data=subset_of_top_ten, aes(x=reorder(invasivespecies,invasivespecies, function(x)-length(x)), fill = factor(impact_category_lockwood)))
gg <- gg + geom_bar(stat="count")
gg
gg <- gg + scale_fill_viridis(discrete = TRUE, option = "magma")
gg <- gg + theme_cowplot() +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 55))
gg
gg <- gg + ylab("Frequency")
gg <- gg + xlab("Invasive Species")
gg <- gg + theme() # Remove legend title
gg <- gg + theme(axis.text.x = element_text(size=15, angle = 90, hjust = 1, vjust = .5),
                 axis.text.y = element_text(size=15),
                 axis.title = element_text(size=20),
                 legend.text = element_text(size = 16), # make entries in the legend larger
                 legend.title=element_blank()) # Remove legend title
gg

pdf(file="~/Desktop/ch2_impacts_systematic_review/figures/figure_taxonomy_top_ten_and_bio_level_stacked_barplot.pdf")
gg
dev.off()
dev.off()

## Figure for all invasive speciec ####
# Count of top 10 species
head(raw_data)
species_counted <- as.data.frame(dplyr::count(raw_data, invasivespecies))
species_ordered <- arrange(species_counted, desc(n))
head(species_ordered)
species_ordered
species_ordered <- dplyr::rename(species_ordered, count = n)
species_ordered$invasivespecies <- factor(species_ordered$invasivespecies, levels = species_ordered$invasivespecies[order(-species_ordered$count)])
dim(species_counted)
# Replace all invasive species names with a unique numerical identified
# levels(species_ordered$invasivespecies) <- 1:575
# head(species_ordered)

# If we want to look at only TOP TEN
top_ten_species <- slice(species_ordered, 1:10)
top_ten_species <-as.data.frame(top_ten_species)
top_ten_species
head(top_ten_species)
top_ten_species$invasivespecies <- factor(top_ten_species$invasivespecies, levels = top_ten_species$invasivespecies[order(-top_ten_species$count)])
species_ordered
# Figure for ALL invasive species
gg_all_species <- ggplot(data = species_ordered, aes(x=invasivespecies, y = count))
gg_all_species <- gg_all_species + geom_bar(stat="identity", fill = "#239B56")
gg_all_species <- gg_all_species + theme_cowplot() +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 55))
gg_all_species <- gg_all_species + ylab("Frequency")
gg_all_species <- gg_all_species + xlab("Invasive Species")
gg_all_species <- gg_all_species + theme(axis.text=element_text(size=15), # Change tick mark label size
                 axis.title=element_text(size=20),
                 axis.text.x=element_blank(),
                 axis.ticks = element_blank(),
                 strip.text = element_text(size=12)) # Change axis title size
gg_all_species

plot_grid(gg_all_species,gg, ncol = 2, labels = c('A', 'B'))
