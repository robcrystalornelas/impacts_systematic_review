## READ IN DATA ####
source("~/Desktop/Impacts Systematic Review/scripts/impacts_systematic_review/clean_raw_data.R")
# all of the correct variables in our environment

## LOAD LIBRARIES ####
library(dplyr)
library(ggplot2)
library(ggthemes)
library(viridis)

## CLEAN DATA ####
head(raw_data)
species_and_year <- dplyr::select(raw_data, publicationyear, invasivespecies)
head(species_and_year)

selected <- c("Aporrectodea longa","cane toad","crested wheatgrass", "red swamp crayfish") # these are focal species
counted_species_and_year <- species_and_year[species_and_year$invasivespecies %in% selected,] %>%
  plyr::count()
counted_species_and_year

# Trying to make every combo of species and year represented
vals <- expand.grid(invasivespecies = unique(counted_species_and_year$invasivespecies),
                    publicationyear = unique(counted_species_and_year$publicationyear))
counted_species_and_year_final <- merge(vals,counted_species_and_year,all = TRUE)
counted_species_and_year_final[is.na(counted_species_and_year_final)] <- 0
# 
# # Make the figure
# gg <- ggplot(counted_species_and_year_final, aes(x=publicationyear, y=freq, group = invasivespecies, colour = invasivespecies)) +
#       geom_line(size = 2) +
#   scale_colour_brewer(type = "div")
#   gg
# 
# pdf(file="~/Desktop/Impacts Systematic Review/figures/taxonomy_time_series.pdf")
# gg
# dev.off()
# dev.off()
# 
# # Try it out as dodged barplot
# head(counted_species_and_year_final)
# gg <- ggplot(counted_species_and_year_final,aes(x = publicationyear, y = freq)) + 
#   geom_bar(aes(fill = invasivespecies),stat = "identity", position = "dodge") +
#   theme_tufte()
# gg
# pdf(file="~/Desktop/Impacts Systematic Review/figures/taxonomy_dodged_barplot.pdf")
# gg
# dev.off()
# dev.off()
# 

# 5 year increments
## CLEAN DATA ####
head(raw_data)
species_and_year <- dplyr::select(raw_data, invasivespecies,yearbinned)
head(species_and_year)

selected <- c("cane toad","crested wheatgrass", "red swamp crayfish","zebra mussel") # these are focal species
counted_species_and_year <- species_and_year[species_and_year$invasivespecies %in% selected,] %>%
  plyr::count()
counted_species_and_year

# Trying to make every combo of species and year represented
vals <- expand.grid(invasivespecies = unique(counted_species_and_year$invasivespecies),
                    yearbinned = unique(counted_species_and_year$yearbinned))
counted_species_and_year_final <- merge(vals,counted_species_and_year,all = TRUE)
counted_species_and_year_final[is.na(counted_species_and_year_final)] <- 0

# Make the figure
gg <- ggplot(counted_species_and_year_final, aes(x=yearbinned, y=freq, group = invasivespecies, colour = invasivespecies)) +
  geom_line(size = 2.5) + scale_colour_brewer(type = "div")
gg <- gg + theme_tufte()
gg <- gg + theme(axis.text.x = element_text(size=15, hjust = 1, vjust = .5),
                   axis.text.y = element_text(size=15),
                   axis.title = element_text(size=20))
gg <- gg + xlab("Years") + ylab("Frequency")
gg <- gg + theme(legend.title=element_blank()) # remove legend title
gg <- gg + theme(legend.text = element_text(size = 16)) # make species names bigger
gg

pdf(file="~/Desktop/Impacts Systematic Review/figures/figure_taxonomy_binned_time_series.pdf")

gg
dev.off()
dev.off()

# # Try it out as dodged barplot 5 YEAR INCREMENTS
# head(counted_species_and_year_final)
# gg <- ggplot(counted_species_and_year_final,aes(x = yearbinned, y = freq)) + 
#   geom_bar(aes(fill = invasivespecies),stat = "identity", position = "dodge") +
#   theme_tufte()
# gg
# pdf(file="~/Desktop/Impacts Systematic Review/figures/taxonomy_dodged_barplot_yearbinned.pdf")
# gg
# dev.off()
# dev.off()
