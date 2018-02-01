## READ IN DATA ####
source("~/Desktop/Impacts Systematic Review/scripts/impacts_systematic_review/clean_raw_data.R")

## LOAD PACKAGES ####
library(alluvial)
library(dplyr)
library(ggplot2)
library(ggthemes)

## ORGANIZE DATA ####
model_species_and_impact <- dplyr::select(raw_data,invasivespecies,impacttype)

# Count of top 10 species
species_counted <- as.data.frame(count(raw_data, invasivespecies))
species_ordered <- arrange(species_counted, desc(n))
top_ten_species <- slice(species_ordered, 1:10)
top_ten_species <-as.data.frame(top_ten_species)
# create vector of names
list_of_top_ten <- as.character(top_ten_species$invasivespecies)

# Select only rows with our top 10 invasive species
subset_of_model_species_and_impacts <- subset(model_species_and_impact, invasivespecies %in% c(list_of_top_ten))


# There will be no counts for production, other and hybridization, so remove those
class(subset_of_model_species_and_impacts$impacttype)

# Calculate counts for invasive species
model_species_and_impact_count <- count(subset_of_model_species_and_impacts, invasivespecies, impacttype) # count function has to come BEFORE complete
model_species_and_impact_count <- as.data.frame(model_species_and_impact_count)
head(model_species_and_impact_count)
model_species_and_impact_count
model_species_and_impact_count %>% group_by(impacttype) %>% mutate(color =colorRampPalette(solarized_pal()(8))(10) )
model_species_and_impact_count$color[model_species_and_impact_count$impacttype == "abundance"] <- "#268BD2"
model_species_and_impact_count$color[model_species_and_impact_count$impacttype == "behavior"] <- "#688A70"
model_species_and_impact_count$color[model_species_and_impact_count$impacttype == "diversity"] <- "#AB890E"
model_species_and_impact_count$color[model_species_and_impact_count$impacttype == "fitness"] <- "#BD7008"
model_species_and_impact_count$color[model_species_and_impact_count$impacttype == "growth"] <- "#C85313"
model_species_and_impact_count$color[model_species_and_impact_count$impacttype == "habitat change"] <- "#D0421E"
model_species_and_impact_count$color[model_species_and_impact_count$impacttype == "indirect"] <- "#D8362A"
model_species_and_impact_count$color[model_species_and_impact_count$impacttype == "nutrient availability"] <- "#D93345"
model_species_and_impact_count$color[model_species_and_impact_count$impacttype == "other"] <- "#D5346B"
model_species_and_impact_count$color[model_species_and_impact_count$impacttype == "production"] <- "#BE418F"

## MAKE FIGURE ####
alluvial(
  dplyr::select(model_species_and_impact_count, impacttype,invasivespecies),
  freq = model_species_and_impact_count$n,
  col = model_species_and_impact_count$color,
  #border = model_species_and_impact_count$color,
  hide = model_species_and_impact_count$n < 3,
  alpha = 0.7, # transparency of each aulluvia
  cex = 0.6 # Font size
)

# Export file
pdf(file="~/Desktop/Impacts Systematic Review/figures/sankey_diagram_model_species_and_impact.pdf")
alluvial(
  dplyr::select(model_species_and_impact_count, impacttype,invasivespecies),
  freq = model_species_and_impact_count$n,
  col = model_species_and_impact_count$color,
  #border = model_species_and_impact_count$color,
  hide = model_species_and_impact_count$n < 3,
  alpha = 0.7, # transparency of each aulluvia
  cex = 0.6 # Font size
)
dev.off()
dev.off()
