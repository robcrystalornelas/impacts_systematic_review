## READ IN DATA ####
source("~/Desktop/Impacts Systematic Review/scripts/impacts_systematic_review/clean_raw_data.R")

## LOAD PACKAGES ####
library(alluvial)
library(dplyr)
library(ggplot2)
library(ggthemes)

## ORGANIZE DATA ####
impact_taxa_binned_year <- select(raw_data, impacttype, invasivespeciestaxa,yearbinned)
head(impact_taxa_binned_year)

impact_taxa_binned_year_count <- count(impact_taxa_binned_year, impacttype,invasivespeciestaxa,yearbinned) # count function has to come BEFORE complete
impact_taxa_binned_year_count <- as.data.frame(impact_taxa_binned_year_count)
head(impact_taxa_binned_year_count)

# Assign color to each impact type
impact_taxa_binned_year_count
impact_taxa_binned_year_count %>% group_by(impacttype) %>% mutate(color =colorRampPalette(solarized_pal()(8))(16) )
impact_taxa_binned_year_count$color[impact_taxa_binned_year_count$impacttype == "abundance"] <- "#268BD2"
impact_taxa_binned_year_count$color[impact_taxa_binned_year_count$impacttype == "behavior"] <- "#688A70"
impact_taxa_binned_year_count$color[impact_taxa_binned_year_count$impacttype == "diversity"] <- "#AB890E"
impact_taxa_binned_year_count$color[impact_taxa_binned_year_count$impacttype == "fitness"] <- "#BD7008"
impact_taxa_binned_year_count$color[impact_taxa_binned_year_count$impacttype == "growth"] <- "#C85313"
impact_taxa_binned_year_count$color[impact_taxa_binned_year_count$impacttype == "habitat change"] <- "#D0421E"
impact_taxa_binned_year_count$color[impact_taxa_binned_year_count$impacttype == "indirect"] <- "#D8362A"
impact_taxa_binned_year_count$color[impact_taxa_binned_year_count$impacttype == "nutrient availability"] <- "#D93345"
impact_taxa_binned_year_count$color[impact_taxa_binned_year_count$impacttype == "other"] <- "#D5346B"
impact_taxa_binned_year_count$color[impact_taxa_binned_year_count$impacttype == "production"] <- "#BE418F"

head(impact_taxa_binned_year_count)
## MAKE FIGURES ####

sankey <- alluvial(
  select(impact_taxa_binned_year_count, yearbinned, impacttype, invasivespeciestaxa),
  freq = impact_taxa_binned_year_count$n,
  col = impact_taxa_binned_year_count$color,
  border = impact_taxa_binned_year_count$color,
  #col = ifelse(impact_taxa_binned_year_count$impacttype == "diversity", "orange","grey"),
  #border = ifelse(impact_taxa_binned_year_count$impacttype == "diversity", "orange", "grey"),
  hide = impact_taxa_binned_year_count$n == 0,
  alpha = 0.7,
  cex = 0.7
)
sankey

pdf(file="~/Desktop/Impacts Systematic Review/figures/sankey_diagram.pdf")
alluvial(
  select(impact_taxa_binned_year_count, yearbinned, impacttype, invasivespeciestaxa),
  freq = impact_taxa_binned_year_count$n,
  col = impact_taxa_binned_year_count$color,
  border = impact_taxa_binned_year_count$color,
  #col = ifelse(impact_taxa_binned_year_count$impacttype == "diversity", "orange","grey"),
  #border = ifelse(impact_taxa_binned_year_count$impacttype == "diversity", "orange", "grey"),
  hide = impact_taxa_binned_year_count$n == 0,
  alpha = 0.7,
  cex = 0.7
)
dev.off()
dev.off()

colorRampPalette(solarized_pal()(8))(16)
