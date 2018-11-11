## READ IN DATA ####
source("~/Desktop/Impacts Systematic Review/scripts/impacts_systematic_review/clean_data_for_heatmap.R") # This tells R to run our entire cleaning script so that we have
# all of the correct variables in our environment

## LOAD PACKAGES ####
library(ggplot2)
library(dplyr)
library(dplyr)       # consistent data.frame operations
library(tidyr)       # consistent data.frame cleaning
library(scales)      # pairs nicely with ggplot2 for plot label formatting
library(gridExtra)   # a helper for arranging individual ggplot objects
library(ggthemes)    # has a clean theme for ggplot2
library(viridis)     # best. color. palette. evar.
library(knitr)   

## Make count of all ecosyste and impact combinations
all_impacts_and_ecosystems <- dplyr::count(impacts_t, impacttype, ecosystem)

# Remove impacts with impact listed as "NA"
all_impacts_and_ecosystems_complete <-
  all_impacts_and_ecosystems %>%
  filter(ecosystem != "NA") %>%
  droplevels() %>%
  tidyr::complete(impacttype, ecosystem, fill=list(n=NA)) # complete heatmap so that all combinations have either numerical value or NA

# Create heatmap
gg <- ggplot(all_impacts_and_ecosystems_complete, aes(x=impacttype, y=ecosystem, fill = n))
gg <- gg + geom_tile(color="white", size=0.1) # This tells we want every block to have a thin black border
gg <- gg + scale_fill_viridis(option = "C", name="# of case studies", label=comma) # This provides a color-blind friendly palette.  I chose option C for the Vidiris palettes
gg <- gg + coord_equal()
gg <- gg + theme_tufte(base_family="Helvetica")
gg <- gg + theme(axis.ticks=element_blank())
gg <- gg + theme(axis.text.x=element_text(size=18, angle = 90, hjust = 1))
gg <- gg + theme(axis.text.y=element_text(size=18))
gg <- gg + theme(axis.title = element_text(size=20))
gg <- gg + ylab("Ecosystem")
gg <- gg + xlab("Ecological Effect")
gg

pdf(file="~/Desktop/Impacts Systematic Review/figures/figure_heatmap_ecological_effect_and_ecosystem_full.pdf")
gg
dev.off()
dev.off()


