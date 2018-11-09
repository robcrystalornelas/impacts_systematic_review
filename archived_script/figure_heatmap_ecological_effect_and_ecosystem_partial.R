## READ IN DATA ####
source("~/Desktop/Impacts Systematic Review/scripts/impacts_systematic_review/clean_data_for_heatmap.R") # This tells R to run our entire cleaning script so that we have
# all of the correct variables in our environment

## LOAD PACKAGES ####
library(ggplot2)
library(dplyr)
library(dplyr)       # consistent data.frame operations
library(purrr)       # consistent & safe list/vector munging
library(tidyr)       # consistent data.frame cleaning
library(scales)      # pairs nicely with ggplot2 for plot label formatting
library(gridExtra)   # a helper for arranging individual ggplot objects
library(ggthemes)    # has a clean theme for ggplot2
library(viridis)     # best. color. palette. evar.
library(knitr)   

## ORGANIZE DATA ####

# Make sure that year-binned is now part of the count
top_impacts_and_ecosystem_and_yearbinned_count <- dplyr::count(top_impacts_and_ecosystem_t, impacttype, ecosystem, yearbinned) # count function has to come BEFORE complete
head(top_impacts_and_ecosystem_and_yearbinned_count)

top_impacts_and_ecosystem_and_yearbinned_complete <- tidyr::complete(top_impacts_and_ecosystem_and_yearbinned_count, impacttype, ecosystem, yearbinned,fill=list(n=NA))
top_impacts_and_ecosystem_and_yearbinned_complete

## MAKE PLOT non-facet for select categories####
gg <- ggplot(top_impacts_and_ecosystem_and_yearbinned_complete, aes(x=impacttype, y=ecosystem, fill=n))
gg <- gg + geom_tile(color="white", size=0.1) # This tells we want every block to have a thin black border
gg <- gg + scale_fill_viridis(option = "C", name="# of case studies", label=comma) # This provides a color-blind friendly palette.  I chose option C for the Vidiris palettes
gg <- gg + coord_equal()
gg <- gg + theme_tufte(base_family="Helvetica")
gg <- gg + ylab("Ecosystem")
gg <- gg + xlab("Ecological Effect")
gg <- gg + theme(axis.ticks=element_blank(),
                 axis.text.x = element_text(size=15, angle = 90, hjust = 1),
                 axis.text.y = element_text(size=15),
                 axis.title = element_text(size=20))

gg

pdf(file="~/Desktop/Impacts Systematic Review/figures/figure_heatmap_ecological_effect_and_ecosystem.pdf")
gg
dev.off()
dev.off()

## MAKE PLOT non-facet for all categories####
all_impacts_and_ecosystems <- dplyr::count(impacts_t, impacttype, ecosystem)
gg <- ggplot(all_impacts_and_ecosystems, aes(x=impacttype, y=ecosystem, fill = n))
gg <- gg + geom_tile(color="white", size=0.1) # This tells we want every block to have a thin black border
gg <- gg + scale_fill_viridis(option = "C", name="# of case studies", label=comma) # This provides a color-blind friendly palette.  I chose option C for the Vidiris palettes
gg <- gg + coord_equal()
gg <- gg + labs(x = NULL, y = NULL, title="Impact Type & Ecosystem") # This is the title of the plot
gg <- gg + theme_tufte(base_family="Helvetica")
gg <- gg + theme(axis.ticks=element_blank())
gg <- gg + theme(axis.text.x=element_text(size=12, angle = 45, hjust = 1))
gg <- gg + theme(axis.text.y=element_text(size=12))
gg

## MAKE PLOT fascet####
gg <- ggplot(top_impacts_and_ecosystem_and_yearbinned_complete, aes(x=impacttype, y=ecosystem, fill=n))
gg <- gg + geom_tile(color="white", size=0.1) # This tells we want every block to have a thin black border
gg <- gg + scale_fill_viridis(option = "C", name="# of case studies", label=comma) # This provides a color-blind friendly palette.  I chose option C for the Vidiris palettes
gg <- gg + coord_equal()
gg <- gg + facet_wrap(~yearbinned, ncol=2)
gg <- gg + labs(x = NULL, y = NULL, title="Impact Type & Ecosystem") # This is the title of the plot
gg <- gg + theme_tufte(base_family="Helvetica")
gg <- gg + theme(axis.ticks=element_blank())
gg <- gg + theme(axis.text.x=element_text(size=12, angle = 45, hjust = 1))
gg <- gg + theme(axis.text.y=element_text(size=12))
gg

gg <- gg + theme(panel.border=element_blank())
gg <- gg + theme(plot.title=element_text(hjust=0))
gg <- gg + theme(strip.text=element_text(hjust=0))
gg

gg <- gg + theme(panel.spacing.x = unit(0.5, "cm"))
gg <- gg + theme(panel.spacing.y = unit(0.5, "cm"))
gg <- gg + theme(legend.title=element_text(size=12))
gg <- gg + theme(legend.title.align=1)
gg <- gg + theme(legend.text=element_text(size=12))
gg <- gg + theme(legend.position="bottom")
gg
gg <- gg + theme(legend.key.size=unit(0.2, "cm"))
gg <- gg + theme(legend.key.width=unit(1, "cm"))
gg
