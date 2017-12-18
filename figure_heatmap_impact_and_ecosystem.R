## READ IN DATA ####
source("~/Desktop/Impacts Systematic Review/scripts/impacts_systematic_review/clean_data_for_heatmap.R") # This tells R to run our entire cleaning script so that we have

## LOAD PACKAGES ####
library(ggplot2)
library(dplyr)
library(data.table)  # faster fread() and better weekdays()
library(dplyr)       # consistent data.frame operations
library(purrr)       # consistent & safe list/vector munging
library(tidyr)       # consistent data.frame cleaning
library(lubridate)   # date manipulation
library(countrycode) # turn country codes into pretty names
library(ggplot2)     # base plots are for Coursera professors
library(scales)      # pairs nicely with ggplot2 for plot label formatting
library(gridExtra)   # a helper for arranging individual ggplot objects
library(ggthemes)    # has a clean theme for ggplot2
library(viridis)     # best. color. palette. evar.
library(knitr)   

## MAKE FIGURE ####

# The full version
gg <- ggplot(top_impacts_and_ecosystem_complete, aes(x=impacttype, y=ecosystem, fill=n))
gg <- gg + geom_tile(color="white", size=0.1) # This tells we want every block to have a thin black border
gg <- gg + scale_fill_viridis(option = "C", name="# of cases", label=comma) # This provides a color-blind friendly palette.  I chose option C for the Vidiris palettes
gg <- gg + coord_equal()
gg <- gg + labs(x = NULL, y = NULL, title="Impact Type & Ecosystem") # This is the title of the plot
gg <- gg + theme_tufte(base_family="Helvetica")
# gg <- gg + theme(plot.title=element_text(hjust=0))
gg <- gg + theme(plot.title = element_text(size=18)) # size of plot title
# gg <- gg + theme(axis.ticks=element_blank())
gg <- gg + theme(axis.text=element_text(size=18))
gg <- gg + theme(legend.title=element_text(size=15))
gg <- gg + theme(legend.text=element_text(size=13))
gg <- gg + theme(axis.text.x=element_text(angle=45,hjust=1))
gg

# Plot the figure
pdf(file="~/Desktop/Impacts Systematic Review/figures/impact_and_ecosystem_subset.pdf")
plot(gg)
dev.off()
dev.off()
gg # plot it in the R studio window



