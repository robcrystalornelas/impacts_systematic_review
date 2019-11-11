## READ IN DATA ####
source("~/Desktop/ch2_impacts_systematic_review/scripts/impacts_systematic_review/clean_raw_data.R")
## LOAD PACKAGES ####

library(dplyr)
library(ggplot2)
library(ggthemes)
library(cowplot)

## ORGANIZE DATA ####
impact_and_taxa <- dplyr::select(raw_data, impacttype, invasivespeciestaxa, impact_category_lockwood)
head(impact_and_taxa)
dim(impact_and_taxa)
counted_impacts <- as.data.frame(dplyr::count(raw_data, impacttype)) 
counted_impacts

counted_biological_scale <- as.data.frame(dplyr::count(raw_data, impact_category_lockwood)) 
counted_biological_scale
## MAKE FIGURE for ecological effect ####
gg <-
  ggplot(impact_and_taxa, aes(x = reorder(impacttype, impacttype, function(x)
    -
      length(x))))
gg <- gg + geom_bar(stat = "count", fill = "#1f78b4") +
  #coord_cartesian(ylim=c(0,650), expand = FALSE) 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 620)) 

gg
gg <- gg + theme_cowplot()
gg <- gg + ylab("Frequency")
gg <- gg + xlab("")
gg <-
  gg + theme(
    axis.text = element_text(size = 23),
    axis.text.x = element_text(
      angle = 90,
      hjust = 1,
      vjust = 0.5,
      size = 23
    ),
    strip.text = element_text(size = 23),
    axis.title = element_text(size=23)
  ) # Change axis title size
gg

## Make figure for biological scale ###
gg_bioscale <-
  ggplot(impact_and_taxa, aes(x = reorder(impact_category_lockwood, impact_category_lockwood, function(x)
    -
      length(x))))
gg_bioscale <- gg_bioscale + geom_bar(stat = "count", fill = "firebrick3") +
  coord_cartesian(clip = "off") + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1100)) 

gg_bioscale

gg_bioscale <- gg_bioscale + theme_cowplot()
gg_bioscale <- gg_bioscale + ylab("Frequency")
gg_bioscale <- gg_bioscale + xlab("")
gg_bioscale <-
  gg_bioscale + theme(
    axis.text = element_text(size = 23),
    axis.text.x = element_text(
      angle = 90,
      hjust = 1,
      vjust = 0.5,
      size = 23
    ),
    strip.text = element_text(size = 23),
    axis.title = element_text(size=23)
  ) # Change axis title size
gg_bioscale

## Combine two figures
plot_grid(gg, gg_bioscale, labels = c("A","B"), align = "h")

dev.off()
dev.off()