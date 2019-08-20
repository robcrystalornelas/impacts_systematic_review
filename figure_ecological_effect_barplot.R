## READ IN DATA ####
source("~/Desktop/ch2_impacts_systematic_review/scripts/impacts_systematic_review/clean_raw_data.R")

## LOAD PACKAGES ####
library(plyr)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(cowplot)

## ORGANIZE DATA ####
impact_and_taxa <- dplyr::select(raw_data, impacttype, invasivespeciestaxa)
head(impact_and_taxa)
dim(impact_and_taxa)
counted_impacts <- as.data.frame(dplyr::count(raw_data, impacttype)) 
counted_impacts

## MAKE FIGURES ####
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

dev.off()
dev.off()