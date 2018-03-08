## READ IN DATA ####
source("~/Desktop/Impacts Systematic Review/scripts/impacts_systematic_review/clean_raw_data.R")

## LOAD PACKAGES ####
library(plyr)
library(dplyr)
library(ggplot2)
library(ggthemes)

## ORGANIZE DATA ####
impact_and_taxa <- dplyr::select(raw_data, impacttype, invasivespeciestaxa)
head(impact_and_taxa)
counted_impacts <- as.data.frame(dplyr::count(raw_data, impacttype)) 
counted_impacts

## MAKE FIGURES ####
gg <- ggplot(impact_and_taxa, aes(x = reorder(impacttype,impacttype, function(x)-length(x))))
gg <-gg + geom_bar(stat="count", fill = "#1f78b4")
gg
gg <- gg + theme_tufte()
gg <- gg + ylab("Frequency")
gg <- gg + xlab("Ecological Effect")
gg <- gg + theme(axis.text.x = element_text(size=15, angle = 90, hjust = 1, vjust = .5),
                 axis.text.y = element_text(size=15),
                 axis.title = element_text(size=20))
gg

pdf(file="~/Desktop/Impacts Systematic Review/figures/ecological_effect_barplot.pdf")
gg
dev.off()
dev.off()
