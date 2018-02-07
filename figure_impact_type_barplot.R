## READ IN DATA ####
source("~/Desktop/Impacts Systematic Review/scripts/impacts_systematic_review/clean_raw_data.R")

## LOAD PACKAGES ####
library(dplyr)
library(ggplot2)
library(ggthemes)

## ORGANIZE DATA ####
impact_and_taxa <- dplyr::select(raw_data, impacttype, invasivespeciestaxa)

## MAKE FIGURES ####
gg <- ggplot(impact_and_taxa, aes(x = reorder(impacttype,impacttype, function(x)-length(x))))
gg <-gg + geom_bar(stat="count", fill = "#1f78b4")
gg
gg <- gg + theme_tufte()
gg <- gg + ylab("Frequency")
gg <- gg + xlab("Ecological Effect")
gg <- gg + theme(axis.text=element_text(size=12), # Change tick mark label size
                 axis.title=element_text(size=14,face="bold"),
                 axis.text.x = element_text(angle = 45, hjust = 1)) # Change axis title size
gg

pdf(file="~/Desktop/Impacts Systematic Review/figures/overall_impact_type_barplot.pdf")
gg
dev.off()
dev.off()
# gg <-gg + theme(axis.text.x = element_text(angle = 60, hjust = 1))
# gg
