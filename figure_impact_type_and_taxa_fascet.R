## READ IN DATA ####
source("~/Desktop/Impacts Systematic Review/scripts/impacts_systematic_review/clean_raw_data.R")

## LOAD PACKAGES ####
library(dplyr)
library(ggplot2)
library(grid)
library(ggthemes)
library(colorRamps)
library(RColorBrewer)

## ORGANIZE DATA ####
impact_and_taxa <- dplyr::select(raw_data, impacttype, invasivespeciestaxa)
head(impact_and_taxa)

## MAKE FIGURES ####
# gg <- ggplot(data = impact_and_taxa, aes(x=impacttype)) + 
# geom_bar(stat="count")
# gg <- gg + facet_grid(invasivespeciestaxa ~ . )
# gg <- gg + scale_fill_manual(values = brewer.pal(name="Set3", n=12), guide="none")
# gg

## Try for different colors
gg <- ggplot(impact_and_taxa) +
  geom_bar(aes(x= impacttype, stat="bin", fill = invasivespeciestaxa)) # start up the plot
gg
gg <- gg + facet_wrap(~invasivespeciestaxa) # Fascet by taxa
gg <- gg + scale_fill_manual(values = colorRampPalette(solarized_pal()(8))(16)) # apply 16 different colors
gg
gg <- gg + ylab("Frequency")
gg <- gg + xlab("Impact Type")
gg <- gg + guides(fill=FALSE) # remove legent
gg
gg <- gg + theme(axis.text=element_text(size=12), # Change tick mark label size
                 axis.title=element_text(size=14,face="bold"),
                 axis.text.x = element_text(angle = 45, hjust = 1)) # Change axis title size
gg

pdf(file="~/Desktop/Impacts Systematic Review/figures/impact_type_fascet_by_taxa.pdf")
gg
dev.off()
dev.off()
