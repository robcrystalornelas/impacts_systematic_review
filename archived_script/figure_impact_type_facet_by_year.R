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
impact_and_year <- dplyr::select(raw_data, impacttype, publicationyear)

## MAKE FIGURES ####
# TEST Barplot of overall impact type barplot
gg <- ggplot(impact_and_year, 
             aes(x = reorder(impacttype,impacttype,
                             function(x)-length(x))))
gg <-gg + geom_bar(stat="count")
gg <-gg + theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 20))
gg

# Now created same figure, but fasceted in 6 year increments
# First, have to create new column with 6 year bins
impact_and_year$yearbinned <- cut(impact_and_year$publicationyear, breaks = c(1998,2004,2010,2017), labels = c("1999-2004","2005-2010","2011-2016"))

# Then create fasceted graph
gg <- ggplot(impact_and_year, 
             aes(x = reorder(impacttype,impacttype,
                             function(x)-length(x))))
gg <- gg + geom_bar(aes(fill=yearbinned), stat="count")
gg <- gg + facet_grid(.~yearbinned)
gg
gg <- gg + scale_fill_manual(values = brewer.pal(name="Set2", n=3), guide="none")
gg <- gg + theme_bw() + theme(strip.background  = element_blank(),
                          panel.grid.major = element_line(colour = "grey80"),
                          panel.border = element_blank(),
                          axis.ticks = element_blank(),
                          panel.grid.minor.x=element_blank(),
                          panel.grid.major.x=element_blank() ) + theme(legend.position="bottom")
gg <- gg + theme(axis.text.x = element_text(angle = 60, hjust = 1))
gg
gg <- gg + ylab("Frequency")
gg <- gg + xlab("Impact Type")
gg <- gg + theme(axis.text=element_text(size=12), # Change tick mark label size
                 axis.title=element_text(size=14,face="bold"))
gg

pdf(file="~/Desktop/Impacts Systematic Review/figures/impact_type_fasceted_by_year.pdf")
gg
dev.off()
dev.off()

gg

