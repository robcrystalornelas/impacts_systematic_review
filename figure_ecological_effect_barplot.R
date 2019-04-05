## READ IN DATA ####
source("~/Desktop/ch2_impacts_systematic_review/scripts/impacts_systematic_review/clean_raw_data.R")

## LOAD PACKAGES ####
library(plyr)
library(dplyr)
library(ggplot2)
library(ggthemes)

## ORGANIZE DATA ####
impact_and_taxa <- dplyr::select(raw_data, impacttype, invasivespeciestaxa)
head(impact_and_taxa)
dim(impact_and_taxa)
counted_impacts <- as.data.frame(dplyr::count(raw_data, impacttype)) 
counted_impacts

## MAKE FIGURES ####
gg <- ggplot(impact_and_taxa, aes(x = reorder(impacttype,impacttype, function(x)-length(x))))
gg <-gg + geom_bar(stat="count", fill = "#1f78b4")
gg
gg <- gg + theme_tufte()
gg <- gg + ylab("Frequency")
gg <- gg + xlab("Ecological Effect")
gg <- gg + theme(axis.text=element_text(size=20), # Change tick mark label size
      axis.title=element_text(size=18,face="bold"),
      axis.text.x = element_text(angle = 90, hjust=1, vjust = 0.5),
      strip.text = element_text(size=20)) # Change axis title size

gg

## Figure for defense
gg <- ggplot(impact_and_taxa, aes(x = reorder(impacttype,impacttype, function(x)-length(x))))
gg <-gg + geom_bar(stat="count", fill = "#1f78b4")
gg
gg <- gg + theme_tufte()
gg <- gg + ylab("Frequency")
gg <- gg + xlab("Ecological Effect")
gg <- gg + theme(axis.text=element_text(size=30), # Change tick mark label size
                 axis.title=element_text(size=25,face="bold"),
                 axis.text.x = element_text(angle = 90, hjust=1, vjust = 0.5, size = 30),
                 strip.text = element_text(size=40)) # Change axis title size

gg
dev.off()
dev.off()
