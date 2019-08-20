## READ IN DATA ####
source("~/Desktop/ch2_impacts_systematic_review/scripts/impacts_systematic_review/clean_raw_data.R")

## LOAD PACKAGES ####
library(dplyr)
library(ggplot2)
library(scales)
library(ggthemes)
library(cowplot)

## ORGANIZE DATA ####
head(raw_data)
temporal_raw <- select(raw_data,publicationyear,firstyeardetected,firstyearatsite,yearbegins,yearends,studylength)

# R doesn't know what to do with <1 factor, so replace with numerical placeholder
temporal_raw$studylength <- as.character(temporal_raw$studylength) # first make it a character vector
temporal_raw$studylength[temporal_raw$studylength == "<1"] <- "0.5" # then replace <1 with a placeholder of .5
head(temporal_raw)
temporal_raw$studylength <- as.numeric(temporal_raw$studylength) # now conver the study length column back to numeric
temporal_raw # check to make sure all numbers remained

# Make bins so that they align with strayer categories
temporal_raw$studylengthbinned <- cut(temporal_raw$studylength, breaks = c(0,1,3,10,250), labels = c("0-1","1.1-3","3.1-10",">10"))
head(temporal_raw)

## MAKE FIGURE ####
count(count(temporal_raw$studylengthbinned))
      
gg <- ggplot(data=temporal_raw, aes(x=studylengthbinned))
gg <- gg + geom_bar(stat="count", fill = "#fec44f")
gg
gg <- gg + theme_cowplot()
gg
gg <- gg + ylab("Frequency")
gg <- gg + xlab("Study Length")
gg <- gg + theme(axis.text.x = element_text(size=23),
                 axis.text.y = element_text(size=23),
                 axis.title = element_text(size=23))
gg
gg <- gg + scale_y_continuous(expand = c(0, 0), limits = c(0, 1500),
                              breaks= pretty_breaks(n = 7))
gg
pdf(file="~/Desktop/Impacts Systematic Review/figures/figure_studylength_barplot.pdf")
gg
dev.off()
dev.off()



