## READ IN DATA ####
source("~/Desktop/Impacts Systematic Review/scripts/impacts_systematic_review/clean_raw_data.R")

## LOAD PACKAGES ####
library(dplyr)
library(ggplot2)

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

## MAKE FIGURES ####

# Overall Study Length
gg <- ggplot(data=temporal_raw, aes(x=studylegnthbinned))
gg <- gg + geom_bar(stat="count", fill = "#fec44f")
gg
gg <- gg + theme_tufte()
gg
gg <- gg + ylab("Frequency")
gg <- gg + xlab("Study Length")
gg <- gg + theme(axis.text=element_text(size=12), # Change tick mark label size
                 axis.title=element_text(size=14,face="bold"))
gg

pdf(file="~/Desktop/Impacts Systematic Review/figures/studylength_barplot.pdf")
gg
dev.off()
dev.off()



