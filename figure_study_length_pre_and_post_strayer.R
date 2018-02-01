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
temporal_raw$studylegnthbinned <- cut(temporal_raw$studylength, breaks = c(0,1,3,10,250), labels = c("0-1","1.1-3","3.1-10",">10"))
head(temporal_raw)

temporal_raw$strayer <- ifelse(temporal_raw$publicationyear > 2006,"post-strayer", "pre-strayer")
temporal_raw$strayer <- as.factor(temporal_raw$strayer)
tail(temporal_raw)

## MAKE FIGURES ####
# Make stacked barplot
gg <- ggplot(data=temporal_raw, aes(x=studylegnthbinned, fill = factor(strayer)))
gg <- gg + geom_bar(stat="count")
gg
gg <- gg + scale_fill_brewer(palette="Set2")
gg <- gg + theme_tufte()
gg
gg <- gg + ylab("Frequency")
gg <- gg + xlab("Study Length")
gg <- gg + theme(legend.title=element_blank()) # Remove legen title
gg <- gg + theme(axis.text=element_text(size=12), # Change tick mark label size
                 axis.title=element_text(size=14,face="bold"))
gg

pdf(file="~/Desktop/Impacts Systematic Review/figures/studylength_pre_and_post_strayer.pdf")
gg
dev.off()
dev.off()
