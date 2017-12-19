## READ IN DATA ####
source("~/Desktop/Impacts Systematic Review/scripts/impacts_systematic_review/clean_raw_data.R")

## LOAD PACKAGES ####
library(dplyr)
library(ggplot2)
library(ggthemes)

## ORGANIZE DATA ####
head(raw_data)
temporal_and_impactype <- select(raw_data,publicationyear,impacttype,firstyeardetected,firstyearatsite,yearbegins,yearends,studylength)

# R doesn't know what to do with <1 factor, so replace with numerical placeholder
temporal_and_impactype$studylength <- as.character(temporal_and_impactype$studylength) # first make it a character vector
temporal_and_impactype$studylength[temporal_and_impactype$studylength == "<1"] <- "0.5" # then replace <1 with a placeholder of .5
head(temporal_and_impactype)
temporal_and_impactype$studylength <- as.numeric(temporal_and_impactype$studylength) # now conver the study length column back to numeric
temporal_and_impactype # check to make sure all numbers remained

# Make bins so that they align with strayer categories
temporal_and_impactype$studylengthbinned <- cut(temporal_and_impactype$studylength, breaks = c(0,1,3,10,250), labels = c("0-1","1.1-3","3.1-10",">10"))
head(temporal_and_impactype)

## MAKE FIGURES ####
gg <- ggplot(temporal_and_impactype) +
  geom_bar(aes(x= studylengthbinned, stat="bin", fill = impacttype)) + 
  facet_wrap(~impacttype)
gg
gg <- gg + scale_fill_manual(values = colorRampPalette(ptol_pal()(8))(11))
gg
gg <- gg + theme_tufte()
gg <- gg + theme(axis.text.x = element_text(angle = 60, hjust = 1))
gg <- gg + ylab("Frequency")
gg <- gg + xlab("Study Length")
gg <- gg + ggtitle("Study Length by Impact Measured")
gg

pdf(file="~/Desktop/Impacts Systematic Review/figures/studylength_barplot_by_impacttype.pdf")
gg
dev.off()
dev.off()
