## READ IN DATA ####
source("~/Desktop/ch2_impacts_systematic_review/scripts/impacts_systematic_review/clean_raw_data.R")

## Load libraries #####
library(dplyr)
library(ggplot2)
library(cowplot)

taxa_for_barplot <- dplyr::select(raw_data, latinname, invasivespeciestaxapysek)
taxa_for_barplot
unique_taxa_for_barplot <- unique(taxa_for_barplot)
dim(unique_taxa_for_barplot)
unique_taxa_for_barplot

# Make the plot
gg_taxa <-
  ggplot(unique_taxa_for_barplot, aes(x = reorder(invasivespeciestaxapysek, invasivespeciestaxapysek, function(x)
    -
      length(x))))
gg_taxa <- gg_taxa + geom_bar(stat = "count", fill = "#7e4e90ff") +
  # coord_cartesian(ylim=c(0,1050), expand = FALSE) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 325)) 

gg_taxa
gg_taxa <- gg_taxa + theme_cowplot()
gg_taxa <- gg_taxa + ylab("Frequency")
gg_taxa <- gg_taxa + xlab("")
gg_taxa <-
  gg_taxa + theme(
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
gg_taxa
