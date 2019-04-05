library(ggpubr)
library(ggthemes)
## READ IN DATA ####
source("~/Desktop/ch2_impacts_systematic_review/scripts/impacts_systematic_review/clean_raw_data.R")

## ORGANIZE DATA ####
impact_and_taxa <- dplyr::select(raw_data, impacttype, invasivespeciestaxa)
head(impact_and_taxa)

counted_impacts <- as.data.frame(dplyr::count(raw_data, impacttype)) 
counted_impacts

# Change barplot order
impact_and_taxa_reordered <- impact_and_taxa
impact_and_taxa_reordered$impacttype <- factor(impact_and_taxa$impacttype, c("diversity", "fitness", "abundance", "nutrient availability","behavior","indirect","growth","other","production","hybridization","habitat change"))

## MAKE FIGURES ####
# Make figure for Effect X Taxa
head(impact_and_taxa_reordered)

selected <- c("amphibians and reptiles","crustacean","fish","grasses","herbaceous plant","insect","mammal","molluscs","tree")
subset_of_impact_and_taxa <- impact_and_taxa_reordered[impact_and_taxa_reordered$invasivespeciestaxa %in% selected,]
dim(subset_of_impact_and_taxa)

# Make the subset plot

gg_interaction <- ggplot(subset_of_impact_and_taxa) +
  geom_bar(aes(x= impacttype, stat="bin", fill = invasivespeciestaxa)) # start up the plot
gg_interaction
gg_interaction <- gg_interaction + facet_wrap(~invasivespeciestaxa) # Fascet by taxa
gg_interaction <- gg_interaction + scale_fill_manual(values = colorRampPalette(solarized_pal()(8))(10)) # apply 16 different colors
gg_interaction
gg_interaction <- gg_interaction + theme_tufte()
gg_interaction
gg_interaction <- gg_interaction + ylab("Frequency")
gg_interaction <- gg_interaction + xlab("Ecological Effect")
gg_interaction <- gg_interaction + guides(fill=FALSE) # remove legent
gg_interaction
gg_interaction <- gg_interaction + theme(axis.text=element_text(size=15), # Change tick mark label size
                 axis.title=element_text(size=15,face="bold"),
                 axis.text.x = element_text(angle = 90, hjust=1, vjust = 0.5),
                 strip.text = element_text(size=15)) # Change axis title size
gg_interaction


## Export just the fascet
pdf(file="~/Desktop/ch2_impacts_systematic_review/figures/figure_ecological_effect_fascet.pdf")
gg_interaction
dev.off()
dev.off()

# Now make figure for overall

## MAKE FIGURES ####
gg_overall <- ggplot(impact_and_taxa, aes(x = reorder(impacttype,impacttype, function(x)-length(x))))
gg_overall <-gg_overall + geom_bar(stat="count", fill = "#1f78b4")
gg_overall
gg_overall <- gg_overall + theme_tufte()
gg_overall <- gg_overall + ylab("Frequency")
# gg_overall <- gg_overall + xlab("Ecological Effect")
gg_overall <- gg_overall + theme(axis.text.x = element_text(size=15, angle = 90, hjust = 1, vjust = .5),
                 axis.text.y = element_text(size=15),
                 axis.title = element_text(size=20),
                 axis.title.x=element_blank())
gg_overall


gg_overall_and_effect_fascet_by_taxa <- ggarrange(gg_overall, gg_interaction,
          labels = c("A", "B"),
          ncol = 1, nrow = 2)
gg_overall_and_effect_fascet_by_taxa

pdf(file="~/Desktop/Impacts Systematic Review/figures/ecological_effect_barplot_and_fascet_combined.pdf")
gg_overall_and_effect_fascet_by_taxa
dev.off()
dev.off()
