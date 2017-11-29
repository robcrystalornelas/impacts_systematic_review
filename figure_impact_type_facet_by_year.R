library(dplyr)
library(ggplot2)

raw_data <- read.csv("~/Desktop/Impacts Systematic Review/Crystal-Ornelas_et_al_SR_v14.csv", header=TRUE)

head(raw_data)
impact_and_year <- dplyr::select(raw_data, impacttype, publicationyear)

# Barplot of overall impact type barplot
gg <- ggplot(impact_and_year, 
             aes(x = reorder(impacttype,impacttype,
                             function(x)-length(x))))
gg <-gg + geom_bar(stat="count")
gg <-gg + theme(axis.text.x = element_text(angle = 60, hjust = 1))
gg

# Now created same figure, but fasceted in 5 year increments

# First, have to create new column with 5 year bins
impact_and_year$yearbinned <- cut(impact_and_year$publicationyear, breaks = c(1998,2004,2010,2017), labels = c("1999-2004","2005-2010","2011-2016"))

# Then create fasceted graph
gg <- ggplot(impact_and_year, 
             aes(x = reorder(impacttype,impacttype,
                             function(x)-length(x))))
gg <- gg + geom_bar(aes(fill=yearbinned), stat="count")

gg <- gg + facet_grid(.~yearbinned)
gg <- gg + scale_fill_manual(values = brewer.pal(name="Set2", n=3), guide="none")
gg <- gg + theme_bw() + theme( strip.background  = element_blank(),
                          panel.grid.major = element_line(colour = "grey80"),
                          panel.border = element_blank(),
                          axis.ticks = element_blank(),
                          panel.grid.minor.x=element_blank(),
                          panel.grid.major.x=element_blank() ) + theme(legend.position="bottom")
gg <- gg + theme(axis.text.x = element_text(angle = 60, hjust = 1))

gg
