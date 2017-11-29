library(dplyr)
library(ggplot2)
library(grid)
library(ggthemes)
library(colorRamps)
library(RColorBrewer)

raw_data <- read.csv("~/Desktop/Impacts Systematic Review/Crystal-Ornelas_et_al_SR_v14.csv", header=TRUE)
head(raw_data)

impact_and_taxa <- dplyr::select(raw_data, impacttype, invasivespeciestaxa)
head(impact_and_taxa)

# gg <- ggplot(data = impact_and_taxa, aes(x=impacttype)) + 
# geom_bar(stat="count")
# gg <- gg + facet_grid(invasivespeciestaxa ~ . )
# gg <- gg + scale_fill_manual(values = brewer.pal(name="Set3", n=12), guide="none")
# gg

## Try for different colors
ggplot(impact_and_taxa) +
  geom_bar(aes(x= impacttype, stat="bin", fill = invasivespeciestaxa)) + 
  facet_wrap(~invasivespeciestaxa) +
  scale_fill_manual(values = colorRampPalette(solarized_pal()(8))(16)) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
