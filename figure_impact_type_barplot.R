library(dplyr)

raw_data <- read.csv("~/Desktop/Impacts Systematic Review/Crystal-Ornelas_et_al_SR_v14.csv", header=TRUE)

impact_and_taxa <- dplyr::select(raw_data, impacttype, invasivespeciestaxa)

gg <- ggplot(impact_and_taxa, 
             aes(x = reorder(impacttype,impacttype,
                            function(x)-length(x))))
gg <-gg + geom_bar(stat="count")
gg <-gg + theme(axis.text.x = element_text(angle = 60, hjust = 1))
gg
