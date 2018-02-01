### Heatmaps for impact type and taxa

library(ggplot2)
library(dplyr)

# Import CSV file of systematic review
# Include argument na.strings = ""
impacts_raw_data <- read.csv("~/Desktop/Impacts Systematic Review/Pecchia_et_al_Impacts_Worksheet_v10.csv", header=TRUE, na.strings = "")
head(impacts_raw_data)

# Remove last row which contains all blank entries
levels(impacts_raw_data$ecosystem)
levels(impacts_raw_data$impacttype)
dim(impacts_raw_data)
impacts_raw_data<-impacts_raw_data[-1533,]
dim(impacts_raw_data)

# Convert data.frame to table
impacts_t <- tbl_df(impacts_raw_data)
head(impacts_t)
kable(head(impacts_t))

#############################
## Heatmap of Impact vs. Taxa FULL
##############################

names(impacts_t)
levels(impacts_t$impacttype)
type_and_taxa <- count(impacts_t, impacttype, invasivespeciestaxa)

# Heatmap of impact type and ecosystem
kable(head(type_and_taxa))

gg <- ggplot(type_and_taxa, aes(x=impacttype, y=invasivespeciestaxa, fill=n))
gg <- gg + geom_tile(color="white", size=0.1) # This tells we want every block to have a thin black border
gg <- gg + scale_fill_viridis(option = "C", name="# of cases", label=comma) # This provides a color-blind friendly palette.  I chose option C for the Vidiris palettes
gg <- gg + coord_equal()
gg <- gg + labs(x = NULL, y = NULL, title="Impact Type & Taxa") # This is the title of the plot
gg <- gg + theme_tufte(base_family="Helvetica")
# gg <- gg + theme(plot.title=element_text(hjust=0))
gg <- gg + theme(plot.title = element_text(size=18)) # size of plot title
# gg <- gg + theme(axis.ticks=element_blank())
gg <- gg + theme(axis.text=element_text(size=18))
gg <- gg + theme(legend.title=element_text(size=15))
gg <- gg + theme(legend.text=element_text(size=13))
gg <- gg + theme(axis.text.x=element_text(angle=45,hjust=1))
gg

#############################
## Impact vs. Taxa TOP FIVE IMPACTS
##############################

# Select only rows with top five impacts
levels(impacts_raw_data$impacttype)

# Do all subsetting without dplyr
top_impacts <- subset(impacts_raw_data, impacttype %in% c("diversity","fitness","nutrient availability", "habitat change","fitness","abundance","behavior"))
dim(top_impacts)
top_impacts_and_taxa <-subset(top_impacts, invasivespeciestaxa %in% c("herbaceous plant","tree", "crustacean", "fish","mammal","molluscs","insect") )
dim(top_impacts_and_taxa)
# Drop unused levels from data.frame
top_impacts_and_taxa[] <- lapply(top_impacts_and_taxa, function(x) if(is.factor(x)) factor(x) else x)

top_impacts_and_taxa_t <- tbl_df(top_impacts_and_taxa)

impact_and_taxa_count <- count(top_impacts_and_taxa, impacttype, invasivespeciestaxa) # count function has to come BEFORE complete
impact_and_taxa_complete <- complete(impact_and_taxa_count, impacttype, invasivespeciestaxa, fill=list(n=NA))

gg <- ggplot(impact_and_taxa_complete, aes(x=impacttype, y=invasivespeciestaxa, fill=n))
gg <- gg + geom_tile(color="white", size=0.1) # This tells we want every block to have a thin black border
gg <- gg + scale_fill_viridis(option = "C", name="# of cases", label=comma) # This provides a color-blind friendly palette.  I chose option C for the Vidiris palettes
gg <- gg + coord_equal()
gg <- gg + labs(x = NULL, y = NULL, title="Impact Type & Selected Taxa") # This is the title of the plot
gg <- gg + theme_tufte(base_family="Helvetica")
# gg <- gg + theme(plot.title=element_text(hjust=0))
gg <- gg + theme(plot.title = element_text(size=18)) # size of plot title
# gg <- gg + theme(axis.ticks=element_blank())
gg <- gg + theme(axis.text=element_text(size=18))
gg <- gg + theme(legend.title=element_text(size=15))
gg <- gg + theme(legend.text=element_text(size=13))
gg <- gg + theme(axis.text.x=element_text(angle=45,hjust=1))
gg

# Plot the figure
pdf(file="~/Desktop/Impacts Systematic Review/figures/impact_and_taxa_subset.pdf")
plot(gg)
dev.off()
dev.off()
gg # plot it in the R studio window


