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
impacts_raw_data<-impacts_raw_data[-715,]
dim(impacts_raw_data)

# Convert data.frame to table
impacts_t <- tbl_df(impacts_raw_data)
head(impacts_t)
kable(head(impacts_t))

#############################
## Impact vs. Taxa FULL
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

#############################
## Biological Level Vs. Sampling Frequency
#############################

levels(impacts_raw_data$samplingfrequency)

# remove NA sampling design
impacts_with_sampling_design_cleaned <- subset(impacts_raw_data, samplingfrequency %in% c("one year","less than one year", "more than one year", "single measurement") )
dim(impacts_with_sampling_design_cleaned)

# Drop unused levels from data.frame
impacts_with_sampling_design_cleaned[] <- lapply(impacts_with_sampling_design_cleaned, function(x) if(is.factor(x)) factor(x) else x)

# Re-order sampling freq.
impacts_with_sampling_design_cleaned$samplingfrequency <- factor(impacts_with_sampling_design_cleaned$samplingfrequency, levels=c("single measurement", "less than one year", "one year", "more than one year"))

# Re-order biological level
impacts_with_sampling_design_cleaned$impact_category_lockwood <- factor(impacts_with_sampling_design_cleaned$impact_category_lockwood, levels=c("genetic", "individual", "population","community","ecosystem"))

# Turn into table
impacts_with_sampling_design_cleaned_t <- tbl_df(impacts_with_sampling_design_cleaned)

# Run count function
level_and_frequency <- count(impacts_with_sampling_design_cleaned_t, impact_category_lockwood, samplingfrequency)
head(level_and_frequency)

# Heatmap of impact type and ecosystem COMPLETE
# Use tidyr:complete since we have missing values
level_and_frequency_complete <- complete(level_and_frequency, impact_category_lockwood, samplingfrequency, fill=list(n=NA))
kable(head(level_and_frequency_complete))

gg <- ggplot(level_and_frequency_complete, aes(x=impact_category_lockwood, y=samplingfrequency, fill=n))
gg <- gg + geom_tile(color="white", size=0.1) # This tells we want every block to have a thin black border
gg <- gg + scale_fill_viridis(option = "C", name="# of cases", label=comma) # This provides a color-blind friendly palette.  I chose option C for the Vidiris palettes
gg <- gg + coord_equal()
gg <- gg + labs(x = NULL, y = NULL, title="Time between measurements & biological level") # This is the title of the plot
gg <- gg + theme_tufte(base_family="Helvetica")
# gg <- gg + theme(plot.title=element_text(hjust=0))
gg <- gg + theme(plot.title = element_text(size=18)) # size of plot title
# gg <- gg + theme(axis.ticks=element_blank())
gg <- gg + theme(axis.text=element_text(size=18))
gg <- gg + theme(legend.title=element_text(size=15))
gg <- gg + theme(legend.text=element_text(size=13))
gg <- gg + theme(axis.text.x=element_text(angle=45,hjust=1))

# Plot the figure
pdf(file="~/Desktop/Impacts Systematic Review/figures/biolevel_vs_sampling_frequency.pdf")
plot(gg)
dev.off()
dev.off()
gg # plot it in the R studio window

#############################
## Impact vs. Ecosystem SUBSET
##############################

# Select only rows with top five impacts
levels(impacts_raw_data$impacttype)

# Select top impacts
top_impacts <- subset(impacts_raw_data, impacttype %in% c("diversity","fitness","nutrient availability", "habitat change","fitness","abundance","behavior"))
dim(top_impacts)

levels(impacts_raw_data$ecosystem)
# Select top ecosystems
top_impacts_and_ecosystem <-subset(top_impacts, ecosystem %in% c("forest","lotic", "lentic", "estuarine","island","grassland") )
dim(top_impacts_and_ecosystem)

# Drop unused levels from data.frame
top_impacts_and_ecosystem[] <- lapply(top_impacts_and_ecosystem, function(x) if(is.factor(x)) factor(x) else x)

# Re-order impact type
top_impacts_and_ecosystem$impacttype <- factor(top_impacts_and_ecosystem$impacttype, levels=c("behavior","fitness","abundance","diversity", "nutrient availability","habitat change"))

# Re-order ecosystem
top_impacts_and_ecosystem$ecosystem <- factor(top_impacts_and_ecosystem$ecosystem, levels=c("forest", "lotic", "lentic","estuarine","island","grassland"))

# Run table function
top_impacts_and_ecosystem_t <- tbl_df(top_impacts_and_ecosystem)

# Run count function
top_impacts_and_ecosystem_count <- count(top_impacts_and_ecosystem_t, impacttype, ecosystem) # count function has to come BEFORE complete

# Run complete function
top_impacts_and_ecosystem_complete <- complete(top_impacts_and_ecosystem_count, impacttype, ecosystem, fill=list(n=NA))

gg <- ggplot(top_impacts_and_ecosystem_complete, aes(x=impacttype, y=ecosystem, fill=n))
gg <- gg + geom_tile(color="white", size=0.1) # This tells we want every block to have a thin black border
gg <- gg + scale_fill_viridis(option = "C", name="# of cases", label=comma) # This provides a color-blind friendly palette.  I chose option C for the Vidiris palettes
gg <- gg + coord_equal()
gg <- gg + labs(x = NULL, y = NULL, title="Impact Type & Ecosystem") # This is the title of the plot
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
pdf(file="~/Desktop/Impacts Systematic Review/figures/impact_and_ecosystem_subset.pdf")
plot(gg)
dev.off()
dev.off()
gg # plot it in the R studio window



