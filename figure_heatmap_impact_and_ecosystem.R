#### Heatmap comparisons on ecosystem and impact type

library(ggplot2)
library(dplyr)
library(data.table)  # faster fread() and better weekdays()
library(dplyr)       # consistent data.frame operations
library(purrr)       # consistent & safe list/vector munging
library(tidyr)       # consistent data.frame cleaning
library(lubridate)   # date manipulation
library(countrycode) # turn country codes into pretty names
library(ggplot2)     # base plots are for Coursera professors
library(scales)      # pairs nicely with ggplot2 for plot label formatting
library(gridExtra)   # a helper for arranging individual ggplot objects
library(ggthemes)    # has a clean theme for ggplot2
library(viridis)     # best. color. palette. evar.
library(knitr)   

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



