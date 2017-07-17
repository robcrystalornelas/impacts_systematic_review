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
impacts_raw_data <- read.csv("~/Desktop/Impacts Systematic Review/Pecchia_et_al_Impacts_Worksheet_v8.csv", header=TRUE, na.strings = "")
head(impacts_raw_data)

# Remove last row which contains all blank entries
levels(impacts_raw_data$ecosystem)
levels(impacts_raw_data$impacttype)
dim(impacts_raw_data)
impacts_raw_data<-impacts_raw_data[-715,]
dim(impacts_raw_data)

# Convert data.frame to table
impacts_t <- tbl_df(impacts_raw_data)
kable(head(impacts_t))

# Prep data for impact type vs. ecosystem
names(impacts_t)
levels(impacts_t$impacttype)
type_and_eco <- count(impacts_t, impacttype, ecosystem)
class(type_and_eco)

# Heatmap of impact type and ecosystem
kable(head(type_and_eco))
gg <- ggplot(type_and_eco, aes(x=impacttype, y=ecosystem, fill=n))
gg <- gg + geom_tile(color="black", size=0.1) # This tells we want every block to have a thin black border
gg <- gg + scale_fill_viridis(name="# of studies", label=comma)
gg <- gg + coord_equal()
gg <- gg + labs(x=NULL, y=NULL, size = 20,title="Impact Type & Ecosystem")
gg <- gg + theme_tufte(base_family="Helvetica")
gg <- gg + theme(plot.title=element_text(hjust=0))
gg <- gg + theme(axis.ticks=element_blank())
gg <- gg + theme(axis.text=element_text(size=18))
gg <- gg + theme(legend.title=element_text(size=8))
gg <- gg + theme(legend.text=element_text(size=6))
gg <- gg + theme(axis.text.x=element_text(angle=45,hjust=1))
gg

# Heatmap of impact type and ecosystem COMPLETE
# Use tidyr:complete since we have missing values
head(type_and_eco)
type_and_eco_complete <- complete(type_and_eco, impacttype, ecosystem, fill=list(n=0))
kable(head(type_and_eco_complete))
gg <- ggplot(type_and_eco_complete, aes(x=impacttype, y=ecosystem, fill=n))
gg <- gg + geom_tile(color="black", size=0.1) # This tells we want every block to have a thin black border
gg <- gg + scale_fill_viridis(option = "C", name="# of studies", label=comma) # This provides a color-blind friendly palette.  I chose option C for the Vidiris palettes
gg <- gg + coord_equal()
gg <- gg + labs(x = NULL, y = NULL, title="Impact Type & Ecosystem") # This is the title of the plot
gg <- gg + theme_tufte(base_family="Helvetica")
# gg <- gg + theme(plot.title=element_text(hjust=0))
gg <- gg + theme(plot.title = element_text(size=18)) # size of plot title
gg <- gg + theme(axis.ticks=element_blank())
gg <- gg + theme(axis.text=element_text(size=18))
gg <- gg + theme(legend.title=element_text(size=8))
gg <- gg + theme(legend.text=element_text(size=6))
gg <- gg + theme(axis.text.x=element_text(angle=45,hjust=1))
gg

#############################
#############################
# Prep data for biological level vs. scale
names(impacts_t)
levels(impacts_t$impacttype)
biologicalscale_and_spatialscale <- count(impacts_t, spatialscale, impact_category_lockwood)

# Heatmap of impact type and ecosystem COMPLETE
# Use tidyr:complete since we have missing values
head(biologicalscale_and_spatialscale)
spatial_and_level_complete <- complete(biologicalscale_and_spatialscale, spatialscale, impact_category_lockwood, fill=list(n=0))
kable(head(spatial_and_level_complete))
gg <- ggplot(spatial_and_level_complete, aes(x=spatialscale, y=impact_category_lockwood, fill=n))
gg <- gg + geom_tile(color="black", size=0.1) # This tells we want every block to have a thin black border
gg <- gg + scale_fill_viridis(option = "C", name="# of studies", label=comma) # This provides a color-blind friendly palette.  I chose option C for the Vidiris palettes
gg <- gg + coord_equal()
gg <- gg + labs(x = NULL, y = NULL, title="Bioloigcal Level & Spatial Scale") # This is the title of the plot
gg <- gg + theme_tufte(base_family="Helvetica")
# gg <- gg + theme(plot.title=element_text(hjust=0))
gg <- gg + theme(plot.title = element_text(size=18)) # size of plot title
gg <- gg + theme(axis.ticks=element_blank())
gg <- gg + theme(axis.text=element_text(size=18))
gg <- gg + theme(legend.title=element_text(size=8))
gg <- gg + theme(legend.text=element_text(size=6))
gg <- gg + theme(axis.text.x=element_text(angle=45,hjust=1))
gg

#############################
#############################
# Prep data for biological level vs. scale
names(impacts_t)
impact_and_spatial <- count(impacts_t, impacttype, spatialscale)

# Heatmap of impact type and ecosystem COMPLETE
# Use tidyr:complete since we have missing values
head(impact_and_spatial)
spatial_and_impact_complete <- complete(impact_and_spatial, spatialscale, impacttype, fill=list(n=0))
kable(head(spatial_and_impact_complete))
gg <- ggplot(spatial_and_impact_complete, aes(x=spatialscale, y=impacttype, fill=n))
gg <- gg + geom_tile(color="black", size=0.1) # This tells we want every block to have a thin black border
gg <- gg + scale_fill_viridis(option = "C", name="# of studies", label=comma) # This provides a color-blind friendly palette.  I chose option C for the Vidiris palettes
gg <- gg + coord_equal()
gg <- gg + labs(x = NULL, y = NULL, title="Impact & Spatial Scale") # This is the title of the plot
gg <- gg + theme_tufte(base_family="Helvetica")
# gg <- gg + theme(plot.title=element_text(hjust=0))
gg <- gg + theme(plot.title = element_text(size=18)) # size of plot title
gg <- gg + theme(axis.ticks=element_blank())
gg <- gg + theme(axis.text=element_text(size=18))
gg <- gg + theme(legend.title=element_text(size=8))
gg <- gg + theme(legend.text=element_text(size=6))
gg <- gg + theme(axis.text.x=element_text(angle=45,hjust=1))
gg

#############################
## Biological Level Vs. Sampling Frequency
#############################

#############################
# Prep data for biological level vs. Sampling Frequency
names(impacts_t)
level_and_frequency <- count(impacts_t, impact_category_lockwood, samplingfrequency)

# Heatmap of impact type and ecosystem COMPLETE
# Use tidyr:complete since we have missing values
head(level_and_frequency)
level_and_frequency_complete <- complete(level_and_frequency, impact_category_lockwood, samplingfrequency, fill=list(n=0))
kable(head(level_and_frequency_complete))
gg <- ggplot(level_and_frequency_complete, aes(x=impact_category_lockwood, y=samplingfrequency, fill=n))
gg <- gg + geom_tile(color="black", size=0.1) # This tells we want every block to have a thin black border
gg <- gg + scale_fill_viridis(option = "C", name="# of studies", label=comma) # This provides a color-blind friendly palette.  I chose option C for the Vidiris palettes
gg <- gg + coord_equal()
gg <- gg + labs(x = NULL, y = NULL, title="Frequency & Biological Level") # This is the title of the plot
gg <- gg + theme_tufte(base_family="Helvetica")
# gg <- gg + theme(plot.title=element_text(hjust=0))
gg <- gg + theme(plot.title = element_text(size=18)) # size of plot title
gg <- gg + theme(axis.ticks=element_blank())
gg <- gg + theme(axis.text=element_text(size=18))
gg <- gg + theme(legend.title=element_text(size=8))
gg <- gg + theme(legend.text=element_text(size=6))
gg <- gg + theme(axis.text.x=element_text(angle=45,hjust=1))
gg

#############################
## Impact Vs. Sampling Frequency
#############################

# Prep data for impact vs. Sampling Frequency
names(impacts_t)
impact_and_frequency <- count(impacts_t, impacttype, samplingfrequency)

# Heatmap of impact type and ecosystem COMPLETE
# Use tidyr:complete since we have missing values
head(impact_and_frequency)
impact_and_frequency_complete <- complete(impact_and_frequency, impacttype, samplingfrequency, fill=list(n=0))
kable(head(impact_and_frequency_complete))
gg <- ggplot(impact_and_frequency_complete, aes(x=samplingfrequency, y=impacttype, fill=n))
gg <- gg + geom_tile(color="black", size=0.1) # This tells we want every block to have a thin black border
gg <- gg + scale_fill_viridis(option = "C", name="# of studies", label=comma) # This provides a color-blind friendly palette.  I chose option C for the Vidiris palettes
gg <- gg + coord_equal()
gg <- gg + labs(x = NULL, y = NULL, title="Frequency & Impact") # This is the title of the plot
gg <- gg + theme_tufte(base_family="Helvetica")
# gg <- gg + theme(plot.title=element_text(hjust=0))
gg <- gg + theme(plot.title = element_text(size=18)) # size of plot title
gg <- gg + theme(axis.ticks=element_blank())
gg <- gg + theme(axis.text=element_text(size=18))
gg <- gg + theme(legend.title=element_text(size=8))
gg <- gg + theme(legend.text=element_text(size=6))
gg <- gg + theme(axis.text.x=element_text(angle=45,hjust=1))
gg

#############################
## Ecosystem Vs. Sampling Frequency
#############################

# Prep data for impact vs. Sampling Frequency
names(impacts_t)
ecosystem_and_frequency <- count(impacts_t, ecosystem, samplingfrequency)

# Heatmap of impact type and ecosystem COMPLETE
# Use tidyr:complete since we have missing values
head(ecosystem_and_frequency)
ecosystem_and_frequency_complete <- complete(ecosystem_and_frequency, ecosystem, samplingfrequency, fill=list(n=0))
kable(head(ecosystem_and_frequency_complete))
gg <- ggplot(ecosystem_and_frequency_complete, aes(x=samplingfrequency, y=ecosystem, fill=n))
gg <- gg + geom_tile(color="white", size=0.1) # This tells we want every block to have a thin black border
gg <- gg + scale_fill_viridis(option = "C", name="# of studies", label=comma) # This provides a color-blind friendly palette.  I chose option C for the Vidiris palettes
gg <- gg + coord_equal()
gg <- gg + labs(x = NULL, y = NULL, title="Ecosystem & Sampling Frequency") # This is the title of the plot
gg <- gg + theme_tufte(base_family="Helvetica")
# gg <- gg + theme(plot.title=element_text(hjust=0)) # Alternative way to set title of plot
gg <- gg + theme(plot.title = element_text(size=18)) # size of plot title
# gg <- gg + theme(axis.ticks=element_blank()) # Un-comment if we want no tick marks
gg <- gg + theme(axis.text=element_text(size=18))
gg <- gg + theme(legend.title=element_text(size=8))
gg <- gg + theme(legend.text=element_text(size=6))
gg <- gg + theme(axis.text.x=element_text(angle=45,hjust=1))
gg

