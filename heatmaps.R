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
gg <- gg + scale_fill_viridis(option = "C", name="# of studies", label=comma)
gg <- gg + coord_equal()
gg <- gg + labs(x=NULL, y=NULL, size = 20,title="Impact Type & Ecosystem")
gg <- gg + theme_tufte(base_family="Helvetica")
gg <- gg + theme(plot.title=element_text(hjust=0))
gg <- gg + theme(axis.ticks=element_blank())
gg <- gg + theme(axis.text=element_text(size=18))
gg <- gg + theme(legend.title=element_text(size=8))
gg <- gg + theme(legend.text=element_text(size=6))
gg <- gg + theme(axis.text.x=element_text(angle=23,hjust=1))
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
gg <- gg + labs(x=NULL, y=NULL, size = 20,title="Impact Type & Ecosystem")
gg <- gg + theme_tufte(base_family="Helvetica")
gg <- gg + theme(plot.title=element_text(hjust=0))
gg <- gg + theme(axis.ticks=element_blank())
gg <- gg + theme(axis.text=element_text(size=18))
gg <- gg + theme(legend.title=element_text(size=8))
gg <- gg + theme(legend.text=element_text(size=6))
gg <- gg + theme(axis.text.x=element_text(angle=23,hjust=1))
gg
