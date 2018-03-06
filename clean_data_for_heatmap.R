library(tidyr)
library(plyr)
library(knitr)
library(tibble)
library(dplyr)
impacts_raw_data <- read.csv("~/Desktop/Impacts Systematic Review/Crystal-Ornelas_et_al_SR_v18.csv", header=TRUE, na.strings = "")
head(impacts_raw_data)

# Add new column with years binned
# First, have to create new column with 5 year bins
impacts_raw_data$yearbinned <- cut(impacts_raw_data$publicationyear, breaks = c(1998,2004,2010,2017), labels = c("1999-2004","2005-2010","2011-2016"))

# Convert data.frame to table
impacts_t <- tbl_df(impacts_raw_data)
head(impacts_t)
kable(head(impacts_t))

# Subset ecosystem and impact type

# Select only rows with top five impacts
levels(impacts_raw_data$impacttype)

# Select top 7 impacts from 2006-2016
top_impacts <- subset(impacts_raw_data, impacttype %in% c("diversity","fitness","nutrient availability", "habitat change","fitness","abundance","behavior"))
dim(top_impacts)
levels(impacts_raw_data$ecosystem) # Shows that all of the ecosystems are still there

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
top_impacts_and_ecosystem_count <- dplyr::count(top_impacts_and_ecosystem_t, impacttype, ecosystem) # count function has to come BEFORE complete

# Run complete function
top_impacts_and_ecosystem_complete <- complete(top_impacts_and_ecosystem_count, impacttype, ecosystem, fill=list(n=NA))

