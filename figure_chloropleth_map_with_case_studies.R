library(httr)     # getting data
library(rgdal)    # working with shapefile
library(dplyr)    # awesome data manipulation
library(readr)    # faster reading of CSV data
library(stringi)  # string manipulation
library(stringr)  # string manipulation
library(tidyr)    # reshaping data
library(grid)     # for 'unit'
library(scales)   # for 'percent'
library(ggplot2)  # plotting
library(ggthemes) # theme_map
library(sp)
library(rgdal)
library(dplyr)
library(rvest)
library(stringi)
library(scales)
library(RColorBrewer)
library(ggplot2)
library(geojsonio)

raw_with_iso <- read.csv("/Users/rpecchia/Desktop/Impacts Systematic Review/output/raw_data_with_ISO.csv", header = T)

# Get the base map we'll work with
try(invisible(GET("http://www.pewglobal.org/wp-content/lib/js/world-geo.json",
                  write_disk("world-geo.json"))), silent=TRUE)
world <- readOGR("world-geo.json", "OGRGeoJSON")
world_wt <- spTransform(world, CRS("+proj=robin"))
world_map <- fortify(world_wt)

world_map %>%
  left_join(data_frame(id=rownames(world@data), name=world@data$name)) %>%
  dplyr::select(-id) %>%
  rename(id=name) -> world_map
unique((world_map$id))


# Now work with my data
head(raw_with_iso)
raw_with_iso <- read.csv("/Users/rpecchia/Desktop/Impacts Systematic Review/output/raw_data_with_ISO.csv", header = T)
only_country_iso_and_code <- dplyr::select(raw_with_iso, code, country, isocountrycode)
head(only_country_iso_and_code)

outage <- only_country_iso_and_code[complete.cases(only_country_iso_and_code),]
head(outage)
colnames(outage) <- c("not_imp","id","iso")
count_of_outage <- count(outage, id) # get count of how many studies per country
outage_df_countryname <- as.data.frame(count_of_outage)
head(outage_df_countryname)
outage_df_countryname
colnames(outage_df_countryname) <- c("id", "count") # rename columns to work well with ggplot
outage_df_countryname$id <- stri_trans_totitle(outage_df_countryname) # Convert country code so the first letter is Cap
head(outage_df_countryname)
outage_df_countryname$count <- as.numeric(outage_df_countryname$count)

# Custom breaks for data
outage_df_countryname$out <- cut(outage_df_countryname$count,
                  breaks=c(0, 90, 180, 270, 360, 450, 540, 630, 820),
                  labels=c("0-90", "91-180", "181-270", "271-360",
                           "361-450", "451-540", "541-630",
                           "631-812"))

# Make the map

unique(world_map$id)
gg <- ggplot(data=world_map, aes(map_id=id))
gg <- gg + geom_map(map=world_map, aes(x=long, y=lat),
                    color="#0e0e0e", fill="white", size=0.2)
gg
gg <- gg + geom_map(data=outage_df_countryname, map=world_map, aes(fill=count),
                    color="#0e0e0e", size=0.2)
gg
gg <- gg + coord_equal()
# gg <- gg + labs(title=sprintf("Number of case studies by country",
#                               comma(sum(outage$without_power))))
gg <- gg + theme_map()
gg
gg <- gg + theme(legend.position="right")
gg
