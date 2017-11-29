### Trying with ISO
library(maptools)
library(mapproj)
library(rgeos)
library(rgdal)
library(ggplot2)
library(jsonlite)
library(RCurl)
library(raster)
library(rnaturalearth)
library(dplyr)
library(viridis)

# world <- ne_download(scale = 50, type = "countries")
# world <- world[!world$ISO_A3 %in% c("ATA"),] remove antarctica
world <- spTransform(world, CRS("+proj=wintri"))
map <- fortify(world, region="ISO_N3")
head(map)

# Tune up dataset
raw_with_iso <- read.csv("/Users/rpecchia/Desktop/Impacts Systematic Review/output/raw_data_with_ISO.csv", header = T)
only_isocode_by_case <- select(raw_with_iso, country, isocountrycode)
head(only_isocode_by_case)

cc_iso_by_case <- only_isocode_by_case[complete.cases(only_isocode_by_case),]
head(cc_iso_by_case)

count_of_outage <- count(cc_iso_by_case, isocountrycode) # get count of how many studies per country
outage_df <- as.data.frame(count_of_outage)
head(outage_df)
outage_df

colnames(outage_df) <- c("id", "count") # rename columns to work well with ggplot
outage_df$count <- as.numeric(outage_df$count)

# Custom breaks for data
outage_df$out <- cut(outage_df$count,
                     breaks=c(0, 90, 180, 270, 360, 450, 540, 630, 820),
                     labels=c("0-90", "91-180", "181-270", "271-360",
                              "361-450", "451-540", "541-630",
                              "631-812"))

gg <- ggplot()
gg <- ggplot(data=map, aes(map_id=id))
gg <- gg + geom_map(map=map,
                    aes(x=long, y=lat), color="#0e0e0e", fill="white", size = 0.05)
gg
gg <- gg + geom_map(data=outage_df, map=map, aes(fill=out), colour="#0e0e0e", size=0.05)
gg

gg <- gg + scale_fill_brewer(type="seq", palette="RdPu",
                  name="Number of\ncase studies\nper country")
gg <- gg + labs(title="case studies by country")
gg <- gg + coord_equal(ratio=1)
gg

gg <- gg + ggthemes::theme_map()
gg <- gg + theme(legend.key = element_blank())
gg <- gg + theme(plot.title=element_text(size=16))
gg <- gg + theme(legend.position="right")
gg
