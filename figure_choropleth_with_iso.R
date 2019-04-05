## READ IN DATA ####
raw_with_iso <- read.csv("/Users/rpecchia/Desktop/Impacts Systematic Review/output/raw_data_with_ISO_3.csv", header = T)

## LOAD PACKAGES ####
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
library(withr)
library(stringr)

## ORGANIZE DATA ####
#world <- ne_download(scale = 50, type = "countries")
world <- world[!world$ISO_A3 %in% c("ATA"),] # remove antarctica
world <- spTransform(world, CRS("+proj=wintri"))
map <- fortify(world, region="ISO_N3")
head(map)

# Tune up dataset
only_isocode_by_case <- dplyr::select(raw_with_iso, country, isocountrycode)
head(only_isocode_by_case)
length(unique(only_isocode_by_case$country))
cc_iso_by_case <- only_isocode_by_case[complete.cases(only_isocode_by_case),]
head(cc_iso_by_case)

# Make sure ISO codes with two digits have leading zero
new_iso_list <- as.character(cc_iso_by_case$isocountrycode)
new_iso_list <- str_pad(new_iso_list, 3, pad = "0")
head(new_iso_list)
#new_iso_list <- as.integer(new_iso_list)
# head(new_iso_list)

####
cc_iso_by_case$isocountrycode <- new_iso_list
head(cc_iso_by_case)

count_of_outage <- dplyr::count(cc_iso_by_case, isocountrycode) # get count of how many studies per country
outage_df <- as.data.frame(count_of_outage)
head(outage_df)
outage_df
sum(outage_df$n)
colnames(outage_df) <- c("id", "count") # rename columns to work well with ggplot
outage_df$count <- as.numeric(outage_df$count)

head(outage_df)
countries_ordered <- arrange(outage_df, desc(count))
head(countries_ordered)
countries_ordered
dim(countries_ordered)
# Custom breaks for data
outage_df$out <- cut(outage_df$count,
                     breaks=c(0, 90, 180, 270, 360, 450, 540, 630, 1000),
                     labels=c("0-90", "91-180", "181-270", "271-360",
                              "361-450", "451-540", "541-630",
                              "631-1000"))
outage_df

outage_df$out_small_breaks <- cut(outage_df$count,
                                  breaks=c(0, 20, 40, 60, 80, 100, 120, 140, 1000),
                                  labels=c("1-20", "21-40", "41-60", "61-80",
                                           "81-100", "101-120", "121-140",
                                           "121+"))
outage_df

outage_df$out_smallest_break <- cut(outage_df$count,
                                  breaks=c(0, 20, 40, 60, 80, 100, 200, 750, 1000),
                                  labels=c("1-20", "21-40", "41-60", "61-80",
                                           "81-100", "101-200","201-750",
                                           "751+"))
outage_df

## MAKE FIGURES ####
gg <- ggplot()
gg <- ggplot(data=map, aes(map_id=id))
gg <- gg + geom_map(map=map, aes(x=long, y=lat), color="#0e0e0e", fill="#ffffff", size = 0.05)
gg <- gg + geom_map(data=outage_df, map=map, aes(fill=out_smallest_break), 
                    colour="#0e0e0e", size=0.05)
gg # add in our data with random color scheme
gg <- gg + scale_fill_brewer(type="seq", palette="RdPu",name="Number of\ncase studies\nper country") # Better color theme
gg <- gg + coord_equal(ratio=1) # flatten out map
gg
gg <- gg + ggthemes::theme_map() # more sparse theme
gg <- gg + theme(legend.key = element_blank()) # move legend
gg
gg <- gg + theme(plot.title=element_text(size=40))
gg <- gg + theme(legend.position="right")
gg

pdf(file="~/Desktop/Impacts Systematic Review/figures/figure_map_of_case_studies.pdf")
gg
dev.off()
dev.off()
