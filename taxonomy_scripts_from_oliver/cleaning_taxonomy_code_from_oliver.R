
rm(list = ls())

library(taxize)
library(magrittr)
library(dplyr)
library(utils)

# load data
raw_data <- read.csv("~/Desktop/Impacts Systematic Review/Crystal-Ornelas_et_al_SR_v12.csv", header=TRUE)

# get unique sp
sp_names = unique(raw_data$latinname)
dim(sp_names)
length(sp_names)
### Grab rank and status from col ---

# load function
setwd("/Users/rpecchia/Desktop/Impacts Systematic Review/scripts/taxonomy_clean/")
source("col_check_fun.R")

# run fun by 1000s in case it stops working
df2a = col_check(sp_names[1:670], sp_names[1:670])
dim(df2a)
tail(df2a)

# Don't need these because I only have 600+ species
# df2b = col_check(sp_names[1001:2000], sp_names[1001:2000])
# dim(df2b)
# df2c = col_check(sp_names[2001:length(sp_names)], sp_names[2001:length(sp_names)])

# combine all data.frames for returned species names
# df2_all = rbind.data.frame(df2a, df2b, df2c,
#                            stringsAsFactors = FALSE)

# # save so i dont have to run them again
setwd("/Users/rpecchia/Desktop/Impacts Systematic Review/scripts/taxonomy_clean/")
saveRDS(df2a, "col_check_latinname.rds")
df2_all = readRDS("col_check_latinname.rds")


#### go through synonyms to replace them with accepted name ----

# load fun
setwd("/Users/rpecchia/Desktop/Impacts Systematic Review/scripts/taxonomy_clean/")
source("col_syn_fun.R")

# run fun
df3 = col_syn(df2_all)

# save so i dont have to run them again
setwd("/Users/rpecchia/Desktop/Impacts Systematic Review/scripts/taxonomy_clean/")
saveRDS(df3, "col_syn_latinname.rds")
df3 = readRDS("col_syn_latinname.rds")


#### Manually look through and replace unmatched names ----
df4 = df3
df4

# Write out the .csv file to compare accepted and not-found names
write.csv(df4, file = "latin_name_COL_name_matches.csv")

# run subscript
source("/Users/rpecchia/Desktop/Impacts Systematic Review/scripts/taxonomy_clean/lemis_sp_resolve_replace_unmatched.R")

# save so i dont have to run them again
setwd("/Users/rpecchia/Desktop/Impacts Systematic Review/scripts/taxonomy_clean/")
# saveRDS(df4, "col_match_unmatched.rds")
df4 = readRDS("col_match_unmatched.rds")


#### clean infraspecies synoynms ----
## aka those that are labeled infraspecies but don't have subspecies in name
df5 = df4

# run subscript
source("C:/Users/oliver/Google Drive/PhD/Research/Pet trade/herp_trade_dynamics/scripts/lemis_clean/lemis_sp_resolve_sub_scripts/lemis_sp_resolve_infraspecies.R")


# save so i dont have to run them again
setwd("C:/Users/oliver/Google Drive/PhD/Research/Pet trade/herp_trade_dynamics/data_files/lemis_clean/taxonomy_clean/")
# saveRDS(df5, "col_clean_infra_no_subsp.rds")
df5 = readRDS("col_clean_infra_no_subsp.rds")



#### grab sp/subsp name and id from COL ----

# load fun
setwd("C:/Users/oliver/Google Drive/PhD/Research/Pet trade/Ch1/Data/pet_web_scrapers/scripts/taxonomy_clean/")
source("col_sp_name_id_fun.R")

df6 = df5

# run fun
df6 = col_sp_name_id(df6$vendor_name, df6$clean_1_name, df6$col_rank)


# save so i dont have to run them again
setwd("C:/Users/oliver/Google Drive/PhD/Research/Pet trade/herp_trade_dynamics/data_files/lemis_clean/taxonomy_clean/")
# saveRDS(df6, "col_sp_id_grab.rds")
df6 = readRDS("col_sp_id_grab.rds")

#### minor edits ----

# remove NA, should have done this up top, oh well doesn't affect anything
df6 = df6[ !(is.na(df6$vendor_name)) , ]

# filter out not in CoL
temp = df6[ !(is.na(df6$col_sp)) , ]

# get species in CoL
df6_in_CoL = temp[ (temp$col_sp != "not in COL") , ]

### get species not in CoL
df6_not_in_CoL = temp[ (temp$col_sp == "not in COL") , ]

## get unresolvable and remove from df
# get unresolvable
unresolveable = df6_not_in_CoL[ (df6_not_in_CoL$vendor_name %in% un_edit$unmatched3[ un_edit$unresolvable == 1]) ,  ]

# remove from df
df6_not_in_CoL = df6_not_in_CoL[ !(df6_not_in_CoL$vendor_name %in% unresolveable$vendor_name) , ]

## get NAs which are all genus only
df6_nas = df6[ (is.na(df6$col_sp)) , ]
# append unresolvable
df6_nas = rbind.data.frame(df6_nas, unresolveable, stringsAsFactors = F)


#### get upstream taxonomy ----

# load fun
setwd("C:/Users/oliver/Google Drive/PhD/Research/Pet trade/Ch1/Data/pet_web_scrapers/scripts/taxonomy_clean/")
source("get_upstream_taxonomy_fun.R") 

 
# # natrix natrix causes code to break :(
# classification(x = "Natrix natrix", db = "col", row = 1)


# get indices without natrix natrix
df6_in_CoL_sub = df6_in_CoL[ df6_in_CoL$col_sp != "Natrix natrix"  ,]

# run fun
df7 = get_upstream_taxonomy(df6_in_CoL_sub)


# save so i dont have to run them again
setwd("C:/Users/oliver/Google Drive/PhD/Research/Pet trade/herp_trade_dynamics/data_files/lemis_clean/taxonomy_clean/")
# saveRDS(df7, "col_upstream.rds")
df7 = readRDS("col_upstream.rds")



#### replace ones not in COL  ----
 
df6_not_in_CoL


# create custom ids for them
setwd("C:/Users/oliver/Google Drive/PhD/Research/Pet trade/herp_trade_dynamics/data_files/lemis_clean/taxonomy_clean/")
write.csv(df6_not_in_CoL, "not_in_CoL_custom_ids.csv")

# loaded edited csv
setwd("C:/Users/oliver/Google Drive/PhD/Research/Pet trade/herp_trade_dynamics/data_files/lemis_clean/taxonomy_clean/")
cust_sp = read.csv("not_in_CoL_custom_ids_EDIT.csv", strip.white = T)


## join to df7
df7 = plyr::rbind.fill(df7, cust_sp)


### Ensure that all market customs are accounted for in lemis ----

# load market sp key to make sure sp line up
setwd("C:/Users/oliver/Google Drive/PhD/Research/Pet trade/Ch1/Data/pet_web_scrapers/data_files/wbm/compiled/master_sp_key/")
market_key = readRDS("master_sp_key_with_taxonomy.rds")


## there are 2 discrepancies between market and lemis, fix here

# 1. in market data, Bufo regularis (lemis) = Sclerophrys regularis (market), not what was matched by col (Amietophrynus regularis)
df7$col_sp_id[ df7$vendor_name == "Bufo regularis" ] = "os_scre1"
df7$col_genus[ df7$vendor_name == "Bufo regularis" ] = "Sclerophrys"
df7$col_species[ df7$vendor_name == "Bufo regularis" ] = "Sclerophrys regularis"
df7$col_sp[ df7$vendor_name == "Bufo regularis" ] = "Sclerophrys regularis"

# 2. in market data, Bufo viridis (lemis) = Bufotes viridis (market), not what was matched by col (Pseudepidalea viridis)
df7$col_sp_id[ df7$vendor_name == "Bufo viridis" ] = "os_buvi1"
df7$col_genus[ df7$vendor_name == "Bufo viridis" ] = "Bufotes"
df7$col_species[ df7$vendor_name == "Bufo viridis" ] = "Bufotes viridis"
df7$col_sp[ df7$vendor_name == "Bufo viridis" ] = "Bufotes viridis"


#### add back in natrix natrix ----

colnames(df7)

natrix = data.frame(t( c("Natrix natrix", "Natrix natrix", "27ec70b00734b68213ff0972f7360650",
                      NA, NA, "Reptilia", "Squamata", "Natricidae", "Natrix", "Natrix natrix") ))
colnames(natrix) = colnames(df7)

# join to df
df7 = plyr::rbind.fill(df7, natrix)



#### tidy up ----
## remove duplicates - there are none
dplyr::distinct(df7, vendor_name, col_sp) %>% nrow() == nrow(df7)


## remove NAs - there are none
df7[ df7$vendor_name %>% is.na() ]
df7[ df7$col_sp %>% is.na() ]


# #### merge back into original df ----
# df_merge = dplyr::left_join(allen, df7, by = c('Species' = 'vendor_name'))


 
#### save ----
# setwd("C:/Users/oliver/Google Drive/PhD/Research/Pet trade/Ch1/Data/herp_databases/data_files/resolve_taxonomy/allen")
# saveRDS(df_merge, "allen_sp_resolved.rds")
# 
# df_merge = readRDS("allen_sp_resolved.rds")


# save key
setwd("C:/Users/oliver/Google Drive/PhD/Research/Pet trade/herp_trade_dynamics/data_files/lemis_clean/")
# saveRDS(df7, "lemis_col_key.rds")
lemis_col_key = readRDS("lemis_col_key.rds")

