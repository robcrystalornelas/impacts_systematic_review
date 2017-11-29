# Use the taxize package to look up official species names

library(taxize)
library(plyr)

raw_data <- read.csv("~/Desktop/Impacts Systematic Review/Crystal-Ornelas_et_al_SR_v12.csv", header=TRUE)

# get unique sp
sp_names <- unique(raw_data$latinname)

# df_of_common_names = get_uid(sp_names)
head(df_of_common_names)

# keep only uids which you have in the database
uids.found <- as.uid(df_of_common_names[!is.na(df_of_common_names)])

# keep only species names  corresponding to your ids
species.found <- sp_names[!is.na(df_of_common_names)]

?sci2comm
# common.names <- sci2comm(uids.found, db = 'ncbi')
names(common.names) <- species.found
str(common.names)
class(common.names)

common.names.df <- do.call("rbind", lapply(common.names, as.data.frame)) 
head(common.names.df)
write.csv(common.names.df, file="common_names_looked_up.csv")
