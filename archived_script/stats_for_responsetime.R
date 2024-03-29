## LOAD PACKAGES ####
library(plyr)
library(dplyr)
library(ggthemes)
library(broom)
library(data.table)
## READ IN DATA ####
source("~/Desktop/Impacts Systematic Review/scripts/impacts_systematic_review/clean_raw_data.R") # This tells R to run our entire cleaning script so that we have

## CLEAN DATA ####
response_time_data <- dplyr::select(raw_data, publicationyear, ecosystem,invasivespeciestaxa,firstyeardetected,firstyearatsite,yearbegins,yearends,yearbinned)
head(response_time_data)

# Create new column to show response time (yearbegins - firstyeardetected)
# first, make all columns numerical
response_time_data$firstyeardetected <- as.character(response_time_data$firstyeardetected)
response_time_data$firstyeardetected <- as.numeric(response_time_data$firstyeardetected)

response_time_data$yearbegins <- as.character(response_time_data$yearbegins)
response_time_data$yearbegins <- as.numeric(response_time_data$yearbegins)

# now add new column with response time variable
response_time_data <- mutate(response_time_data, responsetime = yearbegins - firstyeardetected)
head(response_time_data)

# bin response time data
response_time_data$responsetimebinned <- cut(response_time_data$responsetime, breaks = c(0,1,5,10,500), labels = c("Rapid (0-1 years)","1.1-5 years","5.1-10 years","Slow (10.1+)"))
response_time_data

# NAs, so remove those
response_time_data_cc <- dplyr::select(response_time_data, ecosystem,publicationyear,responsetimebinned)
head(response_time_data_cc)
response_time_data_cc <- response_time_data_cc[complete.cases(response_time_data_cc$responsetimebinned), ]
head(response_time_data_cc)
dim(response_time_data_cc)

## STATISTICAL TESTS ####
# General chi-sqaured for response time
counts_of_rt <- count(response_time_data_cc$responsetimebinned)
counts_of_rt
full_chi <- chisq.test(counts_of_rt$freq)
full_chi
full_chi$expected

counts_of_rt_and_ecosystem <- dplyr::select(response_time_data_cc, ecosystem, responsetimebinned) %>%
plyr::count()
counts_of_rt_and_ecosystem

# glm1 <- glm(freq ~ responsetimebinned*ecosystem, data = counts_of_rt_and_ecosystem, family = poisson())
# summary(glm1)
# tidy_glm_response_time <- tidy(glm1)
# tidy_glm_response_time
# write.csv(tidy_glm_response_time, "/Users/rpecchia/Desktop/Impacts Systematic Review/output/coefficients_for_response_time.csv")
# 
# # This lets us see if impact type is significant, if study length is signficant and 
# # if the interaction of impact type and study length are significant
# # is this model a good fit?
# # First, look at residual deviance in the summary above (really low! that's good)
# # then, the p-value in following should be ABOVE .05
# 
# # Should we get rid of interaction
# drop1(glm1, test = "Chisq")
# # second line of results "impacttpye:studylnegthbinned is the model without the interaction
# # Since p-value is below .05, we should drop the interaction
# 
# # so let's drop that interaction and just look at main effects.
# # all main effects are signficant
# # This model assumes that study length and impact type are independent of each other
# # This is called the independence model
# # seems great! all 
# glm2 <- glm(freq ~ responsetimebinned + ecosystem, data = counts_of_rt_and_ecosystem, family = poisson())
# summary(glm2) # but the residual deviance is high, so lets look at the residuals.
# # high residuals might mean poor model fit
# # Rule of thumb is that residual deviance should be close to degrees of freedom
# 
# # we can used chisq test to see if expected freuencies satisfy the simpler model
# # a low p-value here indicates that they do not!
# pchisq(deviance(glm2), df = df.residual(glm2), lower.tail = F)
# # Taken together, we should feel confident going with the more complex model
# 
# # glm1 is fully saturated and really complex. glm2 is really simple, maybe overly simple
# # check an anova to see if one model is better than the other
# # if p is above .05 then both models fit equally...go with the simple
# anova(glm1, glm2)
# # then take the deviance and put it into chi square
# pchisq(54.013, df = 21, lower.tail = F) # actually the two models are really different!
# 
# 
#### 
## Now do the stats using proportions
head(response_time_data_cc)
response_time_data
dim(response_time_data_cc)
head(response_time_data_cc)
response_time_data_cc$ecosystem
counts_of_rt <- plyr::count(response_time_data_cc$responsetimebinned)
head(counts_of_rt)
setDT(counts_of_rt)[, Prop := freq/sum(freq)]
prop_of_rt <- counts_of_rt$Prop
prop_of_rt

rt_and_ecosystem <- dplyr::select(response_time_data_cc,ecosystem,responsetimebinned)
counts_of_rt_and_ecosystem <- plyr::count(rt_and_ecosystem)
table_rt_and_ecosystem <- tbl_df(counts_of_rt_and_ecosystem)

unique(raw_data$ecosystem)
subset_of_forest <- filter(table_rt_and_ecosystem, ecosystem == "forest")
subset_of_forest
observed_freq_forest <- subset_of_forest$freq

subset_of_lotic <- filter(table_rt_and_ecosystem, ecosystem == "lotic")
subset_of_lotic
observed_freq_lotic <- subset_of_lotic$freq
observed_freq_lotic

subset_of_grass <- filter(table_rt_and_ecosystem, ecosystem == "grassland")
subset_of_grass
observed_freq_grass <- c(1,0,2,66)
observed_freq_grass

subset_of_estuarine <- filter(table_rt_and_ecosystem, ecosystem == "estuarine")
subset_of_estuarine
observed_freq_estuarine <- c(0,4,11,84)

subset_of_urban <- filter(table_rt_and_ecosystem, ecosystem == "urban")
subset_of_urban
observed_freq_urban <- c(2,0,0,30)

subset_of_island <- filter(table_rt_and_ecosystem, ecosystem == "island")
subset_of_island
observed_freq_island <- c(0,4,5,115)

subset_of_mountain <- filter(table_rt_and_ecosystem, ecosystem == "mountain")
subset_of_mountain
observed_freq_mountain <- c(0,0,0,9)

subset_of_multiple <- filter(table_rt_and_ecosystem, ecosystem == "multiple")
subset_of_multiple
observed_freq_multiple <- c(0,0,1,21)

subset_of_lentic <- filter(table_rt_and_ecosystem, ecosystem == "lentic")
subset_of_lentic
observed_freq_lentic <- subset_of_lentic$freq

subset_of_intertidal <- filter(table_rt_and_ecosystem, ecosystem == "intertidal")
subset_of_intertidal
observed_freq_intertidal <- c(0,6,4,49)

subset_of_desert <- filter(table_rt_and_ecosystem, ecosystem == "desert")
subset_of_desert
observed_freq_desert <- c(0,0,0,6)

subset_of_coastal <- filter(table_rt_and_ecosystem, ecosystem == "coastal")
subset_of_coastal
observed_freq_coastal <- c(0,2,3,25)

subset_of_shrubland <- filter(table_rt_and_ecosystem, ecosystem == "shrubland")
subset_of_shrubland
observed_freq_shrubland <- c(0,2,0,25)

subset_of_oceanic <- filter(table_rt_and_ecosystem, ecosystem == "oceanic")
subset_of_oceanic
observed_freq_oceanic <- c(0,10,11,25)

## subset_of_lotic <- filter(table_rt_and_ecosystem, ecosystem == "NA")

# Chi-sqaured tests
chisq.test(x = observed_freq_forest, p = prop_of_rt, simulate.p.value = TRUE)
chisq.test(x = observed_freq_lotic, p = prop_of_rt,simulate.p.value = TRUE)
chisq.test(x = observed_freq_grass, p = prop_of_rt, simulate.p.value = TRUE)
chisq.test(x = observed_freq_estuarine, p = prop_of_rt, simulate.p.value = TRUE)
chi_urban <- chisq.test(x = observed_freq_urban, p = prop_of_rt, simulate.p.value = TRUE)
chi_urban
chi_urban$expected
chi_urban$observed
chi_island <- chisq.test(x = observed_freq_island, p = prop_of_rt, simulate.p.value = TRUE)
chi_island
chi_island$observed
chi_island$expected
chisq.test(x = observed_freq_mountain, p = prop_of_rt, simulate.p.value = TRUE)
chisq.test(x = observed_freq_multiple, p = prop_of_rt, simulate.p.value = FALSE)
chisq.test(x = observed_freq_lentic, p = prop_of_rt, simulate.p.value = TRUE)
chisq.test(x = observed_freq_intertidal, p = prop_of_rt, simulate.p.value = TRUE)
chisq.test(x = observed_freq_desert, p = prop_of_rt, simulate.p.value = TRUE)
chisq.test(x = observed_freq_coastal, p = prop_of_rt, simulate.p.value = TRUE)
chisq.test(x = observed_freq_shrubland, p = prop_of_rt, simulate.p.value = TRUE)
ocean_chi <- chisq.test(x = observed_freq_oceanic, p = prop_of_rt, simulate.p.value = TRUE)
ocean_chi
ocean_chi$observed
ocean_chi$expected
?chisq.test
