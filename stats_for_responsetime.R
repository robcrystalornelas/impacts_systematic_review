## LOAD PACKAGES ####
library(tidyverse)
library(ggthemes)

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

glm1 <- glm(freq ~ responsetimebinned*ecosystem, data = counts_of_rt_and_ecosystem, family = poisson())
summary(glm1)

# This lets us see if impact type is significant, if study length is signficant and 
# if the interaction of impact type and study length are significant
# is this model a good fit?
# First, look at residual deviance in the summary above (really low! that's good)
# then, the p-value in following should be ABOVE .05

# Should we get rid of interaction
drop1(glm1, test = "Chisq")
# second line of results "impacttpye:studylnegthbinned is the model without the interaction
# Since p-value is below .05, we should drop the interaction

# so let's drop that interaction and just look at main effects.
# all main effects are signficant
# This model assumes that study length and impact type are independent of each other
# This is called the independence model
# seems great! all 
glm2 <- glm(freq ~ responsetimebinned + ecosystem, data = counts_of_rt_and_ecosystem, family = poisson())
summary(glm2) # but the residual deviance is high, so lets look at the residuals.
# high residuals might mean poor model fit
# Rule of thumb is that residual deviance should be close to degrees of freedom

# we can used chisq test to see if expected freuencies satisfy the simpler model
# a low p-value here indicates that they do not!
pchisq(deviance(glm2), df = df.residual(glm2), lower.tail = F)
# Taken together, we should feel confident going with the more complex model

# glm1 is fully saturated and really complex. glm2 is really simple, maybe overly simple
# check an anova to see if one model is better than the other
# if p is above .05 then both models fit equally...go with the simple
anova(glm1, glm2)
# then take the deviance and put it into chi square
pchisq(54.013, df = 21, lower.tail = F) # actually the two models are really different!
