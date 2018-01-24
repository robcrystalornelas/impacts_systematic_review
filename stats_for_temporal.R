## LOAD PACKAGES ####
library(tidyverse)
library(ggthemes)

## READ IN DATA ####
source("~/Desktop/Impacts Systematic Review/scripts/impacts_systematic_review/clean_raw_data.R") # This tells R to run our entire cleaning script so that we have

## CLEAN DATA ####
head(temporal_raw)
# General chi-square for just strayer figure.  
counts_of_strayer <- count(temporal_raw$studylengthbinned)
counts_of_strayer
full_chi <- chisq.test(counts_of_strayer$freq)
full_chi
full_chi$expected

# loglinear modeling Using the GLM function
temporal_and_impact <- select(temporal_raw, impacttype, studylengthbinned)
temporal_and_impact_count <- plyr::count(temporal_and_impact)
head(temporal_and_impact_count)

# This is from tutorial using loglm()
temporal_table = xtabs(freq ~ studylengthbinned + impacttype,
              data=temporal_and_impact_count)
temporal_table

# Here's the test of mutual independence of the two varaibles of concern
loglm_for_temp_table <- loglm( ~ studylengthbinned*impacttype,
       temporal_table)
loglm_for_temp_table

# This equation is for a saturated loglinear model
temporal_and_impact_count
glm1 <- glm(freq ~ studylengthbinned*impacttype, data = temporal_and_impact_count, family = poisson())
summary(glm1)
glm1$xlevels
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
glm2 <- glm(freq ~ studylengthbinned + impacttype, data = temporal_and_impact_count, family = poisson())
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
pchisq(99.4, df = 1, lower.tail = F) # actually the two models are really different!