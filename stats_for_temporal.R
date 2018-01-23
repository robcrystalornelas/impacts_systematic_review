# Statistics for temporal

source("~/Desktop/Impacts Systematic Review/scripts/impacts_systematic_review/clean_raw_data.R") # This tells R to run our entire cleaning script so that we have
library(dplyr)
library(plyr)

head(temporal_raw)
temporal_table <- xtabs(~impacttype + studylengthbinned, data=temporal_raw)
temporal_table

# As a chi-squared
chisq.test(temporal_table,correct=F)

# as a log linear model
calc1 <- loglin(temporal_table,margin=list(1,2))

calc2 <- loglin(temporal_table, margin=list(1,2), fit = T, param=T)
calc2
# calc2$fit are the usual expected frequencies
calc2$fit


# Using the GLM function
temporal_and_impact <- select(temporal_raw, impacttype, studylengthbinned)
temporal_and_impact_count <- plyr::count(temporal_and_impact)
head(temporal_and_impact_count)
class(temporal_and_impact_count$studylengthbinned)
class(temporal_and_impact_count$impacttype)
class(temporal_and_impact_count)
 
glm1 <- glm(freq~studylengthbinned*impacttype, family = poisson, data=temporal_and_impact_count)
summary(glm1)
# This equation is for a saturated loglinear model
# This lets us see if impact type is significant, if study length is signficant and 
# if the interaction of impact type and study length are significant

# is this model a good fit?
# First, look at residual deviance in the summary above (really low! that's good)
# then, the p-value in following should be ABOVE .05
pchisq(deviance(glm1), df = df.residual(glm1), lower.tail = F)

# Should we get rid of interaction
drop1(glm1, test = "Chisq")
# second line of results "impacttpye:studylnegthbinned is the model without the interaction
# Since p-value is below .05, we should drop the interaction

# so let's drop that interaction and just look at main effects.
# all main effects are signficant
# This model assumes that study length and impact type are independent of each other
# This is called the independence model
# seems great! all 
glm2 <- glm(freq ~ studylengthbinned + impacttype, data = temporal_and_impact_count, family = poisson)
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

# Check out model residuals
par(mfrow=c(2,2))
plot(glm1)
dev.off()

 
