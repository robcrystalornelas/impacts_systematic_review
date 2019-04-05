## LOAD PACKAGES ####

## READ IN DATA ####
crystal_vs_seebens <- read.csv("~/Desktop/Impacts Systematic Review/scripts/other_data_to_use/mann_whitney_vs_seebens.csv",head = T) # This tells R to run our entire cleaning script so that we have
head(crystal_vs_seebens)

boxplot(proportion~paper, data = crystal_vs_seebens)

wilcox.test(proportion~paper, mu = 0, alt="two.sided", correct = TRUE, paired = FALSE, conf.int = TRUE, data = crystal_vs_seebens)


crystal_vs_turbelin <- read.csv("~/Desktop/Impacts Systematic Review/scripts/other_data_to_use/mann_whitney_vs_turbelin.csv",head = T) # This tells R to run our entire cleaning script so that we have
head(crystal_vs_turbelin)
wilcox.test(proportion~paper, mu = 0, alt="two.sided", correct = TRUE, paired = FALSE, conf.int = TRUE, data = crystal_vs_turbelin)

crystal_vs_nentwig <- read.csv("~/Desktop/Impacts Systematic Review/scripts/other_data_to_use/mann_whitney_vs_nentwig.csv",head = T) # This tells R to run our entire cleaning script so that we have
crystal_vs_nentwig
wilcox.test(proportion~paper, mu = 0, alt="two.sided", correct = TRUE, paired = FALSE, conf.int = TRUE, data = crystal_vs_nentwig)

wilcox.test(proportion~paper, data=crystal_vs_nentwig)
